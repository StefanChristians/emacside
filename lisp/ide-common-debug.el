;;; ide-common-debug.el --- debugger management -*- lexical-binding: t; -*-

;; Version: 0.0.1

;; Copyright (C) 2025 Stefan Christians
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Manage DAP debuggers and adapters and launch debugging sessions
;;
;; Each language registers its own debuggers and DAP adapters.
;; The last used adapter for debugging an executable is cached.

;;; Code:

(require 'dap-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; adapters

;; ("name" . (:binary "binary" :type "type" :description "description")
(defvar ide-common-debug-adapters nil
  "Map of debuggers to DAP adapters.")

(defun ide-common-debug-register-adapter (name binary type package description)
  "Register a debugger and associate it with an adapter.

NAME — human-facing name of the debugger.
BINARY — name of the debugger binary (string).
TYPE — DAP adapter type string used by `dap-mode'.
PACKAGE — feature symbol provided by the adapter package (e.g. \\='dap-gdb).
DESCRIPTION — human-facing description.

The adapter will only be registered if both:
  - the debugger BINARY is installed on the system, and
  - the adapter PACKAGE has already been loaded."
  (if (and (executable-find binary)
           (featurep package))
      (progn
        (setf (alist-get name ide-common-debug-adapters nil nil #'string=)
              (list :binary binary :type type :description description))
        (message "registering %s" description))
    (message "skipping %s" description)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; persistent cache

;; last debugger cache
(defvar ide-common-debug-last-debugger '()
  "Alist mapping PROJECT-ROOT → (EXECUTABLE . DEBUGGER-NAME).
Each PROJECT-ROOT entry holds an alist of executables to their last used
debugger name.")

(defvar ide-common-debug-last-debugger-file
  (f-join user-emacs-directory ".cache" "ide-common-debug-last-debugger.eld")
  "Path to file storing last debugger per executable per project.")

;; last debugged executable cache
(defvar ide-common-debug-last-executable '()
  "Alist mapping PROJECT-ROOT → last debugged EXECUTABLE.")

(defvar ide-common-debug-last-executable-file
  (f-join user-emacs-directory ".cache" "ide-common-debug-last-executable.eld")
  "Path to file storing last debugged executable per project.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cache maintenance

(defun ide-common-debug-load-cache ()
  "Load all debug caches from disk."
  (setq ide-common-debug-last-debugger
        (or (ide-common-read-file ide-common-debug-last-debugger-file) '()))
  (setq ide-common-debug-last-executable
        (or (ide-common-read-file ide-common-debug-last-executable-file) '())))

(defun ide-common-debug-save-last-debugger-cache ()
  "Save last used debugger cache to disk."
  (ide-common-write-file ide-common-debug-last-debugger-file
                         ide-common-debug-last-debugger))

(defun ide-common-debug-save-last-executable-cache ()
  "Save last debugged executable cache to disk."
  (ide-common-write-file ide-common-debug-last-executable-file
                         ide-common-debug-last-executable))

(defun ide-common-debug-save-cache ()
  "Save all debug caches to disk."
  (ide-common-debug-save-last-debugger-cache)
  (ide-common-debug-save-last-executable-cache))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; last debugger getters and setters

(defun ide-common-debug-get-last-table (project-root)
  "Return alist of executables for PROJECT-ROOT, creating it if needed."
  (let ((entry (assoc project-root ide-common-debug-last-debugger)))
    (if entry
        (cdr entry)
      (let ((tbl '()))
        (push (cons project-root tbl) ide-common-debug-last-debugger)
        tbl))))

(defun ide-common-debug-set-last-table (project-root table)
  "Replace executable TABLE for PROJECT-ROOT in cache."
  (setq ide-common-debug-last-debugger
        (cons (cons project-root table)
              (assoc-delete-all project-root ide-common-debug-last-debugger)))
  (ide-common-debug-save-last-debugger-cache))

(defun ide-common-debug-get-last-debugger (project-root executable)
  "Return last used debugger name for EXECUTABLE under PROJECT-ROOT.

If the stored name does not exist in `ide-common-debug-adapters',
return nil instead."
  (let* ((exe-table (ide-common-debug-get-last-table project-root))
         (name (cdr (assoc executable exe-table))))
    (when (and name
               (not (assoc name ide-common-debug-adapters)))
      ;; Stored name no longer valid → unset it
      (ide-common-debug-unset-debugger-for-executable project-root executable)
      (setq name nil))
    name))

(defun ide-common-debug-get-last-binary (project-root executable)
  "Return binary of last used debugger for EXECUTABLE under PROJECT-ROOT."
  (let* ((name (ide-common-debug-get-last-debugger project-root executable))
         (entry (assoc name ide-common-debug-adapters)))
    (when entry
      (plist-get (cdr entry) :binary))))

(defun ide-common-debug-get-last-type (project-root executable)
  "Return type of last used debugger for EXECUTABLE under PROJECT-ROOT."
  (let* ((name (ide-common-debug-get-last-debugger project-root executable))
         (entry (assoc name ide-common-debug-adapters)))
    (when entry
      (plist-get (cdr entry) :type))))

(defun ide-common-debug-set-last-debugger (project-root executable name)
  "Set last used debugger NAME for EXECUTABLE under PROJECT-ROOT."
  (let* ((exe-table (ide-common-debug-get-last-table project-root))
         (new-table (cons (cons executable name)
                          (assoc-delete-all executable exe-table))))
    (ide-common-debug-set-last-table project-root new-table)))

(defun ide-common-debug-unset-debugger-for-executable (project-root executable)
  "Remove last debugger for EXECUTABLE in PROJECT-ROOT."
  (let* ((exe-table (ide-common-debug-get-last-table project-root))
         (new-table (assoc-delete-all executable exe-table)))
    (ide-common-debug-set-last-table project-root new-table)))

(defun ide-common-debug-unset-all-debuggers (project-root)
  "Delete all last debuggers for all executables in PROJECT-ROOT."
  (setq ide-common-debug-last-debugger
        (assoc-delete-all project-root ide-common-debug-last-debugger))
  (ide-common-debug-save-last-debugger-cache))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; last executable getters and setters

(defun ide-common-debug-get-last-executable (project-root)
  "Return last debugged executable for PROJECT-ROOT, or nil if none."
  (cdr (assoc project-root ide-common-debug-last-executable)))

(defun ide-common-debug-set-last-executable (project-root executable)
  "Set last debugged EXECUTABLE for PROJECT-ROOT."
  (setq ide-common-debug-last-executable
        (cons (cons project-root executable)
              (assoc-delete-all project-root ide-common-debug-last-executable)))
  (ide-common-debug-save-last-executable-cache))

(defun ide-common-debug-unset-last-executable (project-root)
  "Remove cached last debugged executable for PROJECT-ROOT."
  (setq ide-common-debug-last-executable
        (assoc-delete-all project-root ide-common-debug-last-executable))
  (ide-common-debug-save-last-executable-cache))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; action

(defun ide-common-debug-select-debugger (executable &optional project-root)
  "Select a debugger for EXECUTABLE in PROJECT-ROOT."
  (ide-common-debug-load-cache)
  (if (null ide-common-debug-adapters)
      (error "No debuggers available")
    (let* ((root (or project-root
                     (ide-common-get-current-context-project-root)))
           (executable (ide-common-relative-path-if-descendant executable root))
           (last (ide-common-debug-get-last-debugger root executable))
           (names (mapcar #'car ide-common-debug-adapters))
           ;; Annotation function
           (annot-fn (lambda (cand)
                       (let ((desc (plist-get (alist-get cand ide-common-debug-adapters nil nil #'string=) :description)))
                         (when desc (format " — %s" desc))))))
      ;; Prompt user
      (let* ((completion-extra-properties `(:annotation-function ,annot-fn))
             (choice (completing-read
                      (format "Select debugger for %s: " executable)
                      names nil t nil nil last)))
        ;; store selection as last used debugger
        (ide-common-debug-set-last-debugger root executable choice)
        choice))))

(defun ide-common-debug-select-executable (&optional project-root)
  "Prompt the user to select a file to debug, starting from PROJECT-ROOT.
Caches the selection as the last debugged executable for this project."
  (ide-common-debug-load-cache)
  (let* ((root (or project-root
                     (ide-common-get-current-context-project-root)))
         (default-file (ide-common-debug-get-last-executable root))
         (prompt (if default-file
                     (format "Select executable (default %s): " default-file)
                   "Select executable: "))
         (file (expand-file-name (read-file-name prompt root nil t nil) root))
         (relative-file (when file
                          (ide-common-relative-path-if-descendant file root))))
    ;; Cache the last selected executable
    (when relative-file
      (ide-common-debug-set-last-executable root relative-file))
    relative-file))

(defun ide-common-debug-run-debugger (&optional prefix project-root)
  "Run debugger in PROJECT-ROOT.

If PREFIX is given (usually `C-u`), prompt for configuration.
Otherwise reuse last configuration."
  (interactive "P")
  (let* ((root (or project-root
                  (ide-common-get-current-context-project-root)))
         (executable (or (and prefix
                              (ide-common-debug-select-executable root))
                         (ide-common-debug-get-last-executable root)
                         (ide-common-debug-select-executable root)))
         ;; if executable is relative, append to root
         ;; if executable is absolute, use as is
         (program (f-join root executable))
         (debugger (or (and prefix
                            (ide-common-debug-select-debugger executable root))
                       (ide-common-debug-get-last-debugger root executable)
                       (ide-common-debug-select-debugger executable root)))
         (type (ide-common-debug-get-last-type root executable))
         (binary (ide-common-debug-get-last-binary root executable))
         (command (concat binary " " executable))
         (request "launch")
         (cwd root)
         (env (progn
                (when prefix (ide-common-env-select-and-edit root))
                (ide-common-env-get-profile
                 root
                 (ide-common-env-get-last-profile root))))
         (args (progn
                 (when prefix (ide-common-args-select-and-edit command root))
                 (ide-common-args-get-profile
                  root
                  command
                  (ide-common-args-get-last-profile root command)))))
    (dap-debug
     (list :type type
           :request request
           :name command
           :program program
           :args args
           :cwd cwd
           :environment env
           :stopOnEntry t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DAP wrappers

(defun ide-common-debug-disconnect-all-sessions ()
  "Disconnect all active DAP sessions."
  (interactive)
  (dolist (session (ignore-errors (dap--get-sessions)))
    (when (dap--session-running session)
      (dap-disconnect session))))

(defun ide-common-debug-wait-and-delete-session (session &optional interval)
  "Wait until SESSION is no longer running, then delete it.
INTERVAL is the polling time in seconds (default 0.1)."
  (let ((interval (or interval 0.1)))
    (if (not (dap--session-running session))
        (dap-delete-session session)
      ;; Session still running → check again later
      (run-at-time interval nil
                   (lambda ()
                     (ide-common-debug-wait-and-delete-session session interval))))))

(defun ide-common-debug-disconnect-and-delete-session (&optional session)
  "Safely disconnect and delete a DAP SESSION.
If SESSION is nil, use the current session."
  (interactive)
  (let ((session (or session (dap--cur-session-or-die))))
    (if (dap--session-running session)
        (progn
          (dap-disconnect session)
          (ide-common-debug-wait-and-delete-session session))
      (dap-delete-session session))))

(defun ide-common-debug-disconnect-and-delete-all-sessions ()
  "Safely disconnect and delete all DAP sessions."
  (interactive)
  (dolist (session (ignore-errors (dap--get-sessions)))
    (ide-common-debug-disconnect-and-delete-session session)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; key bindings

(pretty-hydra-define hydra-ide-common-debug
  (:title (format "%s Active Debug Controls" (all-the-icons-material "bug_report"))
   :quit-key ("q" "ESC")
   :color pink
   :foreign-keys run)
  ("Stepping"
   (("n" dap-next "Next")
    ("i" dap-step-in "Step in")
    ("o" dap-step-out "Step out")
    ("c" dap-continue "Continue")
    ("r" dap-restart-frame "Restart frame"))
   "Switch"
   (("ss" dap-switch-session "Session")
    ("st" dap-switch-thread "Thread")
    ("sf" dap-switch-stack-frame "Stack frame")
    ("su" dap-up-stack-frame "Up stack frame")
    ("sd" dap-down-stack-frame "Down stack frame")
    ("sl" dap-ui-locals "List locals")
    ("sb" dap-ui-breakpoints "List breakpoints")
    ("sS" dap-ui-sessions "List sessions"))
   "Breakpoints"
   (("bb" dap-breakpoint-toggle "Toggle")
    ("ba" dap-breakpoint-add "Add")
    ("bd" dap-breakpoint-delete "Delete")
    ("bz" dap-breakpoint-delete-all "Delete all")
    ("bc" dap-breakpoint-condition "Set condition")
    ("bh" dap-breakpoint-hit-condition "Set hit count")
    ("bl" dap-breakpoint-log-message "Set log message"))
   "Eval"
   (("ee" dap-eval "Eval")
    ("ea" dap-ui-expressions-add "Add expression")
    ("er" dap-eval-region "Eval region")
    ("ep" dap-eval-thing-at-point "Eval thing at point"))
   "Debug"
   (;; call run-debugger with prefix (C-u) so that
    ;; user is prompted for configuration
    ("dd" (lambda () (interactive) (setq current-prefix-arg '(4))
            (call-interactively #'ide-common-debug-run-debugger)) "Debug")
    ("dr" dap-debug-recent "Debug recent")
    ;; dap-debug-last should have the same result as calling
    ;; ide-common-debug-run-debugger without prefix
    ("dl" dap-debug-last "Debug last")
    ("de" dap-debug-edit-template "Edit debug template" :exit t)
    ("ds" dap-debug-restart "Debug restart"))
   "Session"
   (("T" ide-common-debug-disconnect-and-delete-all-sessions "Terminate all" :color blue)
    ("t" ide-common-debug-disconnect-and-delete-session "Terminate" :color red)
    ("X" ide-common-debug-disconnect-all-sessions "Disconnect all" :color blue)
    ("x" dap-disconnect "Disconnect" :color red))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end of file

(provide 'ide-common-debug)
;;; ide-common-debug.el ends here
