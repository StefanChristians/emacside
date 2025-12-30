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
(require 'major-mode-hydra)


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

;; debugger cache
(defvar ide-common-debug-debuggers nil
  "Mapping of debuggers to executables in projects.

Alist of (PROJECT . (EXECUTABLES)) pairs.
EXECUTABLES is an alist of (EXECUTABLE . DEBUGGER) pairs.

\((project1 . ((executable1 . debugger1) (executable2 . debugger2))))")

(defvar ide-common-debug-debuggers-file
  (f-join user-emacs-directory ".cache" "ide-common-debug.eld")
  "Path to file used for persistent debugger cache.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cache maintenance

(defun ide-common-debug-load-all ()
  "Load all debug caches from disk."
  (setq ide-common-debug-debuggers
        (ide-common-read-file ide-common-debug-debuggers-file)))

(defun ide-common-debug-save-debugger ()
  "Save last used debugger cache to disk."
  (ide-common-write-file ide-common-debug-debuggers-file
                         ide-common-debug-debuggers))

(defun ide-common-debug-save-all ()
  "Save all debug caches to disk."
  (ide-common-debug-save-debugger))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; current debugger getters and setters

(defun ide-common-debug-get-executables (project-root)
  "Return EXECUTABLES in PROJECT-ROOT.

EXECUTABLES is an alist of (EXECUTABLE . DEBUGGER) pairs."
  (alist-get (f-slash project-root) ide-common-debug-debuggers nil nil #'string=))

(defun ide-common-debug-get-debugger (project-root executable)
  "Return debugger for EXECUTABLE in PROJECT-ROOT."
  (let ((executable (ide-common-relative-path-if-descendant executable project-root)))
    (alist-get executable (ide-common-debug-get-executables project-root) nil nil #'string=)))

(defun ide-common-debug-get-binary (project-root executable)
  "Return binary of debugger for EXECUTABLE in PROJECT-ROOT."
  (let* ((name (ide-common-debug-get-debugger project-root executable))
         (entry (assoc name ide-common-debug-adapters)))
    (when entry
      (plist-get (cdr entry) :binary))))

(defun ide-common-debug-get-type (project-root executable)
  "Return type of debugger for EXECUTABLE in PROJECT-ROOT."
  (let* ((name (ide-common-debug-get-debugger project-root executable))
         (entry (assoc name ide-common-debug-adapters)))
    (when entry
      (plist-get (cdr entry) :type))))

(defun ide-common-debug-set-debugger (project-root executable debugger-name)
  "Set DEBUGGER-NAME for EXECUTABLE in PROJECT-ROOT."
  (let ((executable (ide-common-relative-path-if-descendant executable project-root)))
    (setf (alist-get executable (alist-get (f-slash project-root) ide-common-debug-debuggers nil nil #'string=) nil nil #'string=) debugger-name)
    (ide-common-debug-save-debugger)))

(defun ide-common-debug-unset-executable (project-root executable)
  "Remove debugger for EXECUTABLE in PROJECT-ROOT."
  (let* ((executable (ide-common-relative-path-if-descendant executable project-root))
         (executables (ide-common-debug-get-executables project-root))
         (updated (assoc-delete-all executable executables)))
    (setf (alist-get (f-slash project-root) ide-common-debug-debuggers nil nil #'string=) updated)
    (ide-common-debug-save-debugger)))

(defun ide-common-debug-unset-all (project-root)
  "Delete all debuggers for PROJECT-ROOT."
  (setq ide-common-debug-debuggers
        (assoc-delete-all (f-slash project-root) ide-common-debug-debuggers #'string=))
  (ide-common-debug-save-debugger))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; action

(defun ide-common-debug-select-debugger (executable &optional project-root)
  "Select a debugger for EXECUTABLE in PROJECT-ROOT."
  (ide-common-debug-load-all)
  (if (null ide-common-debug-adapters)
      (error "No debuggers available")
    (let* ((root (or project-root
                     (ide-common-get-current-context-project-root)))
           (executable (ide-common-relative-path-if-descendant executable root))
           (last (ide-common-debug-get-debugger root executable))
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
        (ide-common-debug-set-debugger root executable choice)
        choice))))

(defun ide-common-debug-run-debugger (&optional prefix project-root)
  "Run debugger in PROJECT-ROOT.

If PREFIX is given (usually `C-u`), prompt for configuration.
Otherwise reuse last configuration."
  (interactive "P")
  (let* ((root (or project-root
                  (ide-common-get-current-context-project-root)))
         (executable (or (and prefix
                              (ide-common-launch-select-executable root))
                         (ide-common-launch-get-current root)
                         (ide-common-launch-select-executable root)))
         ;; if executable is relative, append to root
         ;; if executable is absolute, use as is
         (program (f-join root executable))
         (debugger (or (and prefix
                            (ide-common-debug-select-debugger program root))
                       (ide-common-debug-get-debugger root program)
                       (ide-common-debug-select-debugger program root)))
         (type (ide-common-debug-get-type root program))
         (binary (ide-common-debug-get-binary root program))
         (command (concat binary " " (f-filename program)))
         (request "launch")
         (cwd root)
         ;; environment passed to target
         (env (ide-common-env-load-as-dap-json
               root
               (ide-common-env-get-current-profile root)))
         ;; environment in which debugger is run
         (process-environment (append
                               (ide-common-env-load-as-list
                                root
                                (ide-common-env-get-current-profile root))
                               (copy-sequence process-environment)))
         (raw-args (progn
                 (when prefix (ide-common-args-select-and-edit program root))
                 (ide-common-args-get-profile
                  root
                  program
                  (ide-common-args-get-current-profile root program))))
         (args (cond
                ((null raw-args) [])
                ((vectorp raw-args) raw-args)
                ((listp raw-args) (vconcat raw-args))
                (t [])))
         (template (list :type type
           :request request
           :name command
           :program program
           :args args
           :cwd cwd
           :environment env)))
    (dap-debug template)))


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

(pretty-hydra-define ide-common-debug-hydra
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

(ide-register-hydra
 'ide-common-debug-hydra/body
 (lambda () (and (fboundp 'dap--cur-session) (dap--cur-session)))
 50)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; bootstrap

;; load persistent cache
(ide-common-debug-load-all)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end of file

(provide 'ide-common-debug)
;;; ide-common-debug.el ends here
