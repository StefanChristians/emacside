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
Each PROJECT-ROOT entry holds an alist of executables to their last used debugger name.")

(defvar ide-common-debug-last-file
  (f-join user-emacs-directory ".cache" "ide-debugger.eld")
  "Path to file storing last debugger per executable per project.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cache maintenance

(defun ide-common-debug-load-cache ()
  "Load all debug caches from disk."
  (setq ide-common-debug-last-debugger
        (or (ide-common-read-file ide-common-debug-last-file) '())))

(defun ide-common-debug-save-last-debugger-cache ()
  "Save last used debugger cache to disk."
  (ide-common-write-file ide-common-debug-last-file
                         ide-common-debug-last-debugger))

(defun ide-common-debug-save-cache ()
  "Save all debug caches to disk."
  (ide-common-debug-save-last-debugger-cache))


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
;;;; action

(defun ide-common-debug-select-debugger (executable &optional project-root)
  "Select a debugger for EXECUTABLE in PROJECT-ROOT."
  (interactive)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end of file

(provide 'ide-common-debug)
;;; ide-common-debug.el ends here
