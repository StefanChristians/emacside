;;; ide-common-launch.el --- launch management -*- lexical-binding: t; -*-

;; Version: 0.0.1

;; Copyright (C) 2025 Stefan Christians
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Manage launch configurations and launch executables directly without debugger

;;; Code:

(require 'f)
(require 'comint)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; persistent cache

;; last launched executable cache
(defvar ide-common-launch-last-executable '()
  "Alist mapping PROJECT-ROOT → last launched EXECUTABLE.")

(defvar ide-common-launch-last-executable-file
  (f-join user-emacs-directory ".cache" "ide-common-launch-last-executable.eld")
  "Path to file storing last debugged executable per project.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cache maintenance

(defun ide-common-launch-load-cache ()
  "Load all launch caches from disk."
  (setq ide-common-launch-last-executable
        (or (ide-common-read-file ide-common-launch-last-executable-file) '())))

(defun ide-common-launch-save-last-executable-cache ()
  "Save last debugged executable cache to disk."
  (ide-common-write-file ide-common-launch-last-executable-file
                         ide-common-launch-last-executable))

(defun ide-common-debug-save-cache ()
  "Save all launch caches to disk."
  (ide-common-launch-save-last-executable-cache))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; last executable getters and setters

(defun ide-common-launch-get-last-executable (project-root)
  "Return last launched executable for PROJECT-ROOT, or nil if none."
  (cdr (assoc project-root ide-common-launch-last-executable)))

(defun ide-common-launch-set-last-executable (project-root executable)
  "Set last launched EXECUTABLE for PROJECT-ROOT."
  (setq ide-common-launch-last-executable
        (cons (cons project-root executable)
              (assoc-delete-all project-root ide-common-launch-last-executable)))
  (ide-common-launch-save-last-executable-cache))

(defun ide-common-launch-unset-last-executable (project-root)
  "Remove cached last launched executable for PROJECT-ROOT."
  (setq ide-common-launch-last-executable
        (assoc-delete-all project-root ide-common-launch-last-executable))
  (ide-common-launch-save-last-executable-cache))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; actions

(defun ide-common-launch-select-executable (&optional project-root)
  "Prompt the user to select a file to launch, starting from PROJECT-ROOT.
Caches the selection as the last launched executable for this project."
  (ide-common-launch-load-cache)
  (let* ((root (or project-root
                     (ide-common-get-current-context-project-root)))
         (default-file (ide-common-launch-get-last-executable root))
         (prompt (if default-file
                     (format "Select executable (default %s): " default-file)
                   "Select executable: "))
         (file (expand-file-name (read-file-name prompt root nil t nil) root))
         (relative-file (when file
                          (ide-common-relative-path-if-descendant file root))))
    ;; Cache the last selected executable
    (when relative-file
      (ide-common-launch-set-last-executable root relative-file))
    relative-file))

(defun ide-common-launch-execute (&optional prefix project-root)
  "Run program in PROJECT-ROOT without debugger.

If PREFIX is given (usually `C-u`), prompt for configuration.
Otherwise reuse last chosen configuration."
  (interactive "P")
  (let* ((root (or project-root
                  (ide-common-get-current-context-project-root)))
         (executable (or (and prefix
                              (ide-common-launch-select-executable root))
                         (ide-common-launch-get-last-executable root)
                         (ide-common-launch-select-executable root)))
         ;; if executable is relative, append to root
         ;; if executable is absolute, use as is
         (program (f-join root executable))
         (env (progn
                (when prefix (ide-common-env-select-and-edit root))
                (ide-common-env-load-as-list root)))
         (args (progn
                 (when prefix (ide-common-args-select-and-edit executable root))
                 (ide-common-args-get-profile
                  root
                  executable
                  (ide-common-args-get-last-profile root executable))))
         (process-environment (append env process-environment))
         (buffer-name (format "*%s*" (f-filename executable))))
    (when (comint-check-proc buffer-name)
      (kill-buffer buffer-name))
    (apply #'make-comint-in-buffer buffer-name buffer-name program nil args)
    (pop-to-buffer buffer-name)
    (with-current-buffer buffer-name
      (comint-mode)
      (setq-local truncate-lines t)
      (setq-local comint-prompt-read-only t)
      (setq-local comint-scroll-to-bottom-on-output t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; bootstrap

;; load persistent cache
(ide-common-launch-load-cache)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end of file

(provide 'ide-common-launch)
;;; ide-common-launch.el ends here
