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

;; current executable cache
(defvar ide-common-launch-current nil
  "Mapping of currently launched executable to project.

Alist of (PROJECT . EXECUTABLE) pairs.

\((project1 . executable1) (project2 . executable2))")

(defvar ide-common-launch-current-file
  (f-join user-emacs-directory ".cache" "ide-common-launch.eld")
  "Path to file used for current executable cache.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cache maintenance

(defun ide-common-launch-load-all ()
  "Load all launch caches from disk."
  (setq ide-common-launch-current
        (or (ide-common-read-file ide-common-launch-current-file) nil)))

(defun ide-common-launch-save-current ()
  "Save last launched executable cache to disk."
  (ide-common-write-file ide-common-launch-current-file
                         ide-common-launch-current))

(defun ide-common-launch-save-all ()
  "Save all launch caches to disk."
  (ide-common-launch-save-current))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; last executable getters and setters

(defun ide-common-launch-get-current (project-root)
  "Return last used executable for PROJECT-ROOT."
 (alist-get (f-slash project-root) ide-common-launch-current nil nil #'string=))

(defun ide-common-launch-set-current (project-root executable)
  "Set last used EXECUTABLE for PROJECT-ROOT."
  (let ((executable (ide-common-relative-path-if-descendant
                     executable project-root)))
    (setf (alist-get (f-slash project-root)
                     ide-common-launch-current nil nil #'string=)
          executable)
    (ide-common-launch-save-current)))

(defun ide-common-launch-unset-current (project-root)
  "Remove cached executable for PROJECT-ROOT."
  (setq ide-common-launch-current
        (assoc-delete-all (f-slash project-root) ide-common-launch-current #'string=))
  (ide-common-launch-save-current))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; actions

(defun ide-common-launch-select-executable (&optional project-root)
  "Prompt the user to select a file to launch, starting from PROJECT-ROOT.
Caches the selection as the last launched executable for this project."
  (ide-common-launch-load-all)
  (let* ((root (or project-root
                     (ide-common-get-current-context-project-root)))
         (default-file (ide-common-launch-get-current root))
         (prompt (if default-file
                     (format "Select executable (default %s): " default-file)
                   "Select executable: "))
         (file (expand-file-name (read-file-name prompt root default-file t nil) root))
         (relative-file (when file
                          (ide-common-relative-path-if-descendant file root))))
    ;; Cache the last selected executable
    (when relative-file
      (ide-common-launch-set-current root relative-file))
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
                         (ide-common-launch-get-current root)
                         (ide-common-launch-select-executable root)))
         ;; if executable is relative, append to root
         ;; if executable is absolute, use as is
         (program (f-join root executable))
         (env (ide-common-env-load-as-list root))
         (args (progn
                 (when prefix (ide-common-args-select-and-edit executable root))
                 (ide-common-args-get-profile
                  root
                  executable
                  (ide-common-args-get-current-profile root executable))))
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
(ide-common-launch-load-all)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end of file

(provide 'ide-common-launch)
;;; ide-common-launch.el ends here
