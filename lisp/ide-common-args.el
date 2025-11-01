;;; ide-common-args.el --- command arguments management -*- lexical-binding: t; -*-

;; Version: 0.0.1

;; Copyright (C) 2025 Stefan Christians
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Manage per-command argument profiles.
;;
;; Each command can have multiple named profiles,
;; each containing a list of arguments.

;;; Code:

(require 'f)
(require 's)
(require 'subr-x)
(require 'marginalia)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; persistent cache

;; arguments cache
(defvar ide-common-args-profiles nil
  "Mapping of argument profiles to commands in projects.

Alist of (PROJECT . (COMMANDS)) pairs.
COMMANDS is an alist of (COMMAND . (PROFILES)) pairs.
PROFILES is an alist of (PROFILE . (ARGS)) pairs.
ARGS is a list of strings.

\((project1 . (command1 . (profile1 . (arg1 arg2)))))")

(defvar ide-common-args-profiles-file
  (f-join user-emacs-directory ".cache" "ide-common-args-profiles.eld")
  "Path to file used for persistent command argument cache.")

;; current profile cache
(defvar ide-common-args-current nil
  "Mapping of currently used arguments profile to command in project.

Alist of (PROJECT . COMMANDS) pairs.
COMMANDS is an alist of (COMMAND . PROFILE) pairs.

\((project1 . ((command1 . profile1) (command2 . profile2))))")

(defvar ide-common-args-current-file
  (f-join user-emacs-directory ".cache" "ide-common-args-current.eld")
  "Path to file used for current profile cache.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cache maintenance

(defun ide-common-args-load-all ()
  "Load all argument caches from disk."
  (setq ide-common-args-profiles
        (or (ide-common-read-file ide-common-args-profiles-file) '()))
  (setq ide-common-args-current
        (or (ide-common-read-file ide-common-args-current-file) '())))

(defun ide-common-args-save-profiles ()
  "Save argument profiles cache to disk."
  (ide-common-write-file ide-common-args-profiles-file
                         ide-common-args-profiles))

(defun ide-common-args-save-current ()
  "Save last used argument profiles cache to disk."
  (ide-common-write-file ide-common-args-current-file
                         ide-common-args-current))

(defun ide-common-args-save-all ()
  "Save all argument caches to disk."
  (ide-common-args-save-profiles)
  (ide-common-args-save-current))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; argument getters and setters

(defun ide-common-args-get-commands (project-root)
  "Return COMMANDS in PROJECT-ROOT.

COMMANDS is an alist of (COMMAND . (PROFILES)) pairs.
PROFILES is an alist of (PROFILE . (ARGS)) pairs.
ARGS is a list of strings."
  (alist-get project-root ide-common-args-profiles nil nil #'string=))

(defun ide-common-args-get-profiles (project-root command)
  "Return PROFILES for COMMAND in PROJECT-ROOT.

PROFILES is an alist of (PROFILE . (ARGS)) pairs.
ARGS is a list of strings."
  (alist-get command (ide-common-args-get-commands project-root) nil nil #'string=))

(defun ide-common-args-get-profile (project-root command profile)
  "Return ARGS for PROFILE for COMMAND in PROJECT-ROOT.

ARGS is a list of strings."
(alist-get profile
             (ide-common-args-get-profiles project-root command)
             nil nil #'equal))

(defun ide-common-args-set-profile (project-root command profile args)
  "Set ARGS for PROFILE for COMMAND under PROJECT-ROOT.

ARGS is a list of strings."
  (setf (alist-get profile (alist-get command (alist-get project-root ide-common-args-profiles nil nil #'string=) nil nil #'string=) nil nil #'string=) args)
  (ide-common-args-save-profiles))

(defun ide-common-args-unset-profile (project-root command profile)
  "Remove PROFILE from COMMAND in PROJECT-ROOT."
  (let* ((profiles (ide-common-args-get-profiles project-root command))
         (updated (assoc-delete-all profile profiles)))
    (setf (alist-get command (alist-get project-root ide-common-args-profiles nil nil #'string=) nil nil #'string=) updated)
    (ide-common-args-save-profiles)))

(defun ide-common-args-unset-command (project-root command)
  "Remove all profiles from COMMAND in PROJECT-ROOT."
  (let* ((commands (ide-common-args-get-commands project-root))
         (updated (assoc-delete-all command commands)))
    (setf (alist-get project-root ide-common-args-profiles nil nil #'string=) updated)
    (ide-common-args-save-profiles)))

(defun ide-common-args-unset-all (project-root)
  "Delete all profiles for PROJECT-ROOT."
  (setq ide-common-args-profiles
        (assoc-delete-all project-root ide-common-args-profiles #'string=))
  (ide-common-args-save-profiles))

(defun ide-common-args-list-profile-names (project-root command)
  "Return list of profile names for COMMAND under PROJECT-ROOT."
  (or (mapcar #'car (ide-common-args-get-profiles project-root command))
      '("default")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; current profile getters and setters

(defun ide-common-args-get-current-profile (project-root command)
  "Return last used profile name for COMMAND in PROJECT-ROOT."
  (or (alist-get command (alist-get project-root ide-common-args-current nil nil #'string=) nil nil #'string=)
      "default"))

(defun ide-common-args-set-current-profile (project-root command profile)
  "Set last used PROFILE name for COMMAND in PROJECT-ROOT."
  (setf (alist-get command (alist-get project-root ide-common-args-current nil nil #'string=) nil nil #'string=)
        profile)
  (ide-common-args-save-current))

(defun ide-common-args-unset-current-profile (project-root command)
  "Remove current profile from COMMAND in PROJECT-ROOT."
  (let* ((commands (alist-get project-root ide-common-args-current nil nil #'string=))
         (updated (assoc-delete-all command commands)))
    (setf (alist-get project-root ide-common-args-current nil nil #'string=) updated)
    (ide-common-args-save-profiles)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; conversions

(defun ide-common-args-store-string-as-list (project-root command profile text)
  "Convert TEXT to args list in PROFILE for COMMAND in PROJECT-ROOT."
  (ide-common-args-set-profile project-root command profile
                               (split-string-shell-command text)))

(defun ide-common-args-load-as-display-string (project-root command &optional profile)
  "Convert args list in PROFILE for COMMAND in PROJECT-ROOT to text."
  (let ((profile (or profile (ide-common-args-get-current-profile project-root command))))
    (combine-and-quote-strings
     (ide-common-args-get-profile project-root command profile))))

(defun ide-common-args-load-as-shell-string (project-root command &optional profile)
  "Convert args list in PROFILE for COMMAND in PROJECT-ROOT to shell string."
  (let ((profile (or profile (ide-common-args-get-current-profile project-root command))))
    (mapconcat #'shell-quote-argument
               (ide-common-args-get-profile project-root command profile)
               " ")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; annotations for completion

(defun marginalia-arguments-profile-plain (cand)
  "Annotate argument profile CAND with dim italic text.
Shows the argument list of the selected profile for the current command
and project."
  (let* ((root (plist-get completion-extra-properties :project-root))
         (command (plist-get completion-extra-properties :command-id))
         (profiles (ide-common-args-list-profile-names root command))
         (max-len (apply #'max (mapcar #'length profiles)))
         (args (ide-common-args-get-profile root command cand)))
    (when args
      (let* ((args-str
              (mapconcat
               (lambda (arg)
                 (if (and (stringp arg)
                          (string-match-p "[[:space:]\"\\$`]" arg))
                     (prin1-to-string arg)
                   arg))
               args " "))
             (padding (+ 2 (- max-len (length cand)))))
        (propertize
         (concat (make-string padding ?\s) args-str)
         'face 'marginalia-documentation)))))

(defun marginalia-arguments-profile-colored (cand)
  "Annotate argument profile CAND with color-coded command-line arguments.
Shows the argument list of the selected profile for the current command
and project, with syntax-aware coloring."
  (let* ((root (plist-get completion-extra-properties :project-root))
         (command (plist-get completion-extra-properties :command-id))
         (profiles (ide-common-args-list-profile-names root command))
         (max-len (apply #'max (mapcar #'length profiles)))
         (args (ide-common-args-get-profile root command cand)))
    (when args
      (let* ((args-str
              (mapconcat
               (lambda (arg)
                 (let* ((quoted (if (and (stringp arg)
                                         (string-match-p "[[:space:]\"\\$`]" arg))
                                    (prin1-to-string arg)
                                  arg))
                        ;; Colorize argument depending on its kind
                        (face
                         (cond
                          ;; Options start with - or --
                          ((string-prefix-p "--" arg) 'font-lock-keyword-face)
                          ((string-prefix-p "-" arg)  'font-lock-type-face)
                          ;; Paths
                          ((string-match-p "/" arg) 'font-lock-string-face)
                          ;; Quoted strings or variables
                          ((string-match-p "\\$\\|`\\|\"" arg) 'font-lock-variable-name-face)
                          ;; Numbers
                          ((string-match-p "^[0-9]+$" arg) 'font-lock-constant-face)
                          ;; Fallback
                          (t 'default))))
                   (propertize quoted 'face face)))
               args " "))
             (padding (+ 2 (- max-len (length cand)))))
        (concat
         (make-string padding ?\s) args-str)))))

;; Register annotators
(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotators
               '(arguments-profile marginalia-arguments-profile-colored
                                     marginalia-arguments-profile-plain
                                     none)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; actions

(defun ide-common-args-select-profile (command &optional project-root)
  "Select or create a profile for COMMAND in PROJECT-ROOT."
  (interactive)
  (ide-common-args-load-all)
  (let* ((root (or project-root
                   (ide-common-get-current-context-project-root)))
         (profiles (sort (ide-common-args-list-profile-names root command) #'string<))
         (default  (ide-common-args-get-current-profile root command))
         ;; pass category to completion frameworks via completion-extra-properties
         (completion-extra-properties `(:category arguments-profile :project-root ,root :command-id ,command))
         (choice (completing-read
                  (format "Select arguments profile for %s: " command)
                  profiles
                  nil nil nil nil default)))
    ;; create profile if missing
    (unless (assoc choice (ide-common-args-get-profiles root command))
      (when (s-blank? choice)
        (user-error "Profile name cannot be empty"))
      (ide-common-args-set-profile root command choice nil))
    ;; save last selected
    (ide-common-args-set-current-profile root command choice)
    (message "Profile %s selected." choice)
    choice))

(defvar-local ide-common-args-current-project nil)
(defvar-local ide-common-args-current-command nil)
(defvar-local ide-common-args-current-profile nil)

(defun ide-common-args-edit (command &optional project-root)
  "Open argument editor for COMMAND in PROJECT-ROOT."
  (interactive)
  (ide-common-args-load-all)
  (let* ((root (or project-root (ide-common-get-current-context-project-root)))
         (profile (ide-common-args-get-current-profile root command))
         (buf-name (format "*%s→%s [%s]*"
                           (f-filename root) command profile))
         (buf (get-buffer-create buf-name))
         result)
    ;; Prepare buffer
    (with-current-buffer buf
      (ide-common-args-mode)
      (setq ide-common-args-current-project root
            ide-common-args-current-command command
            ide-common-args-current-profile profile)
      (ide-common-args-refresh)

      ;; Hook to exit recursive edit if buffer is killed externally
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (> (recursion-depth) 0)
                    (throw 'exit nil)))
                nil t))

    ;; Display buffer and enter blocking recursive edit
    (pop-to-buffer buf)
    (recursive-edit)

    ;; remember result while buffer-local variables are still available
    (with-current-buffer buf
      (setq result (ide-common-args-get-profile
                    ide-common-args-current-project
                    ide-common-args-current-command
                    ide-common-args-current-profile)))

    ;; Cleanup buffer after recursive edit
    (when (buffer-live-p buf)
      (kill-buffer buf))

    result))

(defun ide-common-args-select-and-edit (command &optional project-root)
  "Select or create and edit a profile for COMMAND in PROJECT-ROOT."
  (when (ide-common-args-select-profile command project-root)
    (ide-common-args-edit command project-root)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; UI (text buffer)

(define-derived-mode ide-common-args-mode sh-mode "Arguments"
  "Major mode for editing command argument profiles."
  (setq-local header-line-format
              "Edit command-line arguments (C-c C-c to save, C-c C-k to cancel)")
  (setq-local comment-start "# ")
  (setq-local sh-basic-offset 0)
  (setq-local indent-line-function #'ignore))

(defun ide-common-args-refresh ()
  "Refresh buffer."
  (let* ((args-str (ide-common-args-load-as-display-string
                    ide-common-args-current-project
                    ide-common-args-current-command
                    ide-common-args-current-profile)))
      (erase-buffer)
      (insert args-str)
      (goto-char (point-max))))

(defun ide-common-args-save-buffer ()
  "Save buffer to arguments cache."
  (let ((clean-text (string-trim (substring-no-properties (buffer-string)))))
    (ide-common-args-store-string-as-list
     ide-common-args-current-project
     ide-common-args-current-command
     ide-common-args-current-profile
     clean-text)
    (message "Saved profile %s" ide-common-args-current-profile)))

(defun ide-common-args-change-profile ()
  "Switch to another profile for the current command."
  (interactive)
  (let ((new-profile (ide-common-args-select-profile
                      ide-common-args-current-command
                      ide-common-args-current-project)))
    (setq ide-common-args-current-profile new-profile)
    (message "Switched to profile: %s" new-profile)
    (rename-buffer (format "*%s→%s [%s]*"
                           (f-filename ide-common-args-current-project)
                           ide-common-args-current-command
                           ide-common-args-current-profile) t)
    (ide-common-args-refresh)))

(defun ide-common-args-save-and-change-profile ()
  "Save and switch profile."
  (interactive)
  (ide-common-args-save-buffer)
  (ide-common-args-change-profile))

(defun ide-common-args-clone-profile ()
  "Clone the profile currently open in the editor buffer and switch to it."
  (interactive)
  (let* ((root ide-common-args-current-project)
         (command ide-common-args-current-command)
         (source ide-common-args-current-profile)
         (default (concat source "-copy"))
         (dest (read-string (format "New profile name (default %s): " default) nil nil default)))
    (when (s-blank? dest)
      (user-error "Profile name cannot be empty"))
    (when (and (assoc dest (ide-common-args-get-profiles root command))
               (not (yes-or-no-p (format "Profile %s exists.  Overwrite? " dest))))
      (user-error "Clone aborted"))
    ;; clone the profile
    (ide-common-args-set-profile root command dest (copy-tree (ide-common-args-get-profile root command source)))
    ;; update last used and current profile
    (ide-common-args-set-current-profile root command dest)
    (setq ide-common-args-current-profile dest)
    (message "Cloned and switched to profile %s → %s" source dest)
    (rename-buffer (format "*%s→%s [%s]*" (f-filename root) command dest) t)
    (ide-common-args-refresh)
    dest))

(defun ide-common-args-save-and-clone-profile ()
  "Save and clone profile."
  (interactive)
  (ide-common-args-save-buffer)
  (ide-common-args-clone-profile))

(defun ide-common-args-rename-profile ()
  "Rename the profile currently open in the editor buffer."
  (interactive)
  (let* ((root ide-common-args-current-project)
         (command ide-common-args-current-command)
         (source ide-common-args-current-profile)
         (dest (read-string (format "Rename profile %s to: " source))))
    (when (s-blank? dest)
      (user-error "Profile name cannot be empty"))
    (when (assoc dest (ide-common-args-get-profiles root command))
      (user-error "Profile %s already exists" dest))

    ;; rename: clone source to dest and then delete source
    (ide-common-args-set-profile root command dest (copy-tree (ide-common-args-get-profile root command source)))
    (ide-common-args-unset-profile root command source)

    ;; update last used and current profile
    (ide-common-args-set-current-profile root command dest)
    (setq ide-common-args-current-profile dest)
    (message "Renamed profile %s → %s" source dest)
    (rename-buffer (format "*%s→%s [%s]*" (f-filename root) command dest) t)
    (ide-common-args-refresh)
    dest))

(defun ide-common-args-save-and-rename-profile ()
  "Save and rename profile."
  (interactive)
  (ide-common-args-save-buffer)
  (ide-common-args-rename-profile))

(defun ide-common-args-delete-profile ()
  "Delete the profile currently open in the editor buffer."
  (interactive)
  (let* ((root ide-common-args-current-project)
         (command ide-common-args-current-command)
         (source ide-common-args-current-profile))
    (when (yes-or-no-p (format "Are you sure you want to delete %s? " source))

      ;; remove the profile
      (ide-common-args-unset-profile root command source)

      ;; select next profile
      (setq ide-common-args-current-profile (ide-common-args-select-profile
                                             command root))

      ;; update last used and current profile
      (ide-common-args-set-current-profile root command ide-common-args-current-profile)
      (message "Deleted and switched to profile %s → %s" source ide-common-args-current-profile)
      (rename-buffer (format "*%s→%s [%s]*" (f-filename root) command
                             ide-common-args-current-profile) t)
      (ide-common-args-refresh))
    ide-common-args-current-profile))

(defun ide-common-args-delete-all-profiles ()
  "Delete all profiles for current command and create new default profile."
  (interactive)
  (let* ((root ide-common-args-current-project)
         (command ide-common-args-current-command))
    (when (yes-or-no-p (format "Are you sure you want to delete all profiles for %s? " command))

      ;; delete all profiles
      (ide-common-args-unset-command root command)

      ;; create and select default profile
      (ide-common-args-set-profile root command "default" '())
      (setq ide-common-args-current-profile "default")

      ;; update last used and current profile
      (ide-common-args-set-current-profile root command ide-common-args-current-profile)
      (message "Deleted all profiles, switched to %s" ide-common-args-current-profile)
      (rename-buffer (format "*%s→%s [%s]*" (f-filename root) command
                             ide-common-args-current-profile) t)
      (ide-common-args-refresh))
    ide-common-args-current-profile))

(defun ide-common-args-close()
  "Close the arguments editor and continue execution."
  (interactive)
  ;; exit from recursive edit
  (throw 'exit nil))

(defun ide-common-args-save-and-close()
  "Save and close the arguments editor and continue."
  (interactive)
  (ide-common-args-save-buffer)
  (throw 'exit nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; key bindings

(pretty-hydra-define hydra-ide-common-args
  (:title  (format "%s Argument Profiles" (all-the-icons-material "format_list_bulleted"))
          :color teal
          :quit-key ("q" "ESC"))
  ("Profiles"
   (("P" ide-common-args-save-and-change-profile "Change profile")
    ("R" ide-common-args-save-and-rename-profile "Rename profile")
    ("C" ide-common-args-save-and-clone-profile "Clone profile")
    ("D" ide-common-args-delete-profile "Delete profile")
    ("C-D" ide-common-args-delete-all-profiles "Delete all profiles"))
   "Session"
   (("X" ide-common-args-close "Quit" :color blue)
    ("x" ide-common-args-save-and-close "Close" :color blue))))

(defun ide-common-args-setup-keymap ()
  "Define keymap for `ide-common-args-mode'."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a") #'hydra-ide-common-args/body)
    (define-key map (kbd "C-c C-c") #'ide-common-args-save-and-close)
    (define-key map (kbd "C-c C-k") #'ide-common-args-close)
    map))

(setq ide-common-args-mode-map (ide-common-args-setup-keymap))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; bootstrap

;; load persistent cache
(ide-common-args-load-all)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end of file

(provide 'ide-common-args)
;;; ide-common-args.el ends here
