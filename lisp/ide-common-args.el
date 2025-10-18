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
;;;; persistent cache (alist-based)

;; arguments cache
(defvar ide-common-args-project-commands '()
  "Alist mapping projects to command argument profiles.
Each value is an alist mapping COMMAND strings to a list
of (PROFILE-NAME . ARGS-LIST).
ARGS-LIST is a list of strings representing command line arguments.")

(defvar ide-common-args-cache-file
  (f-join user-emacs-directory ".cache" "ide-args.eld")
  "Path to file used for persistent command argument cache.")

;; last profile cache
(defvar ide-common-args-last '()
  "Alist mapping project roots to alists of (COMMAND . LAST-PROFILE).")

(defvar ide-common-args-last-file
  (f-join user-emacs-directory ".cache" "ide-args-last.eld")
  "Path to file storing last selected argument profile per command per project.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cache maintenance

(defun ide-common-args-load-cache ()
  "Load all argument caches from disk."
  (setq ide-common-args-project-commands
        (or (ide-common-read-file ide-common-args-cache-file) '()))
  (setq ide-common-args-last
        (or (ide-common-read-file ide-common-args-last-file) '())))

(defun ide-common-args-save-profile-cache ()
  "Save argument profiles cache to disk."
  (ide-common-write-file ide-common-args-cache-file
                         ide-common-args-project-commands))

(defun ide-common-args-save-last-cache ()
  "Save last used argument profiles cache to disk."
  (ide-common-write-file ide-common-args-last-file
                         ide-common-args-last))

(defun ide-common-args-save-cache ()
  "Save all argument caches to disk."
  (ide-common-args-save-profile-cache)
  (ide-common-args-save-last-cache))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; argument getters and setters

(defun ide-common-args-get-command-table (project-root)
  "Return command table (alist) for PROJECT-ROOT, or create one if missing."
  (let ((entry (assoc project-root ide-common-args-project-commands)))
    (if entry
        (cdr entry)
      (let ((table '()))
        (push (cons project-root table) ide-common-args-project-commands)
        table))))

(defun ide-common-args-set-command-table (project-root table)
  "Set command TABLE for PROJECT-ROOT."
  (setq ide-common-args-project-commands
        (cons (cons project-root table)
              (assoc-delete-all project-root ide-common-args-project-commands)))
  (ide-common-args-save-profile-cache))

(defun ide-common-args-get-profiles (project-root command)
  "Return profile alist for COMMAND in PROJECT-ROOT."
  (let* ((cmd-table (ide-common-args-get-command-table project-root))
         (entry (assoc command cmd-table)))
    (or (cdr entry) '())))

(defun ide-common-args-get-profile (project-root command profile)
  "Return ARGS-LIST for PROFILE under COMMAND in PROJECT-ROOT."
  (alist-get profile
             (ide-common-args-get-profiles project-root command)
             nil nil #'equal))

(defun ide-common-args-set-profile (project-root command profile args)
  "Set ARGS as argument list for PROFILE under COMMAND in PROJECT-ROOT."
  (let* ((cmd-table (ide-common-args-get-command-table project-root))
         (profiles (ide-common-args-get-profiles project-root command))
         (updated (cons (cons profile args)
                        (assoc-delete-all profile profiles)))
         (new-table (cons (cons command updated)
                          (assoc-delete-all command cmd-table))))
    (ide-common-args-set-command-table project-root new-table)))

(defun ide-common-args-unset-profile (project-root command profile)
  "Remove PROFILE for COMMAND in PROJECT-ROOT."
  (let* ((cmd-table (ide-common-args-get-command-table project-root))
         (profiles (ide-common-args-get-profiles project-root command))
         (updated (assoc-delete-all profile profiles))
         (new-table (cons (cons command updated)
                          (assoc-delete-all command cmd-table))))
    (ide-common-args-set-command-table project-root new-table)))

(defun ide-common-args-unset-command (project-root command)
  "Remove all profiles for COMMAND in PROJECT-ROOT."
  (let* ((cmd-table (ide-common-args-get-command-table project-root))
         (new-table (assoc-delete-all command cmd-table)))
    (ide-common-args-set-command-table project-root new-table)))

(defun ide-common-args-unset-all (project-root)
  "Delete all argument profiles for PROJECT-ROOT."
  (setq ide-common-args-project-commands
        (assoc-delete-all project-root ide-common-args-project-commands))
  (ide-common-args-save-profile-cache))

(defun ide-common-args-list-profile-names (project-root command)
  "Return list of profile names for COMMAND under PROJECT-ROOT."
  (or (mapcar #'car (ide-common-args-get-profiles project-root command))
      '("default")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; last used profile getters and setters

(defun ide-common-args-get-last-table (project-root)
  "Return alist of last profiles for PROJECT-ROOT, creating it if needed."
  (let ((entry (assoc project-root ide-common-args-last)))
    (if entry
        (cdr entry)
      (let ((tbl '()))
        (push (cons project-root tbl) ide-common-args-last)
        tbl))))

(defun ide-common-args-set-last-table (project-root table)
  "Set last profile TABLE for PROJECT-ROOT."
  (setq ide-common-args-last
        (cons (cons project-root table)
              (assoc-delete-all project-root ide-common-args-last)))
  (ide-common-args-save-last-cache))

(defun ide-common-args-get-last-profile (project-root command)
  "Return last used profile name for COMMAND under PROJECT-ROOT."
  (let* ((cmd-table (ide-common-args-get-last-table project-root))
         (entry (assoc command cmd-table)))
    (or (cdr entry) "default")))

(defun ide-common-args-set-last-profile (project-root command profile)
  "Set last used PROFILE name for COMMAND under PROJECT-ROOT."
  (let* ((cmd-table (ide-common-args-get-last-table project-root))
         (new-table (cons (cons command profile)
                          (assoc-delete-all command cmd-table))))
    (ide-common-args-set-last-table project-root new-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; conversions

(defun ide-common-args-store-string-as-list (project-root command profile text)
  "Convert TEXT to args list in PROFILE of COMMAND under PROJECT-ROOT."
  (ide-common-args-set-profile project-root command profile
                               (split-string-shell-command text)))

(defun ide-common-args-load-as-display-string (project-root command &optional profile)
  "Convert args list in PROFILE of COMMAND under PROJECT-ROOT to text."
  (let ((profile (or profile (ide-common-args-get-last-profile project-root command))))
    (combine-and-quote-strings
     (ide-common-args-get-profile project-root command profile))))

(defun ide-common-args-load-as-shell-string (project-root command &optional profile)
  "Convert args list in PROFILE of COMMAND under PROJECT-ROOT to shell string."
  (let ((profile (or profile (ide-common-args-get-last-profile project-root command))))
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
  (ide-common-args-load-cache)
  (let* ((root (or project-root
                   (ide-common-get-current-context-project-root)))
         (profiles (sort (ide-common-args-list-profile-names root command) #'string<))
         (default  (ide-common-args-get-last-profile root command))
         ;; pass category to completion frameworks via completion-extra-properties
         (completion-extra-properties `(:category arguments-profile :project-root ,root :command-id ,command))
         (choice (completing-read
                  (format "Select arguments profile for %s: " command)
                  profiles
                  nil t nil nil default)))
    ;; create profile if missing
    (unless (assoc choice (ide-common-args-get-profiles root command))
      (when (s-blank? choice)
        (user-error "Profile name cannot be empty"))
      (ide-common-args-set-profile root command choice nil))
    ;; save last selected
    (ide-common-args-set-last-profile root command choice)
    (message "Profile %s selected." choice)
    choice))

(defvar-local ide-common-args-current-project nil)
(defvar-local ide-common-args-current-command nil)
(defvar-local ide-common-args-current-profile nil)

(defun ide-common-args-edit (command &optional project-root)
  "Open argument editor for COMMAND in PROJECT-ROOT."
  (interactive)
  (ide-common-args-load-cache)
  (let* ((root (or project-root (ide-common-get-current-context-project-root)))
         (profile (ide-common-args-get-last-profile root command))
         (buf-name (format "*%s→%s [%s]*"
                           (f-filename root) command profile)))
    (with-current-buffer (get-buffer-create buf-name)
      (ide-common-args-mode)
      (setq ide-common-args-current-project root)
      (setq ide-common-args-current-command command)
      (setq ide-common-args-current-profile profile)
      (ide-common-args-refresh)
      (switch-to-buffer (current-buffer)))))

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
  (ide-common-args-store-string-as-list ide-common-args-current-project
                           ide-common-args-current-command
                           ide-common-args-current-profile
                           (buffer-string))
  (message (format "Saved profile %s" ide-common-args-current-profile)))

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
    (ide-common-args-set-last-profile root command dest)
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
    (ide-common-args-set-last-profile root command dest)
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
      (ide-common-args-set-last-profile root command ide-common-args-current-profile)
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
      (ide-common-args-set-profile root command "default" nil)
      (setq ide-common-args-current-profile "default")

      ;; update last used and current profile
      (ide-common-args-set-last-profile root command ide-common-args-current-profile)
      (message "Deleted all profiles, switched to %s" ide-common-args-current-profile)
      (rename-buffer (format "*%s→%s [%s]*" (f-filename root) command
                             ide-common-args-current-profile) t)
      (ide-common-args-refresh))
    ide-common-args-current-profile))

(defun ide-common-args-close()
  "Quit the arguments editor."
  (quit-window t))

(defun ide-common-args-save-and-close()
  "Save and close the arguments editor."
  (ide-common-args-save-buffer)
  (ide-common-args-close))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; key bindings

(pretty-hydra-define hydra-ide-common-args
  (:title  (format "%s Argument Profiles" (all-the-icons-material "list_alt"))
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
    (define-key map (kbd "C-c C-k") #'ide-common-args-cancel-and-close)
    map))

(setq ide-common-args-mode-map (ide-common-args-setup-keymap))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end of file

(provide 'ide-common-args)
;;; ide-common-args.el ends here
