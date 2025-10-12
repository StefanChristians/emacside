;;; ide-common-env.el --- project environment management -*- lexical-binding: t; -*-

;; Version: 0.0.1

;; Copyright (C) 2025 Stefan Christians
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Manage per-project environment profiles.
;;
;; Each project can have multiple named profiles,
;; each containing a list of NAME=VALUE pairs.
;;
;; Two caches are maintained:
;;  - ~/.emacs.d/.cache/ide-env.el → project profiles and variables
;;  - ~/.emacs.d/.cache/ide-env-last.el → last selected profile per project

;;; Code:

(require 'f)
(require 's)
(require 'subr-x)
(require 'marginalia)
(require 'tabulated-list)
(require 'major-mode-hydra)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; persistent cache

;; environment cache
(defvar ide-common-env-project-profiles (make-hash-table :test 'equal)
  "Hash table mapping projects to environment profiles.
Each value is an alist of (PROFILE-NAME . VARS).
VARS is an alist of (NAME . VALUE).")

(defvar ide-common-env-cache-file
  (f-join user-emacs-directory ".cache" "ide-env.el")
  "Path to file used for persistent environment profile cache.")

;; last profile cache
(defvar ide-common-env-last (make-hash-table :test 'equal)
  "Hash table mapping project roots to last selected environment profile.")

(defvar ide-common-env-last-file
  (f-join user-emacs-directory ".cache" "ide-env-last.el")
  "Path to file storing last selected environment profile per project.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cache maintenance

(defun ide-common-env-load-cache ()
  "Load all environment caches from disk."
  (setq ide-common-env-project-profiles
        (or (ide-common-read-file ide-common-env-cache-file)
            (make-hash-table :test 'equal)))
  (setq ide-common-env-last
        (or (ide-common-read-file ide-common-env-last-file)
            (make-hash-table :test 'equal))))

(defun ide-common-env-save-profile-cache ()
  "Save environment profiles cache to disk."
  (ide-common-write-file ide-common-env-cache-file
                         ide-common-env-project-profiles))

(defun ide-common-env-save-last-cache ()
  "Save last used profile cache to disk."
  (ide-common-write-file ide-common-env-last-file
                                ide-common-env-last))

(defun ide-common-env-save-cache ()
  "Save all environment caches to disk."
  (ide-common-env-save-profile-cache)
  (ide-common-env-save-last-cache))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; environment getters and setters

(defun ide-common-env-get-profiles (project-root)
  "Return profile alist for PROJECT-ROOT."
  (or (gethash project-root ide-common-env-project-profiles) '()))

(defun ide-common-env-get-profile (project-root profile)
  "Return VARS alist for PROFILE under PROJECT-ROOT."
  (alist-get profile (ide-common-env-get-profiles project-root) nil nil #'equal))

(defun ide-common-env-set-profile (project-root profile vars)
  "Set VARS as environment alist for PROFILE in PROJECT-ROOT."
  (let* ((profiles (ide-common-env-get-profiles project-root))
         (updated (assoc-delete-all profile profiles)))
    (puthash project-root (cons (cons profile vars) updated)
             ide-common-env-project-profiles)
    (ide-common-env-save-profile-cache)))

(defun ide-common-env-unset-profile (project-root profile)
  "Remove PROFILE from PROJECT-ROOT."
  (let* ((profiles (ide-common-env-get-profiles project-root))
         (updated (assoc-delete-all profile profiles)))
    (puthash project-root updated ide-common-env-project-profiles)
    (ide-common-env-save-profile-cache)))

(defun ide-common-env-unset-all (project-root)
  "Delete all environment profiles for PROJECT-ROOT."
  (remhash project-root ide-common-env-project-profiles)
  (ide-common-env-save-profile-cache))

(defun ide-common-env-list-profile-names (project-root)
  "Return list of profile names for PROJECT-ROOT."
  (or (mapcar #'car (ide-common-env-get-profiles project-root)) '("default")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; last used profile getters and setters

(defun ide-common-env-get-last-profile (project-root)
  "Return last used profile name for PROJECT-ROOT."
  (or (gethash project-root ide-common-env-last) "default"))

(defun ide-common-env-set-last-profile (project-root profile)
  "Set last used PROFILE name for PROJECT-ROOT."
  (puthash project-root profile ide-common-env-last)
  (ide-common-env-save-last-cache))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; annotations for completion

(defun marginalia-environment-profile-plain (cand)
  "Annotate environment profile CAND with dim italic text."
  (let* ((root (plist-get completion-extra-properties :project-root))
         (profiles (ide-common-env-list-profile-names root))
         (max-len (apply #'max (mapcar #'length profiles)))
         (vars (ide-common-env-get-profile root cand)))
    (when vars
      (let* ((vars-str
              (mapconcat
               (lambda (pair)
                 (let ((val (cdr pair)))
                   (format "%s=%s"
                           (car pair)
                           (if (and (stringp val)
                                    (string-match-p "[[:space:]\"\\$`]" val))
                               (prin1-to-string val)
                             val))))
               vars " "))
             (padding (+ 2 (- max-len (length cand)))))
        (propertize
         (concat (make-string padding ?\s) vars-str)
         'face 'marginalia-documentation)))))

(defun marginalia-environment-profile-colored (cand)
  "Annotate environment profile CAND with color-coded variables."
  (let* ((root (plist-get completion-extra-properties :project-root))
         (profiles (ide-common-env-list-profile-names root))
         (max-len (apply #'max (mapcar #'length profiles)))
         (vars (ide-common-env-get-profile root cand)))
    (when vars
      (let* ((vars-str
              (mapconcat
               (lambda (pair)
                 (let* ((name (propertize (car pair) 'face 'font-lock-variable-name-face))
                        (val (cdr pair))
                        (val-str
                         (cond
                          ;; Strings needing quoting (spaces, quotes, $ etc.)
                          ((and (stringp val)
                                (string-match-p "[[:space:]\"\\$`]" val))
                           (propertize (prin1-to-string val) 'face 'font-lock-string-face))
                          ;; Pure numbers
                          ((and (stringp val)
                                (string-match-p "\\`[0-9.]+\\'" val))
                           (propertize val 'face 'font-lock-constant-face))
                          ((numberp val)
                           (propertize (number-to-string val) 'face 'font-lock-constant-face))
                          ;; Everything else (unquoted plain strings)
                          (t (propertize (format "%s" val) 'face 'font-lock-string-face)))))
                   (format "%s=%s" name val-str)))
               vars " "))
             (padding (+ 2 (- max-len (length cand)))))
        (concat (make-string padding ?\s) vars-str)))))

;; Register annotators
(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotators
               '(environment-profile marginalia-environment-profile-colored
                                     marginalia-environment-profile-plain
                                     none)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; actions

(defun ide-common-env-select-profile (&optional project-root)
  "Select or create a profile for PROJECT-ROOT."
  (interactive)
  (ide-common-env-load-cache)
  (let* ((root (or project-root
                   (ide-common-get-current-context-project-root)))
         (profiles (sort (ide-common-env-list-profile-names root) #'string<))
         (default  (ide-common-env-get-last-profile root))
         ;; pass category to completion frameworks via completion-extra-properties
         (completion-extra-properties `(:category environment-profile :project-root ,root))
         (choice (completing-read
                  (format "Select environment profile for %s: "
                          (f-filename root))
                  profiles
                  nil t nil nil default)))
    ;; create profile if missing
    (unless (assoc choice (ide-common-env-get-profiles root))
      (when (s-blank? choice)
        (user-error "Profile name cannot be empty"))
      (ide-common-env-set-profile root choice nil))
    ;; save last selected
    (ide-common-env-set-last-profile root choice)
    (message "Profile %s selected." choice)
    choice))

(defvar-local ide-common-env-current-project nil)
(defvar-local ide-common-env-current-profile nil)

(defun ide-common-env-edit (&optional project-root)
  "Open environment editor for PROJECT-ROOT."
  (interactive)
  (ide-common-env-load-cache)
  (let* ((root (or project-root (ide-common-get-current-context-project-root)))
         (profile (ide-common-env-get-last-profile root))
         (buf-name (format "*%s [%s]*" (f-filename root) profile)))
    (with-current-buffer (get-buffer-create buf-name)
      (ide-common-env-mode)
      (setq ide-common-env-current-project root)
      (setq ide-common-env-current-profile profile)
      (ide-common-env-refresh)
      (switch-to-buffer (current-buffer)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; UI (tabulated list)

(define-derived-mode ide-common-env-mode tabulated-list-mode "Environment"
  "Major mode for editing project environment profiles."
  (setq tabulated-list-format [("Variable" 30 t) ("Value" 60 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'ide-common-env-refresh nil t)
  (tabulated-list-init-header))

(defun ide-common-env-refresh ()
  "Refresh environment variable list in the current buffer."
  (let* ((vars (ide-common-env-get-profile ide-common-env-current-project
                                           ide-common-env-current-profile)))
    (setq tabulated-list-entries
          (mapcar (lambda (pair)
                    (let ((name (car pair))
                          (value (cdr pair)))
                      (list name (vector name value))))
                  vars)))
  (tabulated-list-print t))

(defun ide-common-env-edit-value ()
  "Edit the value of the environment variable at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (entry (assoc id (ide-common-env-get-profile
                             ide-common-env-current-project
                             ide-common-env-current-profile))))
    (when entry
      (let ((new-value (read-string (format "New value for %s: " (car entry))
                                    (cdr entry))))
        (setcdr entry new-value)
        (ide-common-env-set-profile ide-common-env-current-project
                                    ide-common-env-current-profile
                                    (ide-common-env-get-profile
                                       ide-common-env-current-project
                                       ide-common-env-current-profile))
        (ide-common-env-refresh)))))

(defun ide-common-env-edit-name ()
  "Edit the name of the environment variable at point."
  (interactive)
  (let* ((old-name (tabulated-list-get-id))
         (vars (ide-common-env-get-profile
                ide-common-env-current-project
                ide-common-env-current-profile))
         (entry (assoc old-name vars)))
    (when entry
      (let* ((new-name (read-string (format "Rename variable %s to: " old-name)
                                    old-name))
             ;; Ensure no duplicate variable names
             (existing (assoc new-name vars)))
        (cond
         ((string= new-name old-name)
          (message "No change."))
         (existing
          (user-error "A variable named '%s' already exists" new-name))
         (t
          ;; Replace key: remove old entry, add new one
          (setq vars (cons (cons new-name (cdr entry))
                           (assoc-delete-all old-name vars)))
          ;; Persist updated profile
          (ide-common-env-set-profile ide-common-env-current-project
                                      ide-common-env-current-profile vars)
          (ide-common-env-refresh)))))))

(defun ide-common-env-add ()
  "Add a new environment variable to the current profile."
  (interactive)
  (let* ((name (read-string "Variable name: "))
         (value (read-string "Value: "))
         (vars (ide-common-env-get-profile ide-common-env-current-project
                                           ide-common-env-current-profile)))
    (push (cons name value) vars)
    (ide-common-env-set-profile ide-common-env-current-project
                                ide-common-env-current-profile
                                vars)
    (ide-common-env-refresh)))

(defun ide-common-env-delete ()
  "Delete the environment variable at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (vars (ide-common-env-get-profile ide-common-env-current-project
                                           ide-common-env-current-profile)))
    (setq vars (assoc-delete-all id vars))
    (ide-common-env-set-profile ide-common-env-current-project
                                ide-common-env-current-profile
                                vars)
    (ide-common-env-refresh)))

(defun ide-common-env-change-profile ()
  "Switch to another profile for the current project."
  (interactive)
  (let ((new-profile (ide-common-env-select-profile ide-common-env-current-project)))
    (setq ide-common-env-current-profile new-profile)
    (message "Switched to profile: %s" new-profile)
    (rename-buffer (format "*Env: %s [%s]*"
                           (f-filename ide-common-env-current-project)
                           ide-common-env-current-profile) t)
    (ide-common-env-refresh)))

(defun ide-common-env-clone-profile ()
  "Clone the profile currently open in the editor buffer and switch to it."
  (interactive)
  (unless ide-common-env-current-project
    (user-error "No project in current buffer"))
  (let* ((root ide-common-env-current-project)
         (source ide-common-env-current-profile)
         (default (concat source "-copy"))
         (dest (read-string (format "New profile name (default %s): " default) nil nil default)))
    (when (s-blank? dest)
      (user-error "Profile name cannot be empty"))
    (when (and (assoc dest (ide-common-env-get-profiles root))
               (not (yes-or-no-p (format "Profile %s exists.  Overwrite? " dest))))
      (user-error "Clone aborted"))
    ;; clone the profile
    (ide-common-env-set-profile root dest (copy-alist (ide-common-env-get-profile root source)))
    ;; update last used and current profile
    (ide-common-env-set-last-profile root dest)
    (setq ide-common-env-current-profile dest)
    (message "Cloned and switched to profile %s → %s" source dest)
    (rename-buffer (format "*Env: %s [%s]*"
                           (f-filename root)
                           ide-common-env-current-profile) t)
    (ide-common-env-refresh)
    dest))

(defun ide-common-env-rename-profile ()
  "Rename the profile currently open in the editor buffer."
  (interactive)
  (unless ide-common-env-current-project
    (user-error "No project in current buffer"))
  (let* ((root ide-common-env-current-project)
         (source ide-common-env-current-profile)
         (dest (read-string (format "Rename profile %s to: " source))))
    (when (s-blank? dest)
      (user-error "Profile name cannot be empty"))
    (when (assoc dest (ide-common-env-get-profiles root))
      (user-error "Profile %s already exists" dest))

    ;; rename: clone source to dest and then delete source
    (ide-common-env-set-profile root dest (copy-alist (ide-common-env-get-profile root source)))
    (ide-common-env-unset-profile root source)

    ;; update last used and current profile
    (ide-common-env-set-last-profile root dest)
    (setq ide-common-env-current-profile dest)
    (message "Renamed profile %s → %s" source dest)
    (rename-buffer (format "*Env: %s [%s]*"
                           (f-filename root)
                           ide-common-env-current-profile) t)
    (ide-common-env-refresh)
    dest))

(defun ide-common-env-delete-profile ()
  "Delete the profile currently open in the editor buffer."
  (interactive)
  (unless ide-common-env-current-project
    (user-error "No project in current buffer"))
  (let* ((root ide-common-env-current-project)
         (source ide-common-env-current-profile))
    (when (yes-or-no-p (format "Are you sure you want to delete %s? " source))

      ;; remove the profile
      (ide-common-env-unset-profile root source)

      ;; select next profile
      (setq ide-common-env-current-profile (ide-common-env-select-profile root))

      ;; update last used and current profile
      (ide-common-env-set-last-profile root ide-common-env-current-profile)
      (message "Deleted and switched to profile %s → %s" source ide-common-env-current-profile)
      (rename-buffer (format "*Env: %s [%s]*"
                             (f-filename root)
                             ide-common-env-current-profile) t)
      (ide-common-env-refresh))
    ide-common-env-current-profile))

(defun ide-common-env-delete-all-profiles ()
  "Delete all profiles for current project and create new default profile."
  (interactive)
  (unless ide-common-env-current-project
    (user-error "No project in current buffer"))
  (let* ((root ide-common-env-current-project))
    (when (yes-or-no-p (format "Are you sure you want to delete all profiles for %s? " (f-filename root)))

      ;; delete all profiles
      (ide-common-env-unset-all root)

      ;; create and select default profile
      (ide-common-env-set-profile root "default" nil)
      (setq ide-common-env-current-profile "default")

      ;; update last used and current profile
      (ide-common-env-set-last-profile root ide-common-env-current-profile)
      (message "Deleted all profiles, switched to %s" ide-common-env-current-profile)
      (rename-buffer (format "*Env: %s [%s]*"
                             (f-filename root)
                             ide-common-env-current-profile) t)
      (ide-common-env-refresh))
    ide-common-env-current-profile))

(defun ide-common-env-quit ()
  "Quit the environment editor."
  (interactive)
  (quit-window t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; key bindings

(pretty-hydra-define hydra-ide-common-env
  (:title  (format "%s Environment Profiles" (all-the-icons-material "nature"))
          :color teal
          :quit-key "q")
  ("Profiles"
   (("P" ide-common-env-change-profile "Change profile")
    ("R" ide-common-env-rename-profile "Rename profile")
    ("C" ide-common-env-clone-profile "Clone profile")
    ("D" ide-common-env-delete-profile "Delete profile")
    ("C-D" ide-common-env-delete-all-profiles "Delete all profiles"))
   "Variables"
   (("a" ide-common-env-add "Add variable")
    ("e" ide-common-env-edit-value "Edit value")
    ("r" ide-common-env-edit-name "Rename variable")
    ("d" ide-common-env-delete "Delete variable"))
   "Session"
   (("x" ide-common-env-quit "Close" :color blue))))

(defun ide-common-env-setup-keymap ()
  "Define keymap for `ide-common-env-mode'."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e") #'hydra-ide-common-env/body)
    (define-key map (kbd "a") #'ide-common-env-add)
    (define-key map (kbd "e") #'ide-common-env-edit-value)
    (define-key map (kbd "r") #'ide-common-env-edit-name)
    (define-key map (kbd "d") #'ide-common-env-delete)
    (define-key map (kbd "P") #'ide-common-env-change-profile)
    (define-key map (kbd "R") #'ide-common-env-rename-profile)
    (define-key map (kbd "C") #'ide-common-env-clone-profile)
    (define-key map (kbd "D") #'ide-common-env-delete-profile)
    (define-key map (kbd "C-D") #'ide-common-env-delete-all-profiles)
    (define-key map (kbd "q") #'ide-common-env-quit)
    (define-key map (kbd "C-c C-c") #'ide-common-env-quit)
    (define-key map (kbd "C-c C-k") #'ide-common-env-quit)
    map))

(setq ide-common-env-mode-map (ide-common-env-setup-keymap))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end of file

(provide 'ide-common-env)
;;; ide-common-env.el ends here
