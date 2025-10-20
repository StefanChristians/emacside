;;; ide-common.el --- common IDE functionality -*- lexical-binding: t; -*-

;; Version: 0.0.1

;; Copyright (C) 2022, 2024, 2025 Stefan Christians
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; settings and functions to support using EMACS as IDE

;;; Code:

(require 'f)
(require 'ox)
(require 'vc)
(require 'spdx)
(require 'ox-md)
(require 'compile)
(require 'lsp-mode)
(require 'ox-ascii)
(require 'treemacs)
(require 'yasnippet)
(require 'ox-asciidoc)
(require 'string-inflection)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; customizations

(defgroup ide-common nil
  "Common settings for IDE functionality."
  :group 'programming
  :group 'convenience)

(defcustom ide-common-available-vc-backends vc-handled-backends
  "List of backends to control project versions."
  :group 'ide-common
  :type '(repeat symbol))

(defcustom ide-common-default-vc-backend 'Git
  "Default version control backend for new projects."
  :group 'ide-common
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (car (member x ide-common-vc-available-backends))))))

(defcustom ide-common-dontask-vc-backend t
  "Always use default without asking when none-nil."
  :group 'ide-common
  :type '(boolean))

(defcustom ide-common-filename-case "snake"
  "Case in which to create new file names."
  :group 'ide-common
  :type '(choice (const :tag "snake_case" "snake")
                 (const :tag "kebab-case" "kebab")
                 (const :tag "UPPER_CASE" "upper")
                 (const :tag "camelCase" "camel")
                 (const :tag "PascalCase" "pascal")))

(defcustom ide-common-classname-case "pascal"
  "Case in which to create new class names."
  :group 'ide-common
  :type '(choice (const :tag "snake_case" "snake")
                 (const :tag "kebab-case" "kebab")
                 (const :tag "UPPER_CASE" "upper")
                 (const :tag "camelCase" "camel")
                 (const :tag "PascalCase" "pascal")))

(defcustom ide-common-available-licenses spdx-data-license-identifiers
  "List of licenses under which to publish a project."
  :group 'ide-common
  :type '(repeat string))

(defcustom ide-common-default-license "MIT"
  "Default license for new projects."
  :group 'ide-common
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (car (member x ide-common-available-licenses))))))

(defcustom ide-common-dontask-license t
  "Always use default without asking when none-nil."
  :group 'ide-common
  :type '(boolean))

(defcustom ide-common-default-project-parent "~/"
  "Default parent directory in which to create new projects."
  :group 'ide-common
  :type '(directory))

(defcustom ide-common-default-project-name "Project"
  "Default name for new projects.
The name may be appended with a numeric suffix to make it unique."
  :group 'ide-common
  :type '(string))

(defcustom ide-common-default-project-descr
  "TODO: Description."
  "Default description for new Projects."
  :group 'ide-common
  :type '(String))

(defcustom ide-common-default-project-vendor
  user-full-name
  "Default vendor for new Projects."
  :group 'ide-common
  :type '(String))

(defcustom ide-common-default-project-contact
  "TODO: email@example.com"
  "Default contact email for new Projects."
  :group 'ide-common
  :type '(String))

(defcustom ide-common-spdx-id-file ".spdx.id"
  "File name used for caching the project's license SPDX-ID."
  :group 'ide-common
  :type '(string))

(defcustom ide-common-available-text-file-extensions
  '((nil "Text")
    (".txt" "Text")
    (".md" "Markdown")
    (".org" "Emacs Org")
    (".adoc" "Ascii Doc"))
  "List of extensions for text files."
  :group 'ide-common
  :type '(alist :key-type string :value-type string))

(defcustom ide-common-available-license-file-extensions
  '((nil "Text")
    (".txt" "Text"))
  "List of extensions for license files."
  :group 'ide-common
  :type '(alist :key-type string :value-type string))

(defcustom ide-common-default-license-file-base "LICENSE"
  "Default base name for license files."
  :group 'ide-common
  :type '(string))

(defcustom ide-common-default-license-file-extension nil
  "Default extension for license files."
  :group 'ide-common
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x)
             (car (member x
                          (map-keys
                           ide-common-available-license-file-extensions)))))))

(defcustom ide-common-dontask-license-file-extension t
  "Always use default without asking when none-nil."
  :group 'ide-common
  :type '(boolean))

(defcustom ide-common-default-readme-file-base "README"
  "Default base name for readme files."
  :group 'ide-common
  :type '(string))

(defcustom ide-common-default-readme-file-extension ".org"
  "Default extension for readme files."
  :group 'ide-common
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x)
             (car (member x
                          (map-keys
                           ide-common-available-text-file-extensions)))))))

(defcustom ide-common-dontask-readme-file-extension t
  "Always use default without asking when none-nil."
  :group 'ide-common
  :type '(boolean))

(defcustom ide-common-available-version-file-extensions
  '((nil "Text")
    (".txt" "Text"))
  "List of extensions for version files."
  :group 'ide-common
  :type '(alist :key-type string :value-type string))

(defcustom ide-common-default-version-file-base "VERSION"
  "Default base name for version files."
  :group 'ide-common
  :type '(string))

;; tell emacs that VERSION files should be opened in text mode
(add-to-list 'auto-mode-alist (cons (concat "/" (regexp-quote ide-common-default-version-file-base) "\\'") 'text-mode))

(defcustom ide-common-default-version-file-extension nil
  "Default extension for version files."
  :group 'ide-common
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x)
             (car (member x
                          (map-keys
                           ide-common-available-version-file-extensions)))))))

(defcustom ide-common-dontask-version-file-extension t
  "Always use default without asking when none-nil."
  :group 'ide-common
  :type '(boolean))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; macros

(defmacro ide-common-hints-from-alist (hint-list)
  "Return a function to associate a hint from HINT-LIST with a key."
  `(lambda (s) (format " %s" (cadr (assoc s ,hint-list)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utility functions

(defun ide-common-convert-string (text case)
  "Convert TEXT to CASE."
  (cond ((string-equal "snake" case)
         (string-inflection-underscore-function text))
        ((string-equal "kebab" case)
         (string-inflection-kebab-case-function text))
        ((string-equal "upper" case)
         (string-inflection-upcase-function text))
        ((string-equal "camel" case)
         (string-inflection-camelcase-function text))
        ((string-equal "pascal" case)
         (string-inflection-pascal-case-function text))
        ((string-equal "title" case)
         (replace-regexp-in-string
          "_+" " "
          (string-inflection-capital-underscore-function text)))
        (t (string-inflection-underscore-function text))))

(defun ide-common-normalize-string (text)
  "Replace whitespace and punctuation in TEXT with underscores.
Remove leading and trailing underscores and apply snake case."
  (string-inflection-underscore-function
   (replace-regexp-in-string
    "^_\\|_$" ""
    (replace-regexp-in-string
     "[[:punct:][:blank:]_]+" "_"
     text))))

(defun ide-common-normalize-classname (text)
  "Convert TEXT to valid class name."
  (ide-common-convert-string
   (ide-common-normalize-string text) ide-common-classname-case))

(defun ide-common-normalize-filename (text)
  "Convert TEXT to valid file name."
  (ide-common-convert-string
   (ide-common-normalize-string text) ide-common-filename-case))

(defun ide-common-normalize-filename-in-path (path)
  "Convert last part of PATH to valid file name."
  (let ((filename (f-filename path))
        (prefix (f-dirname path)))
    (f-join prefix (ide-common-normalize-filename filename))))

(defun ide-common-default-project-path ()
  "Return default project path for newly created project."
  (let* ((parent ide-common-default-project-parent)
         (name (ide-common-normalize-filename ide-common-default-project-name))
         (path (f-join parent name))
         (counter 0))
    (while (or (f-exists? path)
               (treemacs-is-path (treemacs-canonical-path path) :in-workspace))
      (cl-incf counter)
      (setq path (f-join parent (format "%s%d" name counter))))
    path))

(defun ide-common-suggest-valid-project-path (path)
  "Normalize PATH and ensure uniqueness in treemacs."
  (let* ((parent (f-parent path))
         (new-name (ide-common-normalize-filename (f-filename path)))
         (new-path (f-join parent new-name))
         (counter 0))
    (while (treemacs-is-path (treemacs-canonical-path new-path) :in-workspace)
      (cl-incf counter)
      (setq new-path (f-join parent (format "%s%d" new-name counter))))
    ;; if path exists and is empty, it was probably created by GUI file chooser
    ;; - rename it to new path
    (when (and (not (string-equal path new-path))
               (f-exists? path)
               (not (f-exists? new-path))
               (f-empty? path))
      (f-move path new-path))
    new-path))

(defun ide-common-normalize-projectname (text)
  "Convert TEXT to valid project name."
  (ide-common-convert-string(ide-common-normalize-string text) "title"))

(defun ide-common-active-path (&optional path)
  "Return the active PATH.

If PATH is nil, default to current directory.
If PATH is a file, return parent directory.
If PATH is a directory, return PATH."
  (let* ((path (or path default-directory))
         (path (if (f-directory? path) path (f-parent path))))
    path))

(defun ide-common-relative-path (&optional path)
  "Return active PATH relative to project root directory."
  (f-relative (ide-common-active-path path) (ide-common-get-project-root)))

(defun ide-common-relative-path-if-descendant (candidate ancestor)
  "Return relative path of CANDIDATE if descendant of ANCESTOR.

If CANDIDATE is a descendant of ANCESTOR, return CANDIDATE's path relative
to ANCESTOR.
Otherwise, return absolute path to CANDIDATE."
  (let* ((candidate (f-canonical candidate))
         (ancestor (f-canonical ancestor))
         (is-descendant (f-descendant-of? candidate ancestor)))
    (if is-descendant
        (f-relative candidate ancestor)
      candidate)))

(defun ide-common-locate-dominating-directory (file &optional path)
  "Return the directory containing the dominating FILE for PATH."
  (locate-dominating-file (ide-common-active-path path) file))

(defun ide-common-locate-dominating-file (file &optional path)
  "Return the dominating FILE for PATH."
  (let ((found-dir (ide-common-locate-dominating-directory file path)))
    (when found-dir (f-join found-dir file))))

(defun ide-common-locate-first-dominating-file (files &optional path)
  "Return the first dominating file found which is a descendant of PATH's project root.

FILES list of file names to search
PATH path to search upwards whether it contains one of FILES"
  (let* ((files (if (listp files) files (list files)))
         (path (ide-common-active-path path))
         (project-root (ide-common-get-project-root path)))
    (catch 'found
      (dolist (file files)
        (let ((candidate (ide-common-locate-dominating-file file path)))
          (when (and candidate
                     (f-descendant-of? candidate project-root))
            (throw 'found candidate))))
      nil)))

(defun ide-common-dominating-spdx-id (&optional path)
  "Return the dominating license SPDX-ID for PATH.
Fall back to default license if none found."
  (let ((found-file (ide-common-locate-dominating-file
                     ide-common-spdx-id-file path)))
    (if found-file
        (f-read-text found-file)
      ide-common-default-license)))

(defun ide-common-sort-strings-by-length (strings &optional descending)
  "Sort STRINGS in order of their length.
If DESCENDING is non-nil, sort in descending order."
  (let* ((strings (if (listp strings) strings (list strings))))
    (when (member nil strings)
      (setq strings (delete nil strings)) (push "" strings))
    (if descending
        (seq-sort-by #'length #'> strings)
      (seq-sort-by #'length #'< strings))))

(defun ide-common-get-project-root (&optional path)
  "Return project root for PATH.

If PATH is nil, return project root for `default-directory'."
  (let ((dir (or path default-directory)))
    (let ((default-directory dir))
      (f-canonical (ffip-project-root)))))

(defun ide-common-get-current-context-project-root ()
  "Return project root based on current buffer or Treemacs selection."
  (let ((path
         (or
          ;; Treemacs selected node
          (when (and (fboundp 'treemacs-current-button)
                     (treemacs-current-button))
            (treemacs-button-get (treemacs-current-button) :path))
          ;; Current buffer file
          (when buffer-file-name
            (f-dirname buffer-file-name)))))
    (ide-common-get-project-root path)))

(defun ide-common-is-project-root (&optional file)
  "Return non-nil if FILE is in the project root directory."
  (f-equal? (ide-common-active-path file) (ide-common-get-project-root)))

(defun ide-common-get-project-machine-name ()
  "Return machine-readable name of current project."
  (let* ((path (ide-common-get-project-root))
        (name (f-filename path))
        (normalized (ide-common-normalize-filename name)))
    normalized))

(defun ide-common-get-project-human-name ()
  "Return human-readable name of current project."
  (let* ((machine-name (ide-common-get-project-machine-name))
         (human-name (ide-common-normalize-projectname machine-name)))
    human-name))

(defun ide-common-get-project-class-name ()
  "Return class name representation of current project."
  (let* ((machine-name (ide-common-get-project-machine-name))
         (class-name (ide-common-normalize-classname machine-name)))
    class-name))

(defun ide-common-available-snippets (mode &optional group)
  "Return a list of available snippets for MODE in GROUP."
  (yas-activate-extra-mode mode)
  (let ((templates (yas--all-templates (yas--get-snippet-tables mode)))
        (available))
    (dolist (template templates available)
      (if group
          (when (member group (yas--template-group template))
            (push (yas--template-name template) available))
        (push (yas--template-name template) available)))
    (nreverse available)))

(defun ide-common-has-ancestor (ancestors &optional path)
  "Return first of ANCESTORS which PATH is a descendant of."
  (let ((path (ide-common-relative-path path))
        (ancestor)
        (has-ancestor))
    (loop-for-each ancestor ancestors
      (when (member ancestor (f-split path))
        (setq has-ancestor ancestor)
        (loop-break)))
    has-ancestor))

(defun ide-common-descending-path (ancestors &optional path)
  "Return relative PATH descending from first found ancestor in ANCESTORS."
  (let ((path (ide-common-relative-path path))
        (ancestor (ide-common-has-ancestor ancestors path)))
    (when ancestor
      (s-join f--path-separator (cdr (member ancestor (f-split path)))))))

(defun ide-common-ascending-path (ancestors &optional path)
  "Return relative PATH ascending to first found ancestor in ANCESTORS."
  (let ((path (ide-common-relative-path path))
        (ancestor (ide-common-has-ancestor ancestors path))
        (ascending-path))
    (loop-for-each component (f-split path)
      (setq ascending-path (f-join (or ascending-path "") component))
      (when (string= ancestor component)
        (loop-break)))
    ascending-path))

(defun ide-common-first-file-match (path base extensions)
  "Return the first filename in PATH matching BASE with one of EXTENSIONS, or nil if no file is found."
  (let ((result nil))
    (catch 'found
      (dolist (ext-entry extensions)
        (let* ((ext (car ext-entry))
               (filename (concat base (or ext "")))
               (candidate (expand-file-name filename path)))
          (when (file-exists-p candidate)
            (setq result filename)
            (throw 'found result)))))
    result))

(defun ide-common-convert-org (source ext &optional is-snippet &rest args)
  "Convert Org content in SOURCE to the format indicated by EXT.

EXT md, adoc, org, txt, or nil (same as txt).
IS-SNIPPET if non-nil, SOURCE is a yasnippet key
ARGS optional arguments for filling yasnippet fields"
  (let ((org-text
         (if is-snippet
             ;; Create a real buffer for yasnippet expansion
             (let ((buf (generate-new-buffer "*yas-temp*"))
                   result)
               (unwind-protect
                   (with-current-buffer buf
                     (org-mode)
                     (yas-minor-mode-on)
                     (yas-expand-snippet (yas-lookup-snippet source 'org-mode))
                     ;; fill in placeholders with args
                     (dolist (val args)
                       (cond
                        ;; Nil: leave placeholder, just skip
                        ((null val)
                         (yas-next-field))
                        ;; Otherwise: insert stringified value
                        (t
                         (insert (format "%s" val))
                         (yas-next-field))))
                     (yas-exit-all-snippets)
                     (setq result (buffer-string)))
                 (kill-buffer buf))
               result)
           ;; Otherwise use raw Org text
           source)))
    ;; convert #+title: qualifier to level 1 heading if EXT is "md"
    (when (and ext (string-equal (downcase ext) "md"))
      (setq org-text (replace-regexp-in-string "^#\\+title: " "* " org-text)))
    ;; Strip #+subtitle: qualifier if EXT is "adoc" or "md"
    (when (and ext (member (downcase ext) '("adoc" "md")))
      (setq org-text (replace-regexp-in-string "^#\\+subtitle: " "" org-text)))
    ;; Convert org-text according to EXT
    (pcase (and ext (downcase ext))
      ("md"
       (let ((buf (generate-new-buffer "*Org MD Export*"))
             result)
         (unwind-protect
             (with-current-buffer buf
               (insert org-text)
               (org-mode)
               (org-md-export-as-markdown nil nil nil)
               (setq result (buffer-string)))
           (kill-buffer buf))
         result))
      ("adoc"
       (let ((buf (generate-new-buffer "*Org ASCIIDOC Export*"))
             (org-asciidoc-level-offset 1)
             result)
         (unwind-protect
             (with-current-buffer buf
               (insert org-text)
               (org-mode)
               (org-asciidoc-export-as-asciidoc nil nil nil)
               ;; remove spurious empty title
               (goto-char (point-min))
               (flush-lines "^=  =$")
               ;; remove spurious author
               (goto-char (point-min))
               (flush-lines (format "^\\s-*%s" (regexp-quote user-full-name)))
               (setq result (buffer-string)))
           (kill-buffer buf))
         result))
      ("org" org-text)
      (_
       (let ((buf (generate-new-buffer "*Org ASCII Export*"))
             result)
         (unwind-protect
             (with-current-buffer buf
               (insert org-text)
               (org-mode)
               (org-ascii-export-as-ascii nil nil nil nil)
               ;; remove spurious author
               (goto-char (point-min))
               (flush-lines (format "^\\s-*%s" (regexp-quote user-full-name)))
               (setq result (buffer-string)))
           (kill-buffer buf))
         result)))))

(defun ide-common-read-file (file)
  "Read object from FILE, or nil."
  (when (f-exists? file)
    (with-temp-buffer
      (insert-file-contents file)
      (read (current-buffer)))))

(defun ide-common-write-file (file data)
  "Write DATA to FILE."
  (f-mkdir (f-dirname file))
  (with-temp-file file
    (prin1 data (current-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; user interaction

(defun ide-common-list-all-the-icons-material ()
  "Display all material icons ."
  (interactive)
  (require 'all-the-icons)
  (let* ((icons (symbol-value 'all-the-icons-data/material-icons-alist))
         (half (ceiling (/ (length icons) 2.0)))
         (left (cl-subseq icons 0 half))
         (right (cl-subseq icons half))
         (col-width 36)) ;; adjust for spacing between columns
    (with-current-buffer (get-buffer-create "*Material Icons*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Material Icons (%d total)\n\n" (length icons)))
      (dotimes (i half)
        (let* ((l (nth i left))
               (r (nth i right))
               (left-str (format "%s  %-25s" (cdr l) (car l)))
               (right-str (when r (format "%s  %-25s" (cdr r) (car r)))))
          ;; pad left column manually
          (insert (format "%s%s\n"
                          (truncate-string-to-width left-str col-width nil ?\s)
                          (or right-str "")))))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

(defun ide-common-maybe-prompt (prompt value default
                                       &optional use-defaults dont-ask
                                       collection restrict
                                       hints)
  "Prompt user or use defaults as necessary.

PROMPT user prompt
VALUE pre-selected value
DEFAULT value to return if prompting is not allowed
USE-DEFAULTS non-nil if defaults should be used instead of prompting user
DONT-ASK do not prompt even if USE-DEFAULTS is non-nil
COLLECTION simple list of possible selections
RESTRICT non-nil if value requires a match in COLLECTION or HINTS
HINTS annotation-function to resolve hints for completion candidates

If VALUE is non-nil, return VALUE.
Otherwise return DEFAULT or prompt user, depending on USE-DEFAULTS."
  ;; return pre-selected value if defined
  (or value
      (if (or use-defaults dont-ask)
          ;; return default value if prompting is prohibited
          default
        (if hints
            ;; use hints alist
            (let ((completion-extra-properties
                   (list :annotation-function hints)))
              (completing-read prompt collection
                               nil restrict nil nil default t))
          (if collection
              ;; use collection simple list
              (completing-read prompt collection nil restrict nil nil default t)
            ;; free prompt without selection
            (read-string prompt default nil default t))))))

(defun ide-common-maybe-prompt-for-project-path (path use-defaults)
  "Prompt for project path or use defaults as necessary.

PATH pre-selected project path
USE-DEFAULTS non-nil if default values should be used

If PATH is non-nil, return PATH.
Otherwise return default value or prompt user, depending on USE-DEFAULTS."
  (or path
      (if use-defaults
          (ide-common-default-project-path)
        (ide-common-suggest-valid-project-path
         (read-directory-name "Project root directory: "
                              ide-common-default-project-parent nil nil nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; project scaffolding

(cl-defun ide-common-create-project (&rest args &key path name descr
                                           vendor contact
                                           license license-ext
                                           readme-ext version-ext
                                           vc
                                           use-defaults &allow-other-keys)
  "Create a new project and register it in treemacs.

PATH path to project root
NAME project name
DESCR short description of project
VENDOR project vendor
CONTACT project vendor contact email
LICENSE SPDX license ID
LICENSE-EXT license file extension
README-EXT readme file extension
VERSION-EXT version file extension
VC version control backend
USE-DEFAULTS if non-nil, use defaults, otherwise prompt user
ARGS list of arguments"
  (let* ((path (ide-common-maybe-prompt-for-project-path path use-defaults))
         (name (ide-common-maybe-prompt
                "Project name: " name
                (ide-common-normalize-projectname (f-filename path))
                use-defaults))
         (descr (ide-common-maybe-prompt
                 "Project description: " descr
                 ide-common-default-project-descr
                 use-defaults))
         (vendor (ide-common-maybe-prompt
                 "Project vendor: " vendor
                 ide-common-default-project-vendor
                 use-defaults))
         (contact (ide-common-maybe-prompt
                 "Project vendor contact: " contact
                 ide-common-default-project-contact
                 use-defaults))
         (license (ide-common-maybe-prompt
                   "License: " license
                   ide-common-default-license
                   use-defaults ide-common-dontask-license
                   ide-common-available-licenses t))
         (license-ext (ide-common-maybe-prompt
                       "License file extension: " license-ext
                       ide-common-default-license-file-extension
                       use-defaults ide-common-dontask-license-file-extension
                       ide-common-available-license-file-extensions t
                       (ide-common-hints-from-alist
                        ide-common-available-license-file-extensions)))
         (license-file (concat
                        ide-common-default-license-file-base license-ext))
         (readme-ext (ide-common-maybe-prompt
                      "Readme file extension: " readme-ext
                      ide-common-default-readme-file-extension
                      use-defaults ide-common-dontask-readme-file-extension
                      ide-common-available-text-file-extensions t
                      (ide-common-hints-from-alist
                       ide-common-available-text-file-extensions)))
         (readme-file (concat ide-common-default-readme-file-base readme-ext))
         (version-ext (ide-common-maybe-prompt
                       "Version file extension: " version-ext
                       ide-common-default-version-file-extension
                       use-defaults ide-common-dontask-version-file-extension
                       ide-common-available-version-file-extensions t
                       (ide-common-hints-from-alist
                        ide-common-available-version-file-extensions)))
         (version-file (concat
                        ide-common-default-version-file-base version-ext))
         (vc (ide-common-maybe-prompt
              "Version control: " vc
              ide-common-default-vc-backend
              use-defaults ide-common-dontask-vc-backend
              ide-common-available-vc-backends t)))
    (progn
      ;; create project directory if it does not exist
      (f-mkdir-full-path path)
      ;; add project directory to treemacs
      (treemacs-do-add-project-to-workspace path name)
      ;; add project directory to lsp
      (lsp-workspace-folders-add path)
      ;; initialize with version control to mark project root
      (ide-common-initialize-version-control path vc)
      ;; create license files
      (ide-common-create-spdx-id-file path license)
      (ide-common-create-license-file path license-file)
      ;; create version file
      (ide-common-create-version-file path version-file)
      ;; return arguments
      (setf (cl-getf args :use-defaults) use-defaults)
      (setf (cl-getf args :path) path)
      (setf (cl-getf args :orig-path) path)
      (setf (cl-getf args :name) name)
      (setf (cl-getf args :descr) descr)
      (setf (cl-getf args :vendor) vendor)
      (setf (cl-getf args :contact) contact)
      (setf (cl-getf args :license) license)
      (setf (cl-getf args :license-ext) license-ext)
      (setf (cl-getf args :license-file) license-file)
      (setf (cl-getf args :readme-ext) readme-ext)
      (setf (cl-getf args :readme-file) readme-file)
      (setf (cl-getf args :version-ext) version-ext)
      (setf (cl-getf args :version-file) version-file)
      (setf (cl-getf args :vc) vc)
      args)))

(defun ide-common-initialize-version-control (path vc)
  "Initialize version control with VC backend in PATH.
Paths initialized with version control are recognized as projects."
  (let ((old-vc (vc-responsible-backend path t)))
    (if old-vc
        (message "%s is already under version control by %s, skipping"
                 path old-vc)
      (let ((default-directory path))
        ;; initialize repository
        (vc-create-repo vc)
        ;; backend specific configuration
        (pcase vc
          ('Git
           (let ((template-path (f-join path ".gitmessage.txt"))
                 (hook-path (f-join path ".git" "hooks" "commit-msg")))
             ;; commit template (user guidance)
             (find-file template-path)
             (text-mode)
             (yas-expand-snippet (yas-lookup-snippet "gitmessage" 'text-mode))
             (yas-exit-all-snippets)
             (save-buffer)
             (kill-buffer)
             (let ((default-directory path))
               (call-process "git" nil nil nil "config" "commit.template" (f-filename template-path)))
             ;; commit hook
             (find-file hook-path)
             (python-mode)
             (yas-expand-snippet (yas-lookup-snippet "commit-msg" 'python-mode))
             (yas-exit-all-snippets)
             (save-buffer)
             (kill-buffer)
             (set-file-modes hook-path #o755))))))))

(defun ide-common-initial-commit (&optional path)
  "Perform initial commit in PATH."
  (let* ((path (or path default-directory))
         (default-directory path)
         (vc (vc-responsible-backend path t)))
    (unless vc
      (error "No VCS repository detected in %s. Cannot perform initial commit" path))

    (pcase vc
      ('Git
       (call-process "git" nil nil nil "add" "-A")
       (call-process "git" nil nil nil "commit" "-m" "chore(release): v0.0.0")
       (call-process "git" nil nil nil "tag" "v0.0.0")
       (call-process "git" nil nil nil "checkout" "-b" "dev"))
      ('CVS
       ;; CVS tags individual files, so use VERSION file for tagging
       ;; CVS does not support Git-style branching
       (dolist (file (directory-files-recursively path ".*" t))
         (unless (vc-backend file)
           (call-process "cvs" nil nil nil "add" file)))
       (call-process "cvs" nil nil nil "commit" "-m" "chore(release): v0.0.0")
       (let ((versionfile (ide-common-first-file-match path ide-common-default-version-file-base ide-common-available-version-file-extensions)))
         (call-process "cvs" nil nil nil "tag" "-F" "v0.0.0" versionfile)))
      ('SVN
       (call-process "svn" nil nil nil "add" "--force" ".")
       (call-process "svn" nil nil nil "commit" "-m" "chore(release): v0.0.0")
       (call-process "svn" nil nil nil "copy" "." "^/tags/v0.0.0" "-m" "Tag v0.0.0")
       (call-process "svn" nil nil nil "copy" "." "^/branches/dev" "-m" "Create dev branch"))
      ('Hg
       (call-process "hg" nil nil nil "addremove")
       (call-process "hg" nil nil nil "commit" "-m" "chore(release): v0.0.0")
       (call-process "hg" nil nil nil "tag" "v0.0.0")
       (call-process "hg" nil nil nil "branch" "dev")
       (call-process "hg" nil nil nil "commit" "-m" "Start dev branch"))
      ('Bzr
       (call-process "bzr" nil nil nil "add")
       (call-process "bzr" nil nil nil "commit" "-m" "chore(release): v0.0.0")
       (call-process "bzr" nil nil nil "tag" "v0.0.0")
       (call-process "bzr" nil nil nil "branch" "." "dev"))
      ('Mtn
       ;; Monotone does not support Git-style branching
       (call-process "mtn" nil nil nil "add" ".")
       (call-process "mtn" nil nil nil "commit" "-m" "chore(release): v0.0.0")
       (call-process "mtn" nil nil nil "tag" "v0.0.0"))
      (_ (error "Unsupported VC backend: %s" vc)))))

(defun ide-common-create-spdx-id-file (path license)
  "Write SPDX LICENSE ID to PATH."
  (let ((file-path (f-join path ide-common-spdx-id-file)))
    (if (f-exists? file-path)
        (message "%s already exists, skipping" file-path)
      (f-write-text license 'utf-8 file-path))))

(defun ide-common-create-license-file (path &optional license-file)
  "Create LICENSE-FILE in PATH."
  (let* ((license-file (or
                        license-file
                        (concat ide-common-default-license-file-base
                                ide-common-default-license-file-extension)))
         (file-path (f-join path license-file)))
    (if (f-exists? file-path)
        (message "%s already exists, skipping" file-path)
      (progn
        (find-file file-path)
        (yas-exit-all-snippets)
        (save-buffer)
        (kill-buffer)))))

(defun ide-common-create-version-file (path &optional version-file)
  "Create VERSION-FILE in PATH."
  (let* ((version-file (or
                        version-file
                        (concat ide-common-default-version-file-base
                                ide-common-default-version-file-extension)))
         (file-path (f-join path version-file)))
    (if (f-exists? file-path)
        (message "%s already exists, skipping" file-path)
      (progn
        (find-file file-path)
        (yas-exit-all-snippets)
        (save-buffer)
        (kill-buffer)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; auto-inserts

(defun ide-common-register-auto-inserts (filenames extensions actions)
  "Register `auto-insert' actions for newly created files.
FILENAMES list of base names
EXTENSIONS list of filename extensions
ACTIONS list of actions to execute"
  (let ((filenames (ide-common-sort-strings-by-length (or filenames "")))
        (extensions (ide-common-sort-strings-by-length (or extensions "")))
        (actions (vconcat (if (listp actions) actions (list actions)))))
    (dolist (filename filenames)
      (dolist (extension extensions)
        (let ((extension (concat (string-replace "." "\\." extension) "\\'")))
          (define-auto-insert (concat filename extension) actions))))))

(defun ide-common-auto-insert-license ()
  "Auto-insert text in new license files."
  (yas-expand-snippet (yas-lookup-snippet
                       (s-downcase (ide-common-dominating-spdx-id))
                       'text-mode t)))

(ide-common-register-auto-inserts
 ide-common-default-license-file-base
 (map-keys ide-common-available-license-file-extensions)
 'ide-common-auto-insert-license)

(defun ide-common-auto-insert-version ()
  "Auto-insert text in new version files."
  (yas-expand-snippet (yas-lookup-snippet "version" 'text-mode t)))

(ide-common-register-auto-inserts
 ide-common-default-version-file-base
 (map-keys ide-common-available-version-file-extensions)
 'ide-common-auto-insert-version)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; action

(defun ide-common-yassify ()
  "Escape current buffer content to serve as YAS snippet template."
  ;; actually YAS should do this automatically when creating new
  ;; snippet from selected region, but they only seem to escape
  ;; the $ sign, so it does not always work
  (interactive)
  (progn
    ;; existing escapes become double escapes
    (replace-regexp-in-region "\\\\" "\\\\\\\\" (point-min) (point-max))
    ;; escape dollar
    (replace-regexp-in-region "\\$" "\\\\$" (point-min) (point-max))
    ;; escape braces
    (replace-regexp-in-region "{" "\\\\{" (point-min) (point-max))
    (replace-regexp-in-region "}" "\\\\}" (point-min) (point-max))
    ;; escape backticks
    (replace-regexp-in-region "`" "\\\\`" (point-min) (point-max))))

(defun ide-common-insert-snippet (snippet &optional mode)
  "Insert SNIPPET at point of current buffer in MODE."
  (let ((mode (or mode major-mode)))
    (progn
      (yas-minor-mode-on)
      (yas-activate-extra-mode mode)
      (yas-expand-snippet (yas-lookup-snippet snippet mode t))
      (yas-exit-all-snippets))))

(declare-function ide-common-env-load-as-list ide-common-env (project-root &optional profile))
(declare-function ide-common-args-select-and-edit ide-common-args (project-root command &optional profile))
(declare-function ide-common-args-load-as-shell-string ide-common-args (project-root command &optional profile))

(defun ide-common-run-compile (project-root command program base-args do-prompt)
  "Run a none-interactive compile command and log output to compilation buffer.

PROJECT-ROOT project root directory (key for selecting environment profile)
COMMAND name of command (key for selecting arguments profile)
PROGRAM  path of executable to run
BASE-ARGS list of base arguments with which the program is always run
DO-PROMPT if none-nil, prompt user for arguments profile,
          otherwise last selected profile for COMMAND is used"
  (let ((default-directory project-root)
        (process-environment (append (ide-common-env-load-as-list project-root) process-environment))
        (compilation-buffer-name-function (lambda (_) (format "*compile %s*" (f-filename project-root))))
        (compilation-scroll-output 'first-error))
    (when do-prompt
      (ide-common-args-select-and-edit command project-root))
    (let ((args1 (mapconcat #'shell-quote-argument base-args " "))
          (args2 (ide-common-args-load-as-shell-string project-root command)))
      (compile (concat program
                       (when args1 (concat " " args1))
                       (when args2 (concat " " args2)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; extensions

(use-package ide-common-env)
(use-package ide-common-args)
(use-package ide-common-debug
  :demand
  :bind (("C-c d" . hydra-ide-common-debug/body)))
(use-package ide-common-launch)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; bootstrap

;; remove other project types from treemacs menu
(with-eval-after-load 'treemacs-mouse-interface
  (setq treemacs--mouse-project-list-functions nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end of file

(provide 'ide-common)
;;; ide-common.el ends here
