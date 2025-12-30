;;; ide-cpp.el --- support for cpp projects -*- lexical-binding: t; -*-

;; Version: 0.0.1

;; Copyright (C) 2022, 2023, 2024, 2025 Stefan Christians
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; settings and functions to support working with cpp projects

;;; Code:

(require 'f)
(require 's)
(require 'dash)
(require 'json)
(require 'loop)
(require 'cl-lib)
(require 'dap-mode)
(require 'yasnippet)
(require 'major-mode-hydra)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; customizations

(defgroup ide-cpp nil
  "C++ settings for IDE functionality."
  :group 'programming
  :group 'convenience
  :group 'ide-cpp)

(defcustom ide-cpp-available-project-types
  '(("simple"
     "Simple project in single source directory")
    ("application"
     "Standard project with separate core and application directories")
    ("library"
     "Library to be used by other projects")
    ("modular"
     "Large project consisting of sub-modules"))
  "List of project types that can be created."
  :group 'ide-cpp
  :type '(alist :key-type string :value-type string)
  :options '("simple" "application" "library" "modular"))

(defcustom ide-cpp-default-project-type "application"
  "Default type of project to create."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x)
             (car (member x (map-keys ide-cpp-available-project-types)))))))

(defcustom ide-cpp-dontask-project-type nil
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-code-formats
  '("Chromium" "GNU" "Google" "LLVM" "Microsoft" "Mozilla" "Webkit")
  "List of code styles available for formatting."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-code-format "LLVM"
  "The default code style to format new  projects."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (car (member x ide-cpp-available-code-formats))))))

(defcustom ide-cpp-dontask-code-format t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-c-standards '("89" "99" "11" "17" "2x")
  "List of C standards."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-c-standard "17"
  "Default C standard."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (car (member x ide-cpp-available-c-standards))))))

(defcustom ide-cpp-dontask-c-standard t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-cpp-standards '("98" "11" "14" "17" "20" "2b")
  "List of C++ standards."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-cpp-standard "20"
  "Default C++ standard."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (car (member x ide-cpp-available-cpp-standards))))))

(defcustom ide-cpp-dontask-cpp-standard t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-module-base-directories
  '("lib" "libs" "library" "libraries" "extra" "extras")
  "List of possible module base directory names."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-module-base-directory "libs"
  "Default module base directory name."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x)
             (car (member x ide-cpp-available-module-base-directories))))))

(defcustom ide-cpp-dontask-module-base-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-default-module-lib-submodule-name "base"
  "Name to identify the main library submodule of a modular project."
  :group 'ide-cpp
  :type '(string))

(defcustom ide-cpp-dontask-module-lib-submodule-name t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-include-directories
  '("inc" "incl" "include" "includes")
  "List of possible public include directory names."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-include-directory "include"
  "Default include directory name."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (car (member x ide-cpp-available-include-directories))))))

(defcustom ide-cpp-dontask-include-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-source-directories '("src" "source" "sources")
  "List of possible source directory names."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-source-directory "src"
  "Default source directory name."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (car (member x ide-cpp-available-source-directories))))))

(defcustom ide-cpp-dontask-source-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-core-directories '("core" "lib" "library")
  "List of possible core directory names."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-core-directory "core"
  "Default core directory name."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (car (member x ide-cpp-available-core-directories))))))

(defcustom ide-cpp-dontask-core-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-app-directories
  '("app" "apps" "application" "applications"
    "ui" "user-interface" "user_interface")
  "List of possible application directory names."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-app-directory "app"
  "Default application directory name."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (car (member x ide-cpp-available-app-directories))))))

(defcustom ide-cpp-dontask-app-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-default-ui "cli"
  "Default application user interface name."
  :group 'ide-cpp
  :type '(string))

(defcustom ide-cpp-dontask-ui t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-test-directories
  '("tst" "test" "tests"
    "unit_tests" "unit-tests"
    "testsuite" "test-suite" "test_suite")
  "List of possible test directory names."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-test-directory "test"
  "Default test directory name."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (car (member x ide-cpp-available-test-directories))))))

(defcustom ide-cpp-dontask-test-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-mock-directories
  '("mock" "mocks" "mockup" "mockups")
  "List of possible mock directory names."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-mock-directory "mocks"
  "Default mock directory name."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (car (member x ide-cpp-available-mock-directories))))))

(defcustom ide-cpp-dontask-mock-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-userdoc-directories
  '("doc" "docs" "documentation"
    "userdoc" "userdocs"
    "user-doc" "user-docs" "user-documentation"
    "user_doc" "user_docs" "user_documentation")
  "List of possible directory names for user documentation sources."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-userdoc-directory "docs"
  "Default directory name for user documentation sources."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x)
             (car (member x ide-cpp-available-userdoc-directories))))))

(defcustom ide-cpp-dontask-userdoc-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-default-userdoc-file-base "index"
  "Default base name for user documentation files."
  :group 'ide-cpp
  :type '(string))

(defcustom ide-cpp-available-userdoc-file-extensions
  '((".adoc" "Ascii Doc"))
  "List of extensions for user documentation files."
  :group 'ide-cpp
  :type '(alist :key-type string :value-type string))

(defcustom ide-cpp-default-userdoc-file-extension ".adoc"
  "Default extension for user documentation files."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x)
             (car (member x
                          (map-keys
                           ide-cpp-available-userdoc-file-extensions)))))))

(defcustom ide-cpp-dontask-userdoc-file-extension t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-localize-directories '("lang" "langs" "languages" "locale" "loc" "locales" "locs" "localization" "localizations" "po" "trl" "translations")
  "List of possible localization directory names."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-localize-directory "locs"
  "Default localization directory name."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (car (member x ide-cpp-available-localize-directories))))))

(defcustom ide-cpp-dontask-localize-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-default-build-directory "build"
  "Default directory for out-of-source builds."
  :group 'ide-cpp
  :type '(string))

(defcustom ide-cpp-dontask-build-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-default-install-directory "install"
  "Default directory for installing binaries."
  :group 'ide-cpp
  :type '(string))

(defcustom ide-cpp-dontask-install-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-default-pack-directory "distr"
  "Default directory for distributing binary packages."
  :group 'ide-cpp
  :type '(string))

(defcustom ide-cpp-dontask-pack-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-cmake-directories
  '("cmake"
    "modules" "mods" "mod"
    "cmake-modules" "cmake-mods" "cmake-mod"
    "cmake_modules" "cmake_mods" "cmake_mod"
    "cmake/modules" "cmake/mods" "cmake/mod")
  "List of possible directory names for CMake modules."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-cmake-directory "cmake"
  "Default directory for cmake modules."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x)
             (car (member x (map-keys ide-cpp-available-cmake-directories)))))))

(defcustom ide-cpp-dontask-cmake-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-default-tools-directory "tools"
  "Default directory for development scripts."
  :group 'ide-cpp
  :type '(string))

(defcustom ide-cpp-dontask-tools-directory t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-default-c-header-extension ".h"
  "Default C header file extension."
  :group 'ide-cpp
  :type '(string))

(defcustom ide-cpp-dontask-c-header-extension t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-cpp-header-extensions '(".h" ".hh" ".hpp" ".hxx")
  "List of possible C++ header file extensions."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-cpp-header-extension ".hpp"
  "Default C++ header file extension."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x)
             (car (member x ide-cpp-available-cpp-header-extensions))))))

(defcustom ide-cpp-dontask-cpp-header-extension t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-default-c-source-extension ".c"
  "Default C source file extension."
  :group 'ide-cpp
  :type '(string))

(defcustom ide-cpp-dontask-c-source-extension t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-cpp-source-extensions '(".c" ".cc" ".cpp" ".cxx")
  "List of possible C++ source file extensions."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-cpp-source-extension ".cpp"
  "Default C++ source file extension."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x)
             (car (member x ide-cpp-available-cpp-source-extensions))))))

(defcustom ide-cpp-dontask-cpp-source-extension t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-default-cmake-dynamic-config-extension ".in"
  "Default CMake dynamic configuration file extension."
  :group 'ide-cpp
  :type '(string))

(defcustom ide-cpp-dontask-cmake-dynamic-config-extension t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-dynamic-config-excludes '("onfig\.cmake")
  "List of regexes of filenames to exclude from dynamic configuration.
They are handled separately."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-available-test-suffixes
  '(".test" "-test" "_test" ".tst" "-tst" "_tst")
  "List of possible test file suffixes."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-test-suffix "_test"
  "Default test file suffix."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x)
             (car (member x ide-cpp-available-test-suffixes))))))

(defcustom ide-cpp-dontask-test-sufix t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))

(defcustom ide-cpp-available-mock-suffixes '(".mock" "-mock" "_mock")
  "List of possible mock file suffixes."
  :group 'ide-cpp
  :type '(repeat string))

(defcustom ide-cpp-default-mock-suffix "_mock"
  "Default mock file suffix."
  :group 'ide-cpp
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x)
             (car (member x ide-cpp-available-mock-suffixes))))))

(defcustom ide-cpp-dontask-mock-sufix t
  "Always use default without asking when none-nil."
  :group 'ide-cpp
  :type '(boolean))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ephemeral cache

;; current generator type
(defvar ide-cpp-multi-config-cache (make-hash-table :test #'equal)
  "Ephemeral cache of multi-config state per project root.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; current generator type getters and setters

(defun ide-cpp-multi-config-p ()
  "Return non-nil if a multi-config generator is being used.
Uses an ephemeral per-project cache."
  (when-let ((root (ide-common-get-current-context-project-root)))
    (or (gethash root ide-cpp-multi-config-cache)
        (let ((value (ide-cpp-is-multi-config)))
          (puthash root value ide-cpp-multi-config-cache)
          value))))

(defun ide-cpp-warm-project-cache ()
  "Precompute project state for the current buffer."
  (when (ide-cpp-buffer-is-cmake-project)
    (ide-cpp-multi-config-p)))
(add-hook 'find-file-hook #'ide-cpp-warm-project-cache)
(add-hook 'after-change-major-mode-hook #'ide-cpp-warm-project-cache)

(defun ide-cpp-invalidate-multi-config (&optional root)
  "Invalidate cached multi-config state.
If ROOT is nil, invalidate the current project."
  (let ((root (or root (ide-common-get-project-root))))
    (when root
      (remhash root ide-cpp-multi-config-cache))))

;; users can call this function after externally re-initializing
;; the project with a different generator
(defun ide-cpp-refresh-project-state ()
  "Refresh cached project state after external reconfiguration."
  (interactive)
  (clrhash ide-cpp-multi-config-cache)
  (message "IDE C++ project state refreshed"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; persistent cache

;; current build tree cache
(defvar ide-cpp-buildtree-current nil
  "Mapping of currently used buildtree to project.

Alist of (PROJECT . BUILDTREE) pairs.

\((project1 . buildtree1) (project2 . buildtree2))")

(defvar ide-cpp-buildtree-current-file
  (f-join user-emacs-directory ".cache" "ide-cpp-buildtree.eld")
  "Path to file used for current buildtree cache.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cache maintenance

(defun ide-cpp-load-all ()
  "Load all C++ caches from disk."
  (setq ide-cpp-buildtree-current
        (or (ide-common-read-file ide-cpp-buildtree-current-file)
            nil)))

(defun ide-cpp-buildtree-save-cache ()
  "Save last used build tree cache to disk."
  (ide-common-write-file ide-cpp-buildtree-current-file
                         ide-cpp-buildtree-current))

(defun ide-cpp-save-all ()
  "Save all C++ caches to disk."
  (ide-cpp-buildtree-save-cache))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; last build tree getters and setters

(defun ide-cpp-buildtree-get-cache (project-root)
  "Return last build tree for PROJECT-ROOT."
  (alist-get (f-slash project-root) ide-cpp-buildtree-current nil nil #'string=))

(defun ide-cpp-buildtree-set-cache (project-root build-dir)
  "Set last used BUILD-DIR for PROJECT-ROOT."
  (let ((build-dir (ide-common-relative-path-if-descendant build-dir project-root)))
    (setf (alist-get (f-slash project-root) ide-cpp-buildtree-current nil nil #'string=)
          build-dir)
    (ide-cpp-buildtree-save-cache)))

(defun ide-cpp-buildtree-unset-cache (project-root)
  "Remove cached build tree for PROJECT-ROOT."
  (setq ide-cpp-buildtree-current
        (assoc-delete-all (f-slash project-root) ide-cpp-buildtree-current #'string=))
  (ide-cpp-buildtree-save-cache))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utility functions

(defun ide-cpp-project-mouse-selection-menu ()
  "Build a mouse selection menu for available project types."
  (--map
   (vector
    (car
     (cdr it))
    (lambda () (interactive)
      (funcall-interactively 'ide-cpp-create-project :type (car it))))
   ide-cpp-available-project-types))

(defun ide-cpp-link-compile-commands (build-tree project-root)
  "Link or copy compile_commands.json into PROJECT-ROOT from BUILD-TREE."
  (let ((src (f-join build-tree "compile_commands.json"))
        (dst (f-join project-root "compile_commands.json")))
    (if (memq system-type '(windows-nt ms-dos))
        ;; Windows: try symlink, fall back to copy
        (condition-case nil
            (make-symbolic-link src dst t)
          (file-error (f-copy src dst)))
      ;; Unix: symlink only
      (make-symbolic-link src dst t))))

(defun ide-cpp-get-build-tree (&optional path default)
  "Return build tree for PATH's project.

If PATH is nil, return build tree for current context's project.
If build tree is unknown, return DEFAULT.
If DEFAULT is also nil, return `ide-cpp-default-build-tree'."
  (let* ((project-root (if path
                           (ide-common-get-project-root path)
                         (ide-common-get-current-context-project-root)))
         (cached (ide-cpp-buildtree-get-cache project-root))
         ;; only reuse cache if directory still exists
         (build-tree (when
                         (and cached
                              (f-exists? (f-join project-root cached)))
                       cached))
         (compile-commands-file (f-join project-root "compile_commands.json"))
         (is-symlink (f-symlink? compile-commands-file)))

    ;; if no usable cache, recompute and update cache
    (unless build-tree
      (setq build-tree
            (let (candidate)
              ;; try to read build directory from JSON
              (when (or is-symlink (f-exists? compile-commands-file))
                (setq candidate
                      (ignore-errors
                        (let* ((json-object-type 'alist)
                               (json-array-type 'list)
                               (json-key-type 'symbol)
                               (entries (json-read-file compile-commands-file))
                               (dirs (delete-dups
                                      (delq nil
                                            (mapcar
                                             (lambda (e)
                                               (let ((file (alist-get 'file e))
                                                     (dir (alist-get 'directory e)))
                                                 (when (and file dir
                                                            (f-descendant-of? file project-root))
                                                   (f-canonical dir))))
                                             entries)))))
                          (cond
                           ;; exactly one build dir → good
                           ((= (length dirs) 1)
                            (car dirs))
                           ;; multiple → too ambiguous
                           ((> (length dirs) 1)
                            (if is-symlink
                                (progn
                                  (message "Multiple build directories found; using symlink target.")
                                  (f-parent (file-chase-links compile-commands-file)))
                              (message "Multiple build directories found; falling back to default.")
                              nil))))))

                ;; fallback: dangling or missing symlink
                (unless candidate
                  (when is-symlink
                    (setq candidate
                          (ignore-errors (f-parent (file-chase-links compile-commands-file))))
                    (when candidate
                      (message "Using build tree from symlink target: %s" candidate))))

                ;; normalize: relative if inside project-root, else absolute
                (when candidate
                  (setq candidate (f-canonical candidate))
                  (setq candidate
                        (if (f-descendant-of? candidate project-root)
                            (f-relative candidate project-root)
                          candidate)))

                (unless candidate
                  (message "Could not determine build tree; using default: %s"
                           (or default ide-cpp-default-build-directory)))
                candidate)))

      ;; update cache
      (ide-cpp-buildtree-set-cache project-root build-tree))

    ;; return cached or computed result
    (or build-tree default ide-cpp-default-build-directory)))

(defun ide-cpp-is-placeholder-file (&optional file)
  "Return non-nil if FILE has a placeholder extension."
  (let* ((file (or file buffer-file-name))
         (ext (f-ext file t)))
    (when (string= ide-cpp-default-cmake-dynamic-config-extension ext) t)))

(defun ide-cpp-real-file-path (&optional file)
  "Return the \"real\" path of FILE.

In file name:
- final cmake dynamic configuration extension (.in) is stripped
- mock suffix is removed \(..._mock.hpp\)
- test suffix is removed \(..._test.cpp\)

In directory name:
- final mock subdirectory \(/mock\) is removed
- test path component \(/test/\) is replaced with source path
  component \(/src/\)
\(values in parentheses are sample default values\)."
  (let* ((file (or file buffer-file-name))
         (filename (f-filename file))
         (path (ide-cpp-unmocked-path file))
         (mock-suffixes
          (ide-common-sort-strings-by-length
           ide-cpp-available-mock-suffixes t))
         (test-suffixes
          (ide-common-sort-strings-by-length
           ide-cpp-available-test-suffixes t))
         (test-components
          (ide-common-sort-strings-by-length
           ide-cpp-available-test-directories t))
         (found))

    ;; file name
    (when (ide-cpp-is-placeholder-file filename)
      (setq filename (f-no-ext filename)))
    (loop-for-each suffix mock-suffixes
      (setq filename (s-replace (format "%s." suffix) "." filename)))
    (loop-for-each suffix test-suffixes
      (setq filename (s-replace (format "%s." suffix) "." filename)))

    ;; directory name
    ;; (is already unmocked in the let-block)
    (loop-for-each component test-components
      (when (s-contains? component path)
        (loop-for-each replacement ide-cpp-available-source-directories
          (when (f-exists? (f-join
                            (ide-common-get-project-root)
                            (s-replace-regexp
                            (format "\\(\\b%s\\)\\(/\\|\\'\\)" component)
                            (format "%s\\2" replacement)
                            path)))
            (setq path (s-replace-regexp
                        (format "\\(\\b%s\\)\\(/\\|\\'\\)" component)
                        (format "%s\\2" replacement)
                        path))
            (setq found t)
            (loop-break))))
      (when found (loop-break)))

    ;; re-assemble path with new components and names
    (f-join (ide-common-get-project-root) path filename)))

(defun ide-cpp-is-public-header (&optional file)
  "Return non-nil if FILE is in a public header directory."
  (ide-common-has-ancestor ide-cpp-available-include-directories file))

(defun ide-cpp-public-header-dirs (&optional file)
  "Return list of public header directories available for FILE."
  (let ((path (ide-common-active-path file))
        (public-header-dirs))
    (progn
      ;; public include directories under project root
      (dolist (subdir ide-cpp-available-include-directories)
        (when (f-directory? (f-join (ide-common-get-project-root) subdir))
          (push (f-join (ide-common-get-project-root) subdir)
                public-header-dirs)))
      ;; public include directories under this submodule
      (if (ide-cpp-is-submodule file)
          (let ((submodule (ide-cpp-submodule-name file)))
            (dolist (moddir ide-cpp-available-module-base-directories)
              (dolist (incdir ide-cpp-available-include-directories)
                (let ((pubdir (f-join (ide-common-get-project-root)
                                      moddir
                                      submodule
                                      incdir)))
                  (when (f-directory? pubdir)
                    (push pubdir public-header-dirs)))))))
      ;; public include directories under this source directory
      (dolist (subdir ide-cpp-available-include-directories)
        (when (f-directory? (f-join path subdir))
          (push (f-join path subdir) public-header-dirs)))
      public-header-dirs)))

(defun ide-cpp-has-public-header (&optional file)
  "Return non-nil if a public header with the same base name of FILE exists."
  (let* ((file (ide-cpp-real-file-path file))
        (basename (f-base file))
        (has-public-header))
    (dolist (pubdir (ide-cpp-public-header-dirs file))
      (dolist (ext ide-cpp-available-cpp-header-extensions)
        (if (or (f-exists? (f-join
                            pubdir
                            (ide-common-get-project-machine-name)
                            (concat basename ext)))
                (f-exists? (f-join
                            pubdir
                            (concat basename ext)))
                (f-exists? (f-join
                            pubdir
                            (ide-common-get-project-machine-name)
                            (concat
                             basename
                             ext
                             ide-cpp-default-cmake-dynamic-config-extension)))
                (f-exists? (f-join
                            pubdir
                            (concat
                             basename
                             ext
                             ide-cpp-default-cmake-dynamic-config-extension))))
            (setq has-public-header t))))
    has-public-header))

(defun ide-cpp-is-active-header (&optional file)
  "Return non-nil if FILE is an active header."
  ;; a public header is always active
  (or (ide-cpp-is-public-header file)
      ;; a private header is active if there is no public header
      (not (ide-cpp-has-public-header file))))

(defun ide-cpp-is-application (&optional file)
  "Return non-nil if FILE is in an application directory."
  (ide-common-has-ancestor ide-cpp-available-app-directories file))

(defun ide-cpp-is-submodule (&optional file)
  "Return non-nil if FILE is in a submodule directory."
  (ide-common-has-ancestor ide-cpp-available-module-base-directories file))

(defun ide-cpp-doxygen-file-type-comment (&optional file)
  "Return doxygen comment describing the type of FILE."
  (let* ((file (ide-cpp-real-file-path file))
         (ext (f-ext file t))
         (visibility (if (ide-cpp-is-public-header file) "public" "private"))
         (type (cond
                ((member ext ide-cpp-available-cpp-header-extensions)
                 "header file")
                ((member ext ide-cpp-available-cpp-source-extensions)
                 "source file")
                (t "")))
         (type (when (member ext ide-cpp-available-cpp-header-extensions)
                 (format "%s %s" visibility type))))
    (when type (format "///\n/// %s\n" type))))

(defun ide-cpp-header-guard-id (&optional file)
  "Return the header guard ID for FILE."
  (let* ((file (ide-cpp-real-file-path file))
         (base (f-base file))
         (ext (f-ext file))
         (project (upcase (ide-common-normalize-classname
                   (ide-common-get-project-machine-name))))
         (path (cond
                ;; public header - no path component
                ((ide-cpp-is-public-header file) "PUBLIC")
                ;; private header under src - descending path from src
                ((ide-common-has-ancestor ide-cpp-available-source-directories
                                          file)
                 (ide-common-descending-path
                  ide-cpp-available-source-directories
                  file))
                ;; header elsewhere - parent name
                (t (f-filename (f-dirname file)))))
         (path (when path (string-replace f--path-separator "_" path)))
         (submodule (when (ide-cpp-is-submodule file)
                      (upcase (ide-common-normalize-classname
                       (ide-cpp-submodule-name))))))
    (ide-common-convert-string
     (if submodule
         (if path
             (format "%s_%s_%s_%s_%s" project submodule path base ext)
           (format "%s_%s_%s_%s" project submodule base ext))
       (if path
           (format "%s_%s_%s_%s" project path base ext)
         (format "%s_%s_%s" project base ext)))
     "upper")))

(defun ide-cpp-unmocked-path (&optional file)
  "Return the relative path for FILE without final mock subdirectory."
  (let ((path (ide-common-relative-path file)))
    (if (member (f-filename path) ide-cpp-available-mock-directories)
        (f-dirname path)
      path)))

(defun ide-cpp-submodule-name (&optional file)
  "Return the child directory of submodule base in path of FILE."
  (let* ((path (ide-common-relative-path file))
         (descending (ide-common-descending-path
                      ide-cpp-available-module-base-directories path)))
    (when descending (car(f-split descending)))))

(defun ide-cpp-namespace (&optional file)
  "Return the namespace for FILE."
  (let ((level1 (ide-common-get-project-machine-name))
        (level2 (ide-cpp-submodule-name file))
        (level3 (f-filename (ide-cpp-unmocked-path file))))
    (cond
     ((and (ide-cpp-is-submodule file) (ide-cpp-is-application file))
      (format "%s::%s::%s" level1 level2 level3))
     ((ide-cpp-is-submodule file) (format "%s::%s" level1 level2))
     ((ide-cpp-is-application file) (format "%s::%s" level1 level3))
     (t level1))))

(defun ide-cpp-snippet-name (part &optional file)
  "Return the snippet name for PART of FILE."
  (let* ((file (ide-cpp-real-file-path file))
         (ext (f-ext file))
         (base (if (string=
                   (f-base file)
                   (if (ide-cpp-is-submodule file)
                       (ide-cpp-submodule-name)
                     (ide-common-get-project-machine-name)))
                  "core"
                (f-base file))))
    (if part
        (format "%s-%s-%s" base ext part)
      (format "%s-%s" base ext))))

(defun ide-cpp-get-private-header-path (&optional file)
  "Return path to private header for FILE if it exists."
  (let* ((file (ide-cpp-real-file-path file))
         (path (f-dirname file))
         (basename (f-base file))
         (private-header-path))
    (loop-for-each ext ide-cpp-available-cpp-header-extensions
      (when
          (or
           (f-file? (f-join path (concat basename ext)))
           (f-file? (f-join path
                            (concat
                             basename ext
                             ide-cpp-default-cmake-dynamic-config-extension))))
        (setq private-header-path (f-join path (concat basename ext)))
        (loop-break)))
    private-header-path))

(defun ide-cpp-get-private-header (&optional file basename force)
  "Return private header for FILE.

If BASENAME is given, use that as header name.
If FORCE is non-nil, return header even if it does not exist."
  (let* ((file (ide-cpp-real-file-path file))
         (path (f-dirname file))
        (basename (or basename (f-base file)))
        (private-header))
    (loop-for-each ext ide-cpp-available-cpp-header-extensions
      (when
          (or
           (f-file? (f-join path (concat basename ext)))
           (f-file? (f-join path
                            (concat
                             basename ext
                             ide-cpp-default-cmake-dynamic-config-extension))))
        (setq private-header (concat basename ext))
        (loop-break)))
    (unless private-header
      (when force
        (setq private-header
              (concat basename ide-cpp-default-c-header-extension))))
    private-header))

(defun ide-cpp-private-include (&optional file force)
  "Include private header for FILE.

If FORCE is non-nil, include header even if it does not exist."
  (let ((private-header (ide-cpp-get-private-header file nil force)))
    (when private-header
      (format "#include \"%s\"\n\n" private-header))))

(defun ide-cpp-get-public-header-path (&optional file)
  "Return path to public header for FILE if it exists."
  (let* ((file (ide-cpp-real-file-path file))
         (basename (f-base file))
         (subdir (if (ide-cpp-is-submodule file)
                     (f-join (ide-common-get-project-machine-name) (ide-cpp-submodule-name file))
                   (ide-common-get-project-machine-name)))
         (paths (ide-cpp-public-header-dirs file))
         (public-header-path)
         (found))
    (loop-for-each path paths
      (loop-for-each ext ide-cpp-available-cpp-header-extensions
        (when
            (or
             (f-file? (f-join path subdir (concat basename ext)))
             (f-file? (f-join
                       path
                       subdir
                       (concat
                        basename ext
                        ide-cpp-default-cmake-dynamic-config-extension))))
          (setq public-header-path (f-join path subdir (concat basename ext)))
          (setq found t)
          (loop-break)))
      (when found (loop-break)))
  public-header-path))

(defun ide-cpp-get-public-header (&optional file basename force)
  "Return public header for FILE.

If BASENAME is given, use that as header name.
If FORCE is non-nil, return header even if it does not exist."
  (let* ((file (ide-cpp-real-file-path file))
         (basename (or basename (f-base file)))
         (subdir (if (ide-cpp-is-submodule file)
                     (f-join (ide-common-get-project-machine-name) (ide-cpp-submodule-name file))
                   (ide-common-get-project-machine-name)))
         (paths (ide-cpp-public-header-dirs file))
         (public-header)
         (found))
    (loop-for-each path paths
      (loop-for-each ext ide-cpp-available-cpp-header-extensions
        (when
            (or
             (f-file? (f-join path subdir (concat basename ext)))
             (f-file? (f-join
                       path
                       subdir
                       (concat
                        basename ext
                        ide-cpp-default-cmake-dynamic-config-extension))))
          (setq public-header (f-join subdir (concat basename ext)))
          (setq found t)
          (loop-break)))
      (when found (loop-break)))
    (unless public-header
      (when force
        (setq public-header
              (f-join subdir (concat basename
                                     ide-cpp-default-cpp-header-extension)))))
    public-header))

(defun ide-cpp-public-include (&optional file force)
  "Include public header for FILE.

If FORCE is non-nil, include header even if it does not exist."
  (let ((public-header (ide-cpp-get-public-header file nil force)))
    (when public-header
      (format "#include \"%s\"\n\n" public-header))))

(defun ide-cpp-get-project-header ()
  "Return the main project header.

Returns the public project header.
If the public project header does not exist, returns the private project header.
If that does not exist either, forces a public or private project header,
depending on the type of project."
  (let* ((file buffer-file-name)
         (basename (ide-cpp-project-or-submodule-name))
         (header))
    (progn
      (setq header (ide-cpp-get-public-header file basename nil))
      (unless header
        (setq header (ide-cpp-get-private-header file basename nil)))
      (unless header
        (setq header (if (ide-cpp-public-header-dirs file)
                         (ide-cpp-get-public-header file basename t)
                       (ide-cpp-get-private-header file basename t))))
      header)))

(defun ide-cpp-get-version-header ()
  "Return the project version header.

Returns the public version header.
If the public version header does not exist, returns the private version header.
If that does not exist either, forces a public or private version header,
depending on the type of project."
  (let* ((file buffer-file-name)
         (basename "version")
         (header))
    (progn
      (setq header (ide-cpp-get-public-header file basename nil))
      (unless header
        (setq header (ide-cpp-get-private-header file basename nil)))
      (unless header
        (setq header (if (ide-cpp-public-header-dirs file)
                         (ide-cpp-get-public-header file basename t)
                       (ide-cpp-get-private-header file basename t))))
      header)))

(defun ide-cpp-class-name (&optional file)
  "Return the class name associated with FILE."
   (let* ((file (ide-cpp-real-file-path file))
          (basename (f-base file)))
     (ide-common-normalize-classname basename)))

(defun ide-cpp-has-non-default-constructor (file class)
  "Return non-nil if a non-default constructor is declared for CLASS in FILE."
  (let ((class-pattern (format "^[[:space:]]*\\(struct\\|class\\).*\\(\\<%s\\>\\)" class))
        (constructor-pattern (format "\\<%s\\>[[:space:]]*(" class))
        (default-constructor-pattern (format "\\<%s\\>[[:space:]]*([[:space:]]*)" class)))

    (message "class match: %S %S" class (s-match class-pattern (f-read-text file)))
    (message "ctor match: %S %S" class (s-match constructor-pattern (f-read-text file)))
    (message "default ctor match: %S %S" class (s-match default-constructor-pattern (f-read-text file)))

      (and (s-match class-pattern (f-read-text file))
           (s-match constructor-pattern (f-read-text file))
           (not (s-match default-constructor-pattern (f-read-text file))))))

(defun ide-cpp-default-constructable-class-in-header (&optional file)
  "Return class declared in FILE's header.

This is for cases where no non-default constructor is declared,
i.e. if the object is constructable with the default constructor.

Return nil if no class is found or if no default constructor is available."
  (let* ((path (ide-cpp-real-file-path file))
         (public (ide-cpp-get-public-header-path file))
         (private (ide-cpp-get-private-header-path file))
         (class (ide-common-normalize-classname (f-base path)))
         (match))
    (progn
      (message "Creating testobject for %S" class)
      (when public
        (setq match
              (s-match
               (format "^[[:space:]]*\\(struct\\|class\\).*\\(\\<%s\\>\\)" class)
               (f-read-text public)))
        (when match
          (if (ide-cpp-has-non-default-constructor public class)
              (setq match nil))))
      (unless match
        (when private
          (setq match
                (s-match
                 (format
                  "^[[:space:]]*\\(struct\\|class\\).*\\(\\<%s\\>\\)" class)
                 (f-read-text private)))
          (when match
            (if (ide-cpp-has-non-default-constructor private class)
                (setq match nil)))))
      (when match (caddr match)))))

(defun ide-cpp-test-declare-member-of-class (&optional file)
  "Declare object of class associated with FILE as test object."
  (let ((class (ide-cpp-default-constructable-class-in-header file)))
    (when class
      (format "  %s testObject;\n" class))))

(defun ide-cpp-used-cmake-directories ()
  "Return a list of existing CMake module directories."
  (let ((module-dirs))
    (dolist (dir ide-cpp-available-cmake-directories module-dirs)
      (when (f-directory? (f-join (ide-common-get-project-root) dir))
        (push dir module-dirs)))
    module-dirs))

(defun ide-cpp-used-cmake-modules ()
  "Return a list of existing CMake modules from existing CMake directories.

Exclude scripts which should not be included as modules."
  (let ((dirs (ide-cpp-used-cmake-directories))
        (known-modules (ide-common-available-snippets 'cmake-mode "module"))
        (excluded-modules (ide-common-available-snippets 'cmake-mode "script"))
        (found-modules)
        (final-modules))
    (progn
      ;; first find all modules
      (loop-for-each dir dirs
        (loop-for-each file (f-files (f-join (ide-common-get-project-root) dir)
                                     (lambda (f) (equal (f-ext f) "cmake"))
                                     nil)
          ;; exclude modules which are known to be scripts
          (unless (member (f-base file) excluded-modules)
            (cl-pushnew (f-base file) found-modules))))
      ;; then use those modules of which we know the load order
      (loop-for-each module known-modules
        (when (member module found-modules)
          (setq final-modules (append final-modules (list module)))
          (setq found-modules (delete module found-modules))))
      ;; finally append any other modules
      (append final-modules found-modules))))

(defun ide-cpp-cmake-subdirs-include ()
  "List of directories to add as subdirs in root CMakeLists.txt."
  (cl-concatenate 'list
                  ide-cpp-available-module-base-directories
                  ide-cpp-available-source-directories
                  ide-cpp-available-core-directories
                  ide-cpp-available-app-directories
                  ide-cpp-available-test-directories
                  ide-cpp-available-userdoc-directories
                  ide-cpp-available-localize-directories))

(defun ide-cpp-cmake-subdirs-exclude ()
  "List of directories to exclude as subdirs in lower level CMakeLists.txt."
  (cl-concatenate 'list
                  ide-cpp-available-include-directories
                  ide-cpp-available-mock-directories
                  ide-cpp-available-cmake-directories
                  (list (ide-cpp-get-build-tree)
                        ide-cpp-default-install-directory
                        ide-cpp-default-pack-directory)))

(defun ide-cpp-has-sibling (basename extensions &optional file)
  "Return non-nil if file named BASENAME is found in same directory as FILE.

EXTENSIONS list of extensions to test"
  (let ((path (f-parent (or file buffer-file-name)))
        (found))
    (loop-for-each ext extensions
      (when (f-exists? (f-join path (concat basename ext)))
        (setq found (f-join path (concat basename ext)))
        (loop-break)))
    (when found (f-filename found))))

(defun ide-cpp-has-main-sibling (&optional file)
  "Return non-nil if main.cpp is found in same directory as FILE."
  (ide-cpp-has-sibling "main" ide-cpp-available-cpp-source-extensions file))

(defun ide-cpp-has-bootstrap-sibling (&optional file)
  "Return non-nil if bootstrap.cpp is found in same directory as FILE."
  (ide-cpp-has-sibling "bootstrap"
                       ide-cpp-available-cpp-source-extensions file))

(defun ide-cpp-has-environment-sibling (&optional file)
  "Return non-nil if environment.cpp is found in same directory as FILE."
  (ide-cpp-has-sibling "environment"
                       ide-cpp-available-cpp-source-extensions file))

(defun ide-cpp-project-or-submodule-name (&optional file)
  "Return submodule name if FILE is a submodule, otherwise return project name."
  (if (ide-cpp-is-submodule file)
      (ide-cpp-submodule-name file)
    (ide-common-get-project-machine-name)))

(defun ide-cpp-project-or-submodule-class (&optional file)
  "Return project or submodule name for FILE formatted as class name."
  (ide-common-normalize-classname (ide-cpp-project-or-submodule-name file)))

(defun ide-cpp-project-or-submodule-file (&optional file)
  "Return project or submodule name for FILE formatted as file name."
  (ide-cpp-project-or-submodule-name file))

(defun ide-cpp-project-or-submodule-human (&optional file)
  "Return project or submodule name for FILE formatted as human readable name."
  (ide-common-normalize-projectname (ide-cpp-project-or-submodule-name file)))

(defun ide-cpp-application-libraries (&optional file)
  "List of application libraries under FILE's source folder."
  (let ((source-path (f-join (ide-common-get-project-root)
                             (ide-common-ascending-path
                              ide-cpp-available-source-directories
                              file)))
        (found-libs))
    (loop-for-each dir ide-cpp-available-core-directories
      (when (f-directory? (f-join source-path dir))
        (if (ide-cpp-is-submodule file)
            (push (format "%s_%s" (ide-cpp-submodule-name) dir) found-libs)
          (push dir found-libs))))
    found-libs))

(defun ide-cpp-is-test (&optional file)
  "Return non-nil if FILE is in a test directory."
  (ide-common-has-ancestor ide-cpp-available-test-directories file))

(defun ide-cpp-list-source-files (&optional file exclude-main)
  "Return list of source files found in same directory as FILE.

if EXCLUDE-MAIN is non-nil, main.cpp will not be listed."
  (let* ((parent (f-parent (or file buffer-file-name)))
        (source-files (if exclude-main
                          (f--files
                           parent
                           (and (member-ignore-case
                                 (f-ext (ide-cpp-real-file-path it) t)
                                 ide-cpp-available-cpp-source-extensions)
                                (not (equal
                                      "main"
                                      (f-base (ide-cpp-real-file-path it))))))
                        (f--files parent
                                  (member-ignore-case
                                   (f-ext (ide-cpp-real-file-path it) t)
                                   ide-cpp-available-cpp-source-extensions)))))
    (seq-map 'f-filename source-files)))

(defun ide-cpp-list-localization-files (&optional file)
  "Return list of localization files found in same directory as FILE."
  (let* ((parent (f-parent (or file buffer-file-name)))
        (localization-files (f--files parent
                                  (member-ignore-case
                                   (f-ext (ide-cpp-real-file-path it) t)
                                   '(".po")))))
    (seq-map 'f-filename localization-files)))

(defun ide-cpp-get-cmake-target (&optional file)
  "Return the cmake target to use in FILE's directory."
  (let ((target (f-filename (f-parent (ide-cpp-real-file-path file)))))
    (when (member target ide-cpp-available-source-directories)
      (setq target (ide-cpp-project-or-submodule-name)))
    target))

(defun ide-cpp-insert-cmake-sources (sources)
  "Insert an indented list of SOURCES into CMakeLists.txt.

Dynamic files \(.cpp.in\) are inserted with their post-configuration paths."
  (loop-for-each source sources
    (if (ide-cpp-is-placeholder-file source)
        (insert (format "  %s\n"
                        (f-join "\$\{CMAKE_CURRENT_BINARY_DIR\}"
                                (f-base source))))
      (insert (format "  %s\n" source)))))

(defun ide-cpp-insert-cmake-locales (domain locales)
  "Insert commands into CMakeLists.txt to compile LOCALES for DOMAIN."
  (loop-for-each locale locales
    (insert (format "do_if_exists(translations_append %s \"%s\")\n" domain (f-join "\$\{CMAKE_CURRENT_SOURCE_DIR\}" locale)))))

(defun ide-cpp-list-cmake-dynamic-configuration-files (&optional file)
  "Return list of .in files found in same directory as FILE.

Exclude those matching `ide-cpp-dynamic-config-excludes'."
  (let* ((parent (f-parent (or file buffer-file-name)))
         (source-files (f--files parent (ide-cpp-is-placeholder-file it)))
         (filenames (seq-map 'f-filename (seq-map 'ide-cpp-real-file-path source-files))))
    (seq-remove
     (lambda (fname)
       (seq-some (lambda (regex) (string-match-p regex fname))
                 ide-cpp-dynamic-config-excludes))
     filenames)))

(defun ide-cpp-version-source-file-candidates ()
  "Return regexps matching source or header files containing version information."
  (mapcar (lambda (ext)
            (concat "version" (regexp-quote ext)))
          (append ide-cpp-available-cpp-header-extensions
                  ide-cpp-available-cpp-source-extensions)))

(defun ide-cpp-has-dynamic-headers (&optional file)
  "Return non-nil if .hpp.in headers  are found in same directory as FILE."
  (let* ((parent (f-parent (or file buffer-file-name)))
         (dynamic-headers
          (f--files parent
                    (and (ide-cpp-is-placeholder-file it)
                         (member (f-ext (f-base it) t)
                                 ide-cpp-available-cpp-header-extensions)))))
    (seq-map 'f-filename (seq-map 'ide-cpp-real-file-path dynamic-headers))))

(defun ide-cpp-cmake-get-generator ()
  "Return the CMake generator.

Extract the generator string from the project's cache,
or read the CMAKE_GENERATOR environment variable."
  (let* ((root (ide-common-get-current-context-project-root))
         (cache (f-join root (ide-cpp-get-build-tree root) "CMakeCache.txt")))
    (if (f-file? cache)
        (with-temp-buffer
          (insert-file-contents cache)
          (when (re-search-forward "^CMAKE_GENERATOR:INTERNAL=\\(.*\\)$" nil t)
            (match-string 1)))
      (getenv "CMAKE_GENERATOR"))))

(defun ide-cpp-is-multi-config ()
  "Return non-nil if a multi-config generator is being used."
  (when-let ((gen (ide-cpp-cmake-get-generator)))
    (string-match-p "\\(Visual\\|Xcode\\|Multi\\|MULTI\\)" gen)))

(defun ide-cpp-is-cmake-project ()
  "Return non-nil if the current project is a CMake project."
  (when-let ((root (ide-common-get-current-context-project-root)))
    (file-exists-p
     (expand-file-name "CMakeLists.txt" root))))

(defun ide-cpp-buffer-is-cmake-project ()
  "Return non-nil if the current buffer belongs to a CMake project."
  (when-let ((root (ffip-project-root)))
    (file-exists-p
     (expand-file-name "CMakeLists.txt" root))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; buffer project context

(defvar-local ide-cpp-cmake-project-p nil
  "Local buffer project context whether file belongs to a CMake project.")

(defun ide-cpp-init-project-context ()
  "Initialize buffer project context whether it belongs to a CMake project."
  (when buffer-file-name
    (setq ide-cpp-cmake-project-p (ide-cpp-buffer-is-cmake-project))))

(defun ide-cpp-on-buffer-init ()
  "Call project context initialization on buffer."
  (ide-cpp-init-project-context)
  (when ide-cpp-cmake-project-p
    (ide-cpp-build-menu-mode 1)))
(add-hook 'find-file-hook #'ide-cpp-on-buffer-init)
(add-hook 'after-change-major-mode-hook #'ide-cpp-on-buffer-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; project scaffolding

(cl-defun ide-cpp-create-project
    (&key type format c-std cpp-std
          module-base-dir
          include-dir
          source-dir core-dir app-dir ui
          test-dir test-sfx mock-dir mock-sfx
          lib-submodule-name
          userdoc-dir userdoc-ext
          locs-dir
          build-dir install-dir pack-dir
          cmake-dir tools-dir
          c-hdr c-src cpp-hdr cpp-src cmake-in)
  "Create a new C++ project in treemacs.

TYPE project type
FORMAT code style
C-STD C standard
CPP-STD C++ standard
MODULE-BASE-DIR module base directory
INCLUDE-DIR public include directory
SOURCE-DIR source directory
CORE-DIR core directory
APP-DIR application directory
UI user interface application
TEST-DIR test directory
TEST-SFX suffix for test files
MOCK-DIR directory for test mock-ups
MOCK-SFX suffix for mock files
LIB-SUBMODULE-NAME name to identify the main library submodule
USERDOC-DIR directory for user documentation sources
USERDOC-EXT user documentation file extension
LOCS-DIR localization directory
BUILD-DIR build directory
INSTALL-DIR installation directory
PACK-DIR packaging directory
CMAKE-DIR CMake modules directory
TOOLS-DIR tools directory
C-HDR C header file extension
C-SRC C source file extension
CPP-HDR C++ header file extension
CPP-SRC C++ source file extension
CMAKE-IN CMake dynamic configuration file extension"
  (interactive)
  (let* ((use-defaults (not (called-interactively-p 'any)))
         (type (ide-common-maybe-prompt
                "Type: " type
                ide-cpp-default-project-type
                use-defaults ide-cpp-dontask-project-type
                ide-cpp-available-project-types t
                (ide-common-hints-from-alist ide-cpp-available-project-types)))
         (format (ide-common-maybe-prompt
                  "Format: " format
                  ide-cpp-default-code-format
                  use-defaults ide-cpp-dontask-code-format
                  ide-cpp-available-code-formats t))
         (c-std (ide-common-maybe-prompt
                 "C Standard: " c-std
                 ide-cpp-default-c-standard
                 use-defaults ide-cpp-dontask-c-standard
                 ide-cpp-available-c-standards t))
         (cpp-std (ide-common-maybe-prompt
                   "C++ Standard: " cpp-std
                   ide-cpp-default-cpp-standard
                   use-defaults ide-cpp-dontask-cpp-standard
                   ide-cpp-available-cpp-standards t))
         (module-base-dir (when (string= "modular" type)
                            (ide-common-maybe-prompt
                             "Module base directory: " module-base-dir
                             ide-cpp-default-module-base-directory
                             use-defaults ide-cpp-dontask-module-base-directory
                             ide-cpp-available-module-base-directories t)))
         (include-dir (unless (string= "simple" type)
                        (ide-common-maybe-prompt
                         "Include directory: " include-dir
                         ide-cpp-default-include-directory
                         use-defaults ide-cpp-dontask-include-directory
                         ide-cpp-available-include-directories t)))
         (source-dir (ide-common-maybe-prompt
                      "Source directory: " source-dir
                      ide-cpp-default-source-directory
                      use-defaults ide-cpp-dontask-source-directory
                      ide-cpp-available-source-directories t))
         (core-dir (when (string= "application" type)
                        (ide-common-maybe-prompt
                         "Core directory: " core-dir
                         ide-cpp-default-core-directory
                         use-defaults ide-cpp-dontask-core-directory
                         ide-cpp-available-core-directories t)))
         (app-dir (when (string= "application" type)
                        (ide-common-maybe-prompt
                         "App directory: " app-dir
                         ide-cpp-default-app-directory
                         use-defaults ide-cpp-dontask-app-directory
                         ide-cpp-available-app-directories t)))
         (ui (when (string= "application" type)
                        (ide-common-maybe-prompt
                         "UI name: " ui
                         ide-cpp-default-ui
                         use-defaults ide-cpp-dontask-ui)))
         (test-dir (ide-common-maybe-prompt
                      "Test directory: " test-dir
                      ide-cpp-default-test-directory
                      use-defaults ide-cpp-dontask-test-directory
                      ide-cpp-available-test-directories t))
         (test-sfx (ide-common-maybe-prompt
                    "Test file suffix: " test-sfx
                    ide-cpp-default-test-suffix
                    use-defaults ide-cpp-dontask-test-sufix
                    ide-cpp-available-test-suffixes t))
         (mock-dir (ide-common-maybe-prompt
                      "Mock directory: " mock-dir
                      ide-cpp-default-mock-directory
                      use-defaults ide-cpp-dontask-mock-directory
                      ide-cpp-available-mock-directories t))
         (mock-sfx (ide-common-maybe-prompt
                    "Mock file suffix: " mock-sfx
                    ide-cpp-default-mock-suffix
                    use-defaults ide-cpp-dontask-mock-sufix
                    ide-cpp-available-mock-suffixes t))
         (lib-submodule-name (ide-common-maybe-prompt
                              "Main library submodule name: "
                              lib-submodule-name
                              ide-cpp-default-module-lib-submodule-name
                              use-defaults
                              ide-cpp-dontask-module-lib-submodule-name))
         (userdoc-dir (ide-common-maybe-prompt
                       "User documentation directory: " userdoc-dir
                       ide-cpp-default-userdoc-directory
                       use-defaults ide-cpp-dontask-userdoc-directory
                       ide-cpp-available-userdoc-directories t))
         (userdoc-ext (ide-common-maybe-prompt
                    "User documentation file extension: " userdoc-ext
                    ide-cpp-default-userdoc-file-extension
                    use-defaults ide-cpp-dontask-userdoc-file-extension
                    ide-cpp-available-userdoc-file-extensions t
                    (ide-common-hints-from-alist
                     ide-cpp-available-userdoc-file-extensions)))
         (userdoc-file (concat ide-cpp-default-userdoc-file-base userdoc-ext))
         (locs-dir (ide-common-maybe-prompt
                    "Localization directory: " locs-dir
                       ide-cpp-default-localize-directory
                       use-defaults ide-cpp-dontask-localize-directory
                       ide-cpp-available-localize-directories t))
         (build-dir (ide-common-maybe-prompt
                     "Build directory: " build-dir
                     ide-cpp-default-build-directory
                     use-defaults ide-cpp-dontask-build-directory))
         (install-dir (ide-common-maybe-prompt
                     "Install directory: " install-dir
                     ide-cpp-default-install-directory
                     use-defaults ide-cpp-dontask-install-directory))
         (pack-dir (ide-common-maybe-prompt
                     "Packaging directory: " pack-dir
                     ide-cpp-default-pack-directory
                     use-defaults ide-cpp-dontask-pack-directory))
         (cmake-dir (ide-common-maybe-prompt
                     "Cmake directory: " cmake-dir
                     ide-cpp-default-cmake-directory
                     use-defaults ide-cpp-dontask-cmake-directory))
         (tools-dir (ide-common-maybe-prompt
                     "Tools directory: " tools-dir
                     ide-cpp-default-tools-directory
                     use-defaults ide-cpp-dontask-tools-directory))
         (c-hdr (ide-common-maybe-prompt
                 "C header extension: " c-hdr
                 ide-cpp-default-c-header-extension
                 use-defaults ide-cpp-dontask-c-header-extension))
         (c-src (ide-common-maybe-prompt
                 "C source extension: " c-src
                 ide-cpp-default-c-source-extension
                 use-defaults ide-cpp-dontask-c-source-extension))
         (cpp-hdr (ide-common-maybe-prompt
                   "C++ header extension: " cpp-hdr
                   ide-cpp-default-cpp-header-extension
                   use-defaults ide-cpp-dontask-cpp-header-extension
                   ide-cpp-available-cpp-header-extensions t))
         (cpp-src (ide-common-maybe-prompt
                   "C++ source extension: " cpp-src
                   ide-cpp-default-cpp-source-extension
                   use-defaults ide-cpp-dontask-cpp-source-extension
                   ide-cpp-available-cpp-source-extensions t))
         (cmake-in (ide-common-maybe-prompt
                 "CMake dynamic configuration file extension: " cmake-in
                 ide-cpp-default-cmake-dynamic-config-extension
                 use-defaults
                 ide-cpp-dontask-cmake-dynamic-config-extension))
         (args (ide-common-create-project
                :type type :format format :c-std c-std :cpp-std cpp-std
                :module-base-dir module-base-dir
                :include-dir include-dir
                :source-dir source-dir
                :core-dir core-dir :app-dir app-dir :ui ui
                :test-dir test-dir :test-sfx test-sfx
                :mock-dir mock-dir :mock-sfx mock-sfx
                :lib-submodule-name lib-submodule-name
                :userdoc-dir userdoc-dir
                :userdoc-file userdoc-file :userdoc-ext userdoc-ext
                :locs-dir locs-dir
                :build-dir build-dir :install-dir install-dir
                :pack-dir pack-dir
                :cmake-dir cmake-dir :tools-dir tools-dir
                :c-hdr c-hdr :c-src c-src
                :cpp-hdr cpp-hdr :cpp-src cpp-src
                :cmake-in cmake-in
                :use-defaults use-defaults)))
    (progn
      ;; suspend recent files
      (recentf-mode 0)

      ;; C++ common project creation steps
      (apply #'ide-cpp-create-vc-ignore-file args)
      (apply #'ide-cpp-create-code-format-file args)
      (apply #'ide-cpp-create-readme-file args)
      (apply #'ide-cpp-create-graphviz-options-file args)
      (apply #'ide-cpp-create-valgrind-suppressions-file args)
      (apply #'ide-cpp-create-install-dir args)
      (apply #'ide-cpp-create-pack-dir args)
      (apply #'ide-cpp-create-cmake-dir args)
      (apply #'ide-cpp-create-tools-dir args)

      ;; create project structure according to project type
      (apply
       (intern (format "ide-cpp-create-%s-project" (cl-getf args :type)))
       args)

      ;; C++ common project post-creation steps
      (apply #'ide-cpp-create-cmake-root args)
      (ide-cpp-initialize-debug nil (cl-getf args :path) (cl-getf args :build-dir))
      (ide-common-initial-commit (cl-getf args :path))
      (delete-other-windows)

      ;; re-enable recent files
      (recentf-mode 1))))

(cl-defun ide-cpp-create-simple-project (&rest args)
  "Create a simple C++ project.

ARGS project creation parameters"
  (apply #'ide-cpp-create-app-source-dir args)
  (apply #'ide-cpp-create-app-test-dir args)
  (ide-cpp-create-simple-profiles (cl-getf args :path) t))

(cl-defun ide-cpp-create-application-project (&rest args)
  "Create a standard C++ application project.

ARGS project creation parameters"
  (apply #'ide-cpp-create-include-dir args)
  (apply #'ide-cpp-create-source-core-and-app-dirs args)
  (apply #'ide-cpp-create-test-core-and-app-dirs args)
  (apply #'ide-cpp-create-userdoc-dir args)
  (apply #'ide-cpp-create-localize-dir args)
  (apply #'ide-cpp-create-cmake-export-config-template-file args)
  (ide-cpp-create-application-profiles (cl-getf args :path) t))

(cl-defun ide-cpp-create-library-project (&rest args)
  "Create a C++ library project.

ARGS project creation parameters"
  (apply #'ide-cpp-create-include-dir args)
  (apply #'ide-cpp-create-lib-source-dir args)
  (apply #'ide-cpp-create-lib-test-dir args)
  (apply #'ide-cpp-create-cmake-export-config-template-file args)
  (ide-cpp-create-library-profiles (cl-getf args :path) t))

(cl-defun ide-cpp-create-modular-project (&rest args)
  "Create a C++ submodules project.

ARGS project creation parameters"

  ;; add export configuration template to cmake modules directory
  (apply #'ide-cpp-create-cmake-export-config-template-file args)

  ;; create common profiles
  (ide-cpp-create-moudlar-profiles (cl-getf args :path) t)

  ;; first create application submodule
  ;; args contains path to project root
  ;; module-args contains path to module base
  (let ((module-args (copy-tree args))
        (path (cl-getf args :path))
        (root (f-filename (cl-getf args :path)))
        (base (cl-getf args :module-base-dir)))
    (setf (cl-getf module-args :path) (f-join path base root))

    (apply #'ide-cpp-create-app-source-dir module-args)
    (apply #'ide-cpp-create-app-test-dir module-args)
    (apply #'ide-cpp-create-userdoc-dir module-args)
    (apply #'ide-cpp-create-localize-dir module-args)
    ;; cmake subdirs
    (find-file (f-join path base root "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer))

  ;; then create library submodule and source directory structure
  ;; args contains path to project root
  ;; module-args contains path to module base
  (let ((module-args (copy-tree args))
        (path (cl-getf args :path))
        (root (cl-getf args :lib-submodule-name))
        (base (cl-getf args :module-base-dir)))
    (setf (cl-getf module-args :path) (f-join path base root))

    (apply #'ide-cpp-create-include-dir module-args)
    (apply #'ide-cpp-create-lib-source-dir module-args)
    (apply #'ide-cpp-create-lib-test-dir module-args)
    ;; cmake subdirs
    (find-file (f-join path base root "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join path base "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; populate project

(cl-defun ide-cpp-create-vc-ignore-file
    (&key vc path build-dir install-dir pack-dir &allow-other-keys)
  "Create version control ignore file.

VC version control backend
PATH parent directory
BUILD-DIR directory for out-of-source builds
INSTALL-DIR directory for binary installations
PACK-DIR directory for binary distribution packages"
  (let* ((ignores (cond
                   ((string= "CVS" vc) ".cvsignore")
                   ((string= "SVN" vc) ".svnignore")
                   ((string= "Bzr" vc) ".bzrignore")
                   ((string= "Git" vc) ".gitignore")
                   ((string= "Hg" vc) ".hgignore")
                   ((string= "Mtn" vc) ".mtn-ignore")
                   (t nil)))
        (ignore-file (when ignores (f-join path ignores)))
        (snippet (cond
                   ((string= "CVS" vc) "CVS")
                   ((string= "SVN" vc) "SVN")
                   ((string= "Bzr" vc) "Bazaar")
                   ((string= "Git" vc) "Git")
                   ((string= "Hg" vc) "Mercurial")
                   (t nil))))
    (when ignore-file
      (find-file ignore-file)
      (gitignore-mode)

      (insert "### target directories ###\n")
      (insert (format "/%s/\n" build-dir))
      (insert (format "/%s/\n" install-dir))
      (insert (format "/%s/\n" pack-dir))
      (insert "\n")
      (yas-expand-snippet (yas-lookup-snippet "C" 'gitignore-mode))
      (yas-exit-all-snippets)
      (insert "\n")
      (yas-expand-snippet (yas-lookup-snippet "C++" 'gitignore-mode))
      (yas-exit-all-snippets)
      (insert "\n")
      (yas-expand-snippet (yas-lookup-snippet "CMake" 'gitignore-mode))
      (yas-exit-all-snippets)
      (insert "\n")
      (yas-expand-snippet (yas-lookup-snippet "Emacs" 'gitignore-mode))
      (yas-exit-all-snippets)
      (insert "\n")
      (when snippet
        (yas-expand-snippet (yas-lookup-snippet snippet 'gitignore-mode))
        (yas-exit-all-snippets)
        (insert "\n"))
      (yas-expand-snippet (yas-lookup-snippet "Ninja" 'gitignore-mode))
      (yas-exit-all-snippets)
      (insert "\n")
      (yas-expand-snippet (yas-lookup-snippet "VisualStudioCodeAll" 'gitignore-mode))
      (yas-exit-all-snippets)
      (insert "\n")
      (yas-expand-snippet (yas-lookup-snippet "LLVM" 'gitignore-mode))
      (yas-exit-all-snippets)
      (save-buffer)
      (kill-buffer)

      (when (string= "SVN" vc)
        (with-temp-buffer
          (vc-do-command t t "propset"
                         nil
                         '("svn:ignore" "-F" (format "\"%s\"" ignore-file)
                          (format "\"%s\"" path))))))))

(cl-defun ide-cpp-create-code-format-file (&key path format &allow-other-keys)
  "Create code format file.

PATH parent directory
FORMAT code style format"
  (let ((format-file (f-join path ".clang-format")))
    (call-process "clang-format"
                  nil
                  (list :file format-file)
                  nil
                  (format "--style=%s" format)
                  "--dump-config")))

(cl-defun ide-cpp-create-cmake-export-config-template-file (&key path cmake-dir cmake-in &allow-other-keys)
  "Create export-config.cmake.in file.

PATH parent directory
CMAKE-DIR CMake modules directory
CMAKE-IN CMake dynamic configuration file extension"
  (let ((export-file (f-join path cmake-dir (format "export-config.cmake%s" cmake-in))))
    (find-file export-file)
    (yas-expand-snippet (yas-lookup-snippet "export-config" 'cmake-mode))
    (yas-exit-all-snippets)
    (insert "\n")
    (save-buffer)
    (kill-buffer)))

(cl-defun ide-cpp-create-include-dir
    (&key type path orig-path include-dir cpp-hdr &allow-other-keys)
  "Create public include directory.

TYPE project type
PATH parent directory
ORIG-PATH the original parent directory on project creation
INCLUDE-DIR public include directory
CPP-HDR C++ header file extension"
  (let* ((library-name (f-filename path))
         (include-path (if (string= "modular" type)
                           (f-join path include-dir (f-filename orig-path) library-name)
                           (f-join path include-dir library-name))))
    (f-mkdir-full-path include-path)
    (find-file (f-join include-path (concat library-name cpp-hdr)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join include-path (concat "version" cpp-hdr)))
    (save-buffer)
    (kill-buffer)))

(cl-defun ide-cpp-create-app-source-dir
    (&key type path source-dir cpp-hdr cpp-src cmake-in &allow-other-keys)
  "Create application source directory.

TYPE project type
PATH parent directory
SOURCE-DIR source directory
CPP-HDR C++ header file extension
CPP-SRC C++ source file extension
CMAKE-IN CMake dynamic configuration file extension"
  (let ((source-path (f-join path source-dir))
        (project-name (f-filename path)))
    (f-mkdir-full-path source-path)
    (when (or (string= "simple" type) (string= "modular" type))
      (find-file (f-join source-path (concat project-name cpp-hdr)))
      (save-buffer)
      (kill-buffer)
      (find-file (f-join source-path (concat "version" cpp-hdr)))
      (save-buffer)
      (kill-buffer))
    (find-file (f-join source-path (concat "version" cpp-src cmake-in)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-path (concat project-name cpp-src)))
    (save-buffer)
    (kill-buffer)
    (when (string= "modular" type)
      (find-file (f-join source-path (concat "environment" cpp-hdr)))
      (save-buffer)
      (kill-buffer)
      (find-file (f-join source-path (concat "environment" cpp-src)))
      (save-buffer)
      (kill-buffer))
    (find-file (f-join source-path (concat "config" cpp-hdr)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-path (concat "config" cpp-src)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-path (concat "bootstrap" cpp-hdr)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-path (concat "bootstrap" cpp-src)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-path (concat "main" cpp-src)))
    (save-buffer)
    (kill-buffer)
    ;; cmake subdirs
    (find-file (f-join source-path "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)))

(cl-defun ide-cpp-create-lib-source-dir
    (&key path source-dir cpp-src cmake-in &allow-other-keys)
  "Create library source directory.

PATH parent directory
SOURCE-DIR source directory
CPP-SRC C++ source file extension
CMAKE-IN CMake dynamic configuration file extension"
  (let ((source-path (f-join path source-dir))
        (project-name (f-filename path)))
    (f-mkdir-full-path source-path)
    (find-file (f-join source-path (concat "version" cpp-src cmake-in)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-path (concat project-name cpp-src)))
    (save-buffer)
    (kill-buffer)
    ;; cmake subdirs
    (find-file (f-join source-path "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)))

(cl-defun ide-cpp-create-app-test-dir
    (&key type path test-dir test-sfx mock-dir cpp-src &allow-other-keys)
  "Create application test directory.

TYPE project type
PATH parent directory
TEST-DIR directory for unit tests
TEST-SFX suffix for test files
MOCK-DIR directory for mocks
CPP-SRC C++ source file extension"
  (let* ((test-path (f-join path test-dir))
         (mock-path (f-join test-path mock-dir))
         (project-name (f-filename path)))
    (f-mkdir-full-path mock-path)
    (find-file (f-join test-path (concat project-name test-sfx cpp-src)))
    (save-buffer)
    (kill-buffer)
    (when (string= "modular" type)
      (find-file (f-join test-path (concat "environment" test-sfx cpp-src)))
      (save-buffer)
      (kill-buffer))
    (find-file (f-join test-path (concat "config" test-sfx cpp-src)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join test-path (concat "bootstrap" test-sfx cpp-src)))
    (save-buffer)
    (kill-buffer)
    ;; cmake subdirs
    (find-file (f-join test-path "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)))

(cl-defun ide-cpp-create-lib-test-dir
    (&key path test-dir test-sfx mock-dir cpp-src &allow-other-keys)
  "Create library test directory.

PATH parent directory
TEST-DIR directory for unit tests
TEST-SFX suffix for test files
MOCK-DIR directory for mocks
CPP-SRC C++ source file extension"
  (let* ((test-path (f-join path test-dir))
         (mock-path (f-join test-path mock-dir))
         (project-name (f-filename path)))
    (f-mkdir-full-path mock-path)
    (find-file (f-join test-path (concat project-name test-sfx cpp-src)))
    (save-buffer)
    (kill-buffer)
    ;; cmake subdirs
    (find-file (f-join test-path "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)))

(cl-defun ide-cpp-create-source-core-and-app-dirs
    (&key path source-dir core-dir app-dir ui
          cpp-hdr cpp-src cmake-in &allow-other-keys)
  "Create source directory.

PATH parent directory
SOURCE-DIR source directory
CORE-DIR core directory
APP-DIR application directory
UI user interface application
CPP-HDR C++ header file extension
CPP-SRC C++ source file extension
CMAKE-IN CMake dynamic configuration file extension"
  (let ((project-name (f-filename path))
        (source-core-path (f-join path source-dir core-dir))
        (source-app-path (f-join path source-dir app-dir ui)))
    (f-mkdir-full-path source-core-path)
    (find-file (f-join source-core-path (concat project-name cpp-src)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-core-path (concat "version" cpp-src cmake-in)))
    (save-buffer)
    (kill-buffer)
    (f-mkdir-full-path source-app-path)
    (find-file (f-join source-app-path (concat "config" cpp-hdr)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-app-path (concat "config" cpp-src)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-app-path (concat "environment" cpp-hdr)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-app-path (concat "environment" cpp-src)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-app-path (concat "bootstrap" cpp-hdr)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-app-path (concat "bootstrap" cpp-src)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-app-path (concat "main" cpp-src)))
    (save-buffer)
    (kill-buffer)
    ;; cmake subdirs
    (find-file (f-join source-app-path "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join path source-dir app-dir "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join source-core-path "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join path source-dir  "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)))

(cl-defun ide-cpp-create-test-core-and-app-dirs
    (&key path test-dir test-sfx mock-dir
          core-dir app-dir ui cpp-src &allow-other-keys)
  "Create source directory.

PATH parent directory
TEST-DIR directory for unit tests
TEST-SFX suffix for test files
MOCK-DIR directory for mocks
CORE-DIR core directory
APP-DIR application directory
UI user interface application
CPP-SRC C++ source file extension"
  (let* ((project-name (f-filename path))
        (test-core-path (f-join path test-dir core-dir))
        (mock-core-path (f-join test-core-path mock-dir))
        (test-app-path (f-join path test-dir app-dir ui))
        (mock-app-path (f-join test-app-path mock-dir)))
    (f-mkdir-full-path mock-core-path)
    (find-file (f-join test-core-path (concat project-name test-sfx cpp-src)))
    (save-buffer)
    (kill-buffer)
    (f-mkdir-full-path mock-app-path)
    (find-file (f-join test-app-path (concat "config" test-sfx cpp-src)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join test-app-path (concat "bootstrap" test-sfx cpp-src)))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join test-app-path (concat "environment" test-sfx cpp-src)))
    (save-buffer)
    (kill-buffer)
    ;; cmake subdirs
    (find-file (f-join test-app-path "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join path test-dir app-dir "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join test-core-path "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)
    (find-file (f-join path test-dir  "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)))

(cl-defun ide-cpp-create-readme-file (&key type path readme-file name descr install-dir pack-dir vc &allow-other-keys)
  "Generate README-FILE in PATH.

TYPE project type
PATH path to project root
README-FILE name of readme file including extension
NAME project name
DESCR short description of project
INSTALL-DIR installation directory
PACK-DIR packaging directory
VC version control backend"
  (let* ((file-path (f-join path readme-file))
         (ext (file-name-extension readme-file))
         ;; assemble readme file from building blocks in org-mode
         (assembled
          (let ((buf (generate-new-buffer "*yas-readme-temp*"))
                result)
            (unwind-protect
                (with-current-buffer buf
                  (org-mode)
                  (yas-minor-mode-on)
                  (yas-expand-snippet (yas-lookup-snippet "readme-cpp-title" 'org-mode))
                  (insert name)
                  (yas-next-field)
                  (insert descr)
                  (yas-next-field)
                  (yas-exit-all-snippets)
                  (yas-expand-snippet (yas-lookup-snippet "readme-cpp-quickstart" 'org-mode))
                  (yas-exit-all-snippets)
                  (yas-expand-snippet (yas-lookup-snippet "readme-cpp-compiling" 'org-mode))
                  (yas-exit-all-snippets)
                  (if (member type '("application" "modular"))
                      (yas-expand-snippet (yas-lookup-snippet "readme-cpp-documentation" 'org-mode))
                    (yas-expand-snippet (yas-lookup-snippet "readme-cpp-documentation-nouserdocs" 'org-mode)))
                  (yas-exit-all-snippets)
                  (when (member type '("application" "modular"))
                    (yas-expand-snippet (yas-lookup-snippet "readme-cpp-localization" 'org-mode))
                    (yas-exit-all-snippets))
                  (yas-expand-snippet (yas-lookup-snippet "readme-cpp-versioning" 'org-mode))
                  (yas-exit-all-snippets)
                  (pcase vc
                    ('Git
                     (yas-expand-snippet (yas-lookup-snippet "readme-cpp-vcs-git" 'org-mode))
                     (yas-exit-all-snippets)))
                  (yas-expand-snippet (yas-lookup-snippet "readme-cpp-bumping" 'org-mode))
                  (yas-exit-all-snippets)
                  (yas-expand-snippet (yas-lookup-snippet "readme-cpp-installing" 'org-mode))
                  (insert install-dir)
                  (yas-next-field)
                  (insert pack-dir)
                  (yas-next-field)
                  (yas-exit-all-snippets)
                  (unless (member type '("simple"))
(yas-expand-snippet (yas-lookup-snippet "readme-cpp-importing" 'org-mode))
                    (yas-exit-all-snippets))
                  (when (member type '("application" "modular"))
(yas-expand-snippet (yas-lookup-snippet "readme-cpp-libraries" 'org-mode))
                    (yas-exit-all-snippets))
                  (when (member type '("simple"))
(yas-expand-snippet (yas-lookup-snippet "readme-cpp-libraries-noboost" 'org-mode))
                    (yas-exit-all-snippets))
                  (yas-expand-snippet (yas-lookup-snippet "readme-cpp-contributing" 'org-mode))
                  (yas-exit-all-snippets)
                  (setq result (buffer-string)))
              (kill-buffer buf))
            result))
         ;; convert to final format
         (converted (ide-common-convert-org assembled ext nil)))
    ;; write readme file in destination format
    (progn
      (when (f-exists? file-path)
        (delete-file file-path))
      (find-file file-path)
      (goto-char (point-max))
      (insert converted)
      (save-buffer)
      (kill-buffer))))

(defun ide-cpp-create-userdoc-file (path &optional userdoc-file name descr)
  "Create USERDOC-FILE in PATH."
  (let* ((userdoc-file (or
                       userdoc-file
                       (concat ide-cpp-default-userdoc-file-base
                               ide-cpp-default-userdoc-file-extension)))
         (file-path (f-join path userdoc-file))
         (ext (file-name-extension userdoc-file))
         (executable (ide-common-normalize-filename name))
         (converted (ide-common-convert-org "userdoc" ext t name descr executable)))
    (progn
      (when (f-exists? file-path)
        (delete-file file-path))
      (find-file file-path)
      (goto-char (point-max))
      (insert converted)
      (save-buffer)
      (kill-buffer))))

(cl-defun ide-cpp-create-userdoc-dir
    (&key path userdoc-dir userdoc-file name descr &allow-other-keys)
  "Create  directory for user documentation sources.

PATH parent directory
USERDOC-DIR subdirectory for user documentation sources
USERDOC-FILE user documentation file name
NAME project name
DESCR project description"
  (let ((userdoc-parent (f-join path userdoc-dir)))
    (f-mkdir-full-path userdoc-parent)
    (ide-cpp-create-userdoc-file userdoc-parent userdoc-file name descr)
    ;; cmake subdirs
    (find-file (f-join userdoc-parent "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)))

(cl-defun ide-cpp-create-localize-dir (&key path orig-path locs-dir tools-dir &allow-other-keys)
  "Create localization directory.

PATH parent directory (project root or module root)
ORIG-PATH original parent directory (always project root)
LOCS-DIR directory for localized dictionaries
TOOLS-DIR directory where localization script can be found"
  (let ((command (f-join orig-path tools-dir "i18ngen"))
        (localize-dir (f-join path locs-dir))
        (locale "en_US"))
    (f-mkdir-full-path localize-dir)
    (shell-command (format "%s -l %s" command locale) "*Python Output*")
    (find-file (f-join localize-dir "CMakeLists.txt"))
    (save-buffer)
    (kill-buffer)))

(cl-defun ide-cpp-create-install-dir (&key path install-dir &allow-other-keys)
  "Create installation directory.

PATH parent directory
INSTALL-DIR directory for installing binaries"
  (f-mkdir (f-join path install-dir)))

(cl-defun ide-cpp-create-pack-dir (&key path pack-dir &allow-other-keys)
  "Create packaging directory.

PATH parent directory
PACK-DIR directory for binary distribution packages"
  (f-mkdir (f-join path pack-dir)))

(cl-defun ide-cpp-create-cmake-dir
    (&key path cmake-dir &allow-other-keys)
  "Create CMake directory.

PATH parent directory.
CMAKE-DIR directory for CMake modules and scripts."
  (let ((cmake-module-path (f-join path cmake-dir)))
    (f-mkdir-full-path cmake-module-path)
    (dolist (group '("module" "script"))
      (dolist (snippet (ide-common-available-snippets 'cmake-mode group))
        (find-file (f-join cmake-module-path (format "%s.cmake" snippet)))
        (yas-expand-snippet (yas-lookup-snippet snippet 'cmake-mode))
        (yas-exit-all-snippets)
        (save-buffer)
        (kill-buffer)))))

(cl-defun ide-cpp-create-tools-dir
    (&key path tools-dir &allow-other-keys)
  "Create Tools directory.

PATH parent directory
TOOLS-DIR directory for development scripts"
  (let ((tools-script-path (f-join path tools-dir)))
    (f-mkdir-full-path (f-join path tools-dir))
    (dolist (script (ide-common-available-snippets 'python-mode "buildtool"))
      (find-file (f-join tools-script-path script))
      (python-mode)
      (yas-expand-snippet (yas-lookup-snippet script 'python-mode))
      (yas-exit-all-snippets)
      (save-buffer)
      (kill-buffer))))

(cl-defun ide-cpp-create-cmake-root
    (&key path descr vendor contact c-std cpp-std type &allow-other-keys)
  "Create root CMakeLists.txt, CMakeGraphVizOptions.cmake, and valgrind.supp.

PATH parent directory
DESCR short project description
VENDOR project vendor
CONTACT project vendor contact email
C-STD C standard
CPP-STD C++ standard
TYPE project type"
  (find-file (f-join path "CMakeLists.txt"))
  (ide-cpp-disable-cmake-includes-for-type type)
  (goto-char (point-min))
  (while (re-search-forward "DESCRIPTION \".*\"" nil t)
    (replace-match (format "DESCRIPTION \"%s\"" descr)))
  (goto-char (point-min))
  (while (re-search-forward "PROJECT_VENDOR \".*\"" nil t)
    (replace-match (format "PROJECT_VENDOR \"%s\"" vendor)))
  (goto-char (point-min))
  (while (re-search-forward "PROJECT_CONTACT \".*\"" nil t)
    (replace-match (format "PROJECT_CONTACT \"%s\"" contact)))
  (goto-char (point-min))
  (while (re-search-forward "CMAKE_C_STANDARD[[:space:]]*[[:digit:]]+" nil t)
    (replace-match (format "CMAKE_C_STANDARD %s" c-std)))
  (goto-char (point-min))
  (while (re-search-forward "CMAKE_CXX_STANDARD[[:space:]]*[[:digit:]]+" nil t)
    (replace-match (format "CMAKE_CXX_STANDARD %s" cpp-std)))
  (save-buffer))

(cl-defun ide-cpp-create-graphviz-options-file
    (&key path type &allow-other-keys)
  "Create CMakeGraphVizOptions.cmake file to configure graphviz.

PATH parent directory
TYPE project type"
  (find-file (f-join path "CMakeGraphVizOptions.cmake"))
  (save-buffer)
  (kill-buffer))

(cl-defun ide-cpp-create-valgrind-suppressions-file
    (&key path type &allow-other-keys)
  "Create valgrind.supp file to suppress valgrind errors.

PATH parent directory
TYPE project type"
  (find-file (f-join path "valgrind.supp"))
  (unless (member type '("simple" "library"))
    (insert "\n")
    (ide-common-insert-snippet"valgrindsupp-boost-locale" 'text-mode))
  (save-buffer)
  (kill-buffer))

(defun ide-cpp-disable-cmake-includes-for-type (type)
  "Exclude CMake modules which are not needed for TYPE project types."
  (pcase type
    ("simple" (ide-cpp-disable-cmake-includes '("user_documentation" "boost" "boost_locale" "boost_dll" "msgfmt")))
    ("library" (ide-cpp-disable-cmake-includes '("user_documentation" "cli11" "boost" "boost_locale" "boost_dll" "msgfmt")))))

(defun ide-cpp-disable-cmake-includes (includes)
  "Comment out INCLUDES from CMakeLists.txt."
  (dolist (include includes)
    (let ((regex (format "^include[[:blank:]]*(%s)" include)))
      (progn (goto-char (point-min))
             (while (re-search-forward regex nil t)
               (comment-line 1))))))

(defun ide-cpp-create-application-profiles (&optional path reset)
  "Create common profiles for building application projects.

PATH project root directory
RESET if non-nil, erase all previously existing profiles for project"
  (interactive)
  (let ((path (or path (ide-common-get-project-root))))

    (when reset (ide-common-reset-profiles path))

    (ide-common-args-store-string-as-list
     path "build-debug" "default" "")
    (ide-common-args-store-string-as-list
     path "build-debug" "coverage" "-t coverage")
    (ide-common-args-store-string-as-list
     path "build-debug" "memcheck" "-t valgrind")
    (ide-common-args-store-string-as-list
     path "build-debug" "docs" "-t docs")
    (ide-common-args-store-string-as-list
     path "build-debug" "all" "-t all -t coverage -t valgrind -t docs")
    (ide-common-args-set-current-profile path "build-debug" "default")

    (ide-common-args-store-string-as-list
     path "build-release" "default" "")
    (ide-common-args-store-string-as-list
     path "build-release" "docs" "-t userdocs -t publicapidocs")
    (ide-common-args-store-string-as-list
     path "build-release" "all" "-t all -t userdocs -t publicapidocs")
    (ide-common-args-set-current-profile path "build-release" "default")

    (ide-common-args-store-string-as-list
     path "install-debug" "default" "")
    (ide-common-args-store-string-as-list
     path "install-debug" "runtime" "--component=runtime")
    (ide-common-args-store-string-as-list
     path "install-debug" "devel" "--component=devel")
    (ide-common-args-store-string-as-list
     path "install-debug" "all" "--component=runtime --component=devel")
    (ide-common-args-set-current-profile path "install-debug" "default")

    (ide-common-args-store-string-as-list
     path "install-release" "default" "")
    (ide-common-args-store-string-as-list
     path "install-release" "runtime" "--component=runtime")
    (ide-common-args-store-string-as-list
     path "install-release" "devel" "--component=devel")
    (ide-common-args-store-string-as-list
     path "install-release" "all" "--component=runtime --component=devel")
    (ide-common-args-set-current-profile path "install-release" "default")))

(defun ide-cpp-create-modular-profiles (&optional path reset)
  "Create common profiles for building modular projects.

PATH project root directory
RESET if non-nil, erase all previously existing profiles for project"
  (interactive)
  (let ((path (or path (ide-common-get-project-root))))

    (when reset (ide-common-reset-profiles path))

    (ide-common-args-store-string-as-list
     path "build-debug" "default" "")
    (ide-common-args-store-string-as-list
     path "build-debug" "coverage" "-t coverage")
    (ide-common-args-store-string-as-list
     path "build-debug" "memcheck" "-t valgrind")
    (ide-common-args-store-string-as-list
     path "build-debug" "docs" "-t docs")
    (ide-common-args-store-string-as-list
     path "build-debug" "all" "-t all -t coverage -t valgrind -t docs")
    (ide-common-args-set-current-profile path "build-debug" "default")

    (ide-common-args-store-string-as-list
     path "build-release" "default" "")
    (ide-common-args-store-string-as-list
     path "build-release" "docs" "-t userdocs -t publicapidocs")
    (ide-common-args-store-string-as-list
     path "build-release" "all" "-t all -t userdocs -t publicapidocs")
    (ide-common-args-set-current-profile path "build-release" "default")

    (ide-common-args-store-string-as-list
     path "install-debug" "default" "")
    (ide-common-args-store-string-as-list
     path "install-debug" "runtime" "--component=runtime")
    (ide-common-args-store-string-as-list
     path "install-debug" "devel" "--component=devel")
    (ide-common-args-store-string-as-list
     path "install-debug" "all" "--component=runtime --component=devel")
    (ide-common-args-set-current-profile path "install-debug" "default")

    (ide-common-args-store-string-as-list
     path "install-release" "default" "")
    (ide-common-args-store-string-as-list
     path "install-release" "runtime" "--component=runtime")
    (ide-common-args-store-string-as-list
     path "install-release" "devel" "--component=devel")
    (ide-common-args-store-string-as-list
     path "install-release" "all" "--component=runtime --component=devel")
    (ide-common-args-set-current-profile path "install-release" "default")))

(defun ide-cpp-create-simple-profiles (&optional path reset)
  "Create common profiles for building simple projects.

PATH project root directory
RESET if non-nil, erase all previously existing profiles for project"
  (interactive)
  (let ((path (or path (ide-common-get-project-root))))

    (when reset (ide-common-reset-profiles path))

    (ide-common-args-store-string-as-list
     path "build-debug" "default" "")
    (ide-common-args-store-string-as-list
     path "build-debug" "coverage" "-t coverage")
    (ide-common-args-store-string-as-list
     path "build-debug" "memcheck" "-t valgrind")
    (ide-common-args-store-string-as-list
     path "build-debug" "docs" "-t docs")
    (ide-common-args-store-string-as-list
     path "build-debug" "all" "-t all -t coverage -t valgrind -t docs")
    (ide-common-args-set-current-profile path "build-debug" "default")

    (ide-common-args-store-string-as-list
     path "build-release" "default" "")
    (ide-common-args-store-string-as-list
     path "build-release" "docs" "-t publicapidocs")
    (ide-common-args-store-string-as-list
     path "build-release" "all" "-t all -t publicapidocs")
    (ide-common-args-set-current-profile path "build-release" "default")

    (ide-common-args-store-string-as-list
     path "install-debug" "default" "")
    (ide-common-args-store-string-as-list
     path "install-debug" "runtime" "--component=runtime")
    (ide-common-args-store-string-as-list
     path "install-debug" "devel" "--component=devel")
    (ide-common-args-store-string-as-list
     path "install-debug" "all" "--component=runtime --component=devel")
    (ide-common-args-set-current-profile path "install-debug" "default")

    (ide-common-args-store-string-as-list
     path "install-release" "default" "")
    (ide-common-args-store-string-as-list
     path "install-release" "runtime" "--component=runtime")
    (ide-common-args-store-string-as-list
     path "install-release" "devel" "--component=devel")
    (ide-common-args-store-string-as-list
     path "install-release" "all" "--component=runtime --component=devel")
    (ide-common-args-set-current-profile path "install-release" "default")))

(defun ide-cpp-create-library-profiles (&optional path reset)
  "Create common profiles for building library projects.

PATH project root directory
RESET if non-nil, erase all previously existing profiles for project"
  (interactive)
  (let ((path (or path (ide-common-get-project-root))))

    (when reset (ide-common-reset-profiles path))

    (ide-common-args-store-string-as-list
     path "build-debug" "default" "")
    (ide-common-args-store-string-as-list
     path "build-debug" "coverage" "-t coverage")
    (ide-common-args-store-string-as-list
     path "build-debug" "memcheck" "-t valgrind")
    (ide-common-args-store-string-as-list
     path "build-debug" "docs" "-t docs")
    (ide-common-args-store-string-as-list
     path "build-debug" "all" "-t all -t coverage -t valgrind -t docs")
    (ide-common-args-set-current-profile path "build-debug" "default")

    (ide-common-args-store-string-as-list
     path "build-release" "default" "")
    (ide-common-args-store-string-as-list
     path "build-release" "docs" "-t publicapidocs")
    (ide-common-args-store-string-as-list
     path "build-release" "all" "-t all -t publicapidocs")
    (ide-common-args-set-current-profile path "build-release" "default")

    (ide-common-args-store-string-as-list
     path "install-debug" "default" "")
    (ide-common-args-store-string-as-list
     path "install-debug" "runtime" "--component=runtime")
    (ide-common-args-store-string-as-list
     path "install-debug" "devel" "--component=devel")
    (ide-common-args-store-string-as-list
     path "install-debug" "all" "--component=runtime --component=devel")
    (ide-common-args-set-current-profile path "install-debug" "default")

    (ide-common-args-store-string-as-list
     path "install-release" "default" "")
    (ide-common-args-store-string-as-list
     path "install-release" "runtime" "--component=runtime")
    (ide-common-args-store-string-as-list
     path "install-release" "devel" "--component=devel")
    (ide-common-args-store-string-as-list
     path "install-release" "all" "--component=runtime --component=devel")
    (ide-common-args-set-current-profile path "install-release" "default")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; auto-inserts

(defun ide-cpp-register-auto-inserts-with-placeholder
    (filenames extensions actions)
  "Register `auto-insert' actions for newly created files.

FILENAMES list of base namespace
EXTENSIONS list of filename extensions
ACTIONS list of actions to execute

Append `ide-cpp-default-cmake-dynamic-config-extension' to EXTENSIONS
before calling `ide-common-register-auto-inserts'."
  (let ((final-extensions))
    (progn
      (dolist (extension extensions final-extensions)
        (push (format"%s\\(?:%s\\)?"
                     extension
                     ide-cpp-default-cmake-dynamic-config-extension)
              final-extensions))
      (ide-common-register-auto-inserts filenames final-extensions actions))))

(define-skeleton ide-cpp-header-skeleton
  "Template for C++ header files."
  nil
  '(ide-common-insert-snippet "spdx")
  '(ide-common-insert-snippet "doxyfile")
  '(ide-common-insert-snippet "headerguard")
  '(when (ide-cpp-is-active-header)
     (ide-common-insert-snippet (ide-cpp-snippet-name "includes")))
  '(ide-common-insert-snippet "namespace")
  '(when (ide-cpp-is-active-header)
     (ide-common-insert-snippet (ide-cpp-snippet-name "body"))))

(ide-cpp-register-auto-inserts-with-placeholder
 nil
 ide-cpp-available-cpp-header-extensions
 'ide-cpp-header-skeleton)

(define-skeleton ide-cpp-source-skeleton
  "Template for C++ source files."
  nil
  '(ide-common-insert-snippet "spdx")
  '(ide-common-insert-snippet "doxyfile")
  (ide-cpp-private-include)
  (ide-cpp-public-include)
  '(ide-common-insert-snippet (ide-cpp-snippet-name "includes"))
  '(ide-common-insert-snippet "namespace")
  '(ide-common-insert-snippet (ide-cpp-snippet-name "body")))

(ide-cpp-register-auto-inserts-with-placeholder
 nil
 ide-cpp-available-cpp-source-extensions
 'ide-cpp-source-skeleton)

(define-skeleton ide-cpp-main-source-skeleton
  "Template for C++ source files."
  nil
  '(ide-common-insert-snippet "spdx")
  '(ide-common-insert-snippet "doxyfile")
  (ide-cpp-private-include)
  (ide-cpp-public-include)
  '(ide-common-insert-snippet (ide-cpp-snippet-name "includes"))
  '(ide-common-insert-snippet (ide-cpp-snippet-name "body")))

(ide-cpp-register-auto-inserts-with-placeholder
 "main"
 ide-cpp-available-cpp-source-extensions
 'ide-cpp-main-source-skeleton)

(define-skeleton ide-cpp-test-skeleton
  "Template for C++ unit test files."
  nil
  '(ide-common-insert-snippet "spdx")
  '(ide-common-insert-snippet (ide-cpp-snippet-name "test-includes"))
  (ide-cpp-private-include)
  (ide-cpp-public-include)
  '(ide-common-insert-snippet "test-includes")
  '(ide-common-insert-snippet "namespace")
  "class " (ide-cpp-class-name) "Tests : public testing::Test{\n"
  "protected:\n"
  (ide-cpp-test-declare-member-of-class)
  "};\n\n"
  '(ide-common-insert-snippet (ide-cpp-snippet-name "test-body")))

(ide-common-register-auto-inserts
 (mapcar (lambda (s) (format "/%s/.*" s)) ide-cpp-available-test-directories)
 ide-cpp-available-cpp-source-extensions
 'ide-cpp-test-skeleton)

(define-skeleton ide-cpp-mock-skeleton
  "Template for C++ mock files for testing."
  nil
  '(ide-common-insert-snippet "spdx")
  '(ide-common-insert-snippet (ide-cpp-snippet-name "mock-includes"))
  (ide-cpp-private-include)
  (ide-cpp-public-include)
  '(ide-common-insert-snippet "mock-includes")
  '(ide-common-insert-snippet "namespace")
  '(ide-common-insert-snippet (ide-cpp-snippet-name "mock-body")))

(ide-common-register-auto-inserts
 (mapcar (lambda (s) (format "/%s/.*" s)) ide-cpp-available-mock-directories)
 ide-cpp-available-cpp-header-extensions
 'ide-cpp-mock-skeleton)

(define-skeleton ide-cpp-root-cmakelists-skeleton
  "Template for top-level CMakeLists.txt."
  nil
  '(ide-common-insert-snippet "cmake_minimum_required")
  "\n\n"
  '(ide-common-insert-snippet "project")
  "\n\n"
  '(ide-common-insert-snippet "cmake_c_standard")
  '(ide-common-insert-snippet "cmake_cxx_standard")
  "\n"
  '(ide-common-insert-snippet "modules")
  "\n"
  "enable_testing()\n"
  "\n"
  '(ide-common-insert-snippet "subdirs")
  "\n"
  '(ide-common-insert-snippet "apidocs")
  '(ide-common-insert-snippet "userdocs")
  "docs_generate()\n"
  "launch_generate()\n"
  "pack()\n")

(define-skeleton ide-cpp-sub-cmakelists-skeleton
  "Template for sub-level CMakeLists.txt."
  nil
  '(ide-common-insert-snippet "configure")
  '(ide-common-insert-snippet "target")
  '(ide-common-insert-snippet "dependencies")
  '(ide-common-insert-snippet "sources")
  '(ide-common-insert-snippet "headers")
  '(ide-common-insert-snippet "analysis")
  '(ide-common-insert-snippet "installation")
  '(ide-common-insert-snippet "localizations")
  '(ide-common-insert-snippet "subdirs")
  '(ide-common-insert-snippet "apidocs")
  '(ide-common-insert-snippet "userdocs")
  )

(defun ide-cpp-auto-insert-cmakelists ()
  "Auto-insert text in new CMakeLists.txt files."
  (cond
   ((ide-common-is-project-root) (ide-cpp-root-cmakelists-skeleton))
   (t (ide-cpp-sub-cmakelists-skeleton))))

(ide-common-register-auto-inserts
 "CMakeLists"
 ".txt"
 'ide-cpp-auto-insert-cmakelists)

(defun ide-cpp-auto-insert-cmake-graphviz-options ()
  "Auto-insert CMake GraphViz options."
  (yas-expand-snippet (yas-lookup-snippet "graphvizoptions" 'cmake-mode)))

(ide-common-register-auto-inserts
 "CMakeGraphVizOptions"
 ".cmake"
 'ide-cpp-auto-insert-cmake-graphviz-options)

(defun ide-cpp-auto-insert-valgrind-supp ()
  "Auto-insert valgrind suppressions."
  (ide-common-insert-snippet "valgrindsupp" 'text-mode))

(ide-common-register-auto-inserts
 "valgrind"
 ".supp"
 'ide-cpp-auto-insert-valgrind-supp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; actions

(defun ide-cpp-configure (build-type &optional prefix)
  "Configure project.

BUILD-TYPE Debug or Release
With PREFIX, prompt for extra args"
  (let* ((build-tree (ide-cpp-get-build-tree))
         (project-root (ide-common-get-current-context-project-root))
         (command (format "configure-%s" (downcase build-type)))
         (base-args (if (ide-cpp-is-multi-config)
                        (list "-S" "." "-B" build-tree
                              "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
                      (list "-S" "." "-B" build-tree
                            (format "-DCMAKE_BUILD_TYPE=%s" build-type)
                            "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"))))
    ;; install one-shot hook to provide compile-commands.json when ready
    (let (hook-fn)
      (setq hook-fn
            (lambda (_buf _msg)
              (ide-cpp-link-compile-commands build-tree project-root)
              (remove-hook 'compilation-finish-functions hook-fn)))
      (add-hook 'compilation-finish-functions hook-fn))
    ;; run cmake
    (ide-common-run-compile project-root command "cmake" base-args prefix)))

(defun ide-cpp-configure-debug (&optional prefix)
  "Configure project for Debug.  With PREFIX, prompt for extra args."
  (interactive "P")
  (ide-cpp-configure "Debug" prefix))

(defun ide-cpp-configure-release (&optional prefix)
  "Configure project for Release.  With PREFIX, prompt for extra args."
  (interactive "P")
  (ide-cpp-configure "Release" prefix))

(defun ide-cpp-initialize (build-type &optional prefix path build-dir)
  "Initialize project for BUILD-TYPE (\"Debug\" or \"Release\").

If called non-interactively (PATH and BUILD-DIR are provided and PREFIX is nil),
do not prompt.

If called interactively, prompt for BUILD-DIR override.
If called with PREFIX (usually `C-u`), prompt for extra arguments."
  (let* ((project-root (or path (ide-common-get-current-context-project-root)))
         (final-build-tree
          (let* ((default (f-join project-root
                                  (ide-cpp-get-build-tree project-root)))
                 (input (or build-dir
                            (read-directory-name
                             "Build directory: "
                             (f-slash (f-parent default))
                             default
                             nil
                             (f-filename default))))
                 (candidate (f-canonical input)))
            (if (f-descendant-of? candidate project-root)
                (f-relative candidate project-root)
              candidate))))

    ;; phase 1: clear existing build trees
    (cl-labels
        ((cleanup-pass (project-root default)
           (let ((tree (f-join project-root (ide-cpp-get-build-tree project-root default))))
             (when (and tree (f-exists? tree))
               (message "Deleting existing build tree: %s" tree)
               (f-delete tree t)))
           (ide-cpp-buildtree-unset-cache project-root)))
      ;; first pass
      (cleanup-pass project-root nil)
      (cleanup-pass project-root final-build-tree)
      ;; remove compile_commands.json
      (let ((cc (f-join project-root "compile_commands.json")))
        (when (f-exists? cc)
          (message "Removing stale %s" cc)
          (f-delete cc)
          ;; second pass
          (cleanup-pass project-root nil)
          (cleanup-pass project-root final-build-tree))))

    ;; phase 2: create and register new build tree
    (unless (f-exists? final-build-tree)
      (message "Creating new build tree directory: %s" final-build-tree)
      ;; f-join scraps previous path components if final-build-tree is absolute
      ;; so /project-root/ build --> /project-root/build
      ;; but /project-root/ /tmp/build --> /tmp/build
      (f-mkdir (f-join project-root final-build-tree)))
    (ide-cpp-buildtree-set-cache project-root final-build-tree)

    ;; phase 3: configure new build tree
    (message "Initializing fresh %s build tree: %s"
             build-type final-build-tree)
    (pcase build-type
      ("Release"
       (ide-cpp-configure-release prefix))
      (_
       (ide-cpp-configure-debug prefix)))
    (ide-cpp-invalidate-multi-config)))

(defun ide-cpp-initialize-debug (&optional prefix path build-dir)
  "Initialize project for Debug build.

If called programmatically (e.g. from `ide-cpp-create-project'),
PREFIX must be nil and PATH and BUILD-DIR provided.

If called interactively, prompt user for build tree.
With PREFIX (usually `C-u`), prompt for extra arguments"
  (interactive "P")
  (ide-cpp-initialize "Debug" prefix path build-dir))

(defun ide-cpp-initialize-release (&optional prefix)
  "Initialize project for Release build.

Typically invoked interactively by the user.
With PREFIX (usually `C-u`), prompt for extra arguments."
  (interactive "P")
  (ide-cpp-initialize "Release" prefix))

(defun ide-cpp-build (build-type &optional prefix)
  "Build project.

BUILD-TYPE Debug or Release
With PREFIX, prompt for extra args"
  (let* ((build-tree (ide-cpp-get-build-tree))
         (project-root (ide-common-get-current-context-project-root))
         (command (format "build-%s" (downcase build-type)))
         (base-args (if (ide-cpp-multi-config-p)
                        (list "--build" build-tree "--config" build-type)
                      (list "--build" build-tree))))
    ;; run cmake
    (ide-common-run-compile project-root command "cmake" base-args prefix)))

(defun ide-cpp-build-debug (&optional prefix)
  "Build Debug configuration.  With PREFIX, prompt for extra args."
  (interactive "P")
  (ide-cpp-build "Debug" prefix))

(defun ide-cpp-build-release (&optional prefix)
  "Build Release configuration.  With PREFIX, prompt for extra args."
  (interactive "P")
  (ide-cpp-build "Release" prefix))

(defun ide-cpp-install (build-type &optional prefix)
  "Install project.

BUILD-TYPE Debug or Release
With PREFIX, prompt for extra args"
  (let* ((build-tree (ide-cpp-get-build-tree))
         (project-root (ide-common-get-current-context-project-root))
         (command (format "install-%s" (downcase build-type)))
         (base-args (if (ide-cpp-multi-config-p)
                        (list "--install" build-tree "--config" build-type)
                      (list "--install" build-tree))))
    ;; run cmake
    (ide-common-run-compile project-root command "cmake" base-args prefix)))

(defun ide-cpp-install-debug (&optional prefix)
  "Install Debug configuration.  With PREFIX, prompt for extra args."
  (interactive "P")
  (ide-cpp-install "Debug" prefix))

(defun ide-cpp-install-release (&optional prefix)
  "Install Release configuration.  With PREFIX, prompt for extra args."
  (interactive "P")
  (ide-cpp-install "Release" prefix))

(defun ide-cpp-pack (build-type &optional prefix)
  "Package project with cpack.

BUILD-TYPE Debug or Release
With PREFIX, prompt for extra args"
  (let* ((build-tree (ide-cpp-get-build-tree))
         (project-root (ide-common-get-current-context-project-root))
         (command (format "pack-%s" (downcase build-type)))
         (base-args (append (list "--config" (f-join build-tree "CPackConfig.cmake"))
                            (when (ide-cpp-multi-config-p)
                              (list "-C" build-type)))))
    ;; run cmake
    (ide-common-run-compile project-root command "cpack" base-args prefix)))

(defun ide-cpp-pack-debug (&optional prefix)
  "Package Debug configuration with cpack.  With PREFIX, prompt for extra args."
  (interactive "P")
  (ide-cpp-pack "Debug" prefix))

(defun ide-cpp-pack-release (&optional prefix)
  "Package Release configuration with cpack.  With PREFIX, prompt for extra args."
  (interactive "P")
  (ide-cpp-pack "Release" prefix))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; hydra menus

(pretty-hydra-define ide-cpp-hydra-build-single-config
  (:title (concat
           (format "%s C++ Build & Run\n" (all-the-icons-material "build"))
           (propertize
            (format "single-config, %s environment\n" (ide-common-env-get-current-profile (ide-common-get-current-context-project-root)))
            'face 'shadow)
           (propertize
            (substitute-command-keys
             "Press `\\[universal-argument]' to modify arguments") 'face 'shadow))
   :quit-key ("q" "ESC")
   :color teal)
  ("Edit"
   (("s" ide-hydra-spellchecker/body "spelling…")
    ("f" ide-hydra-folding/body "folding…")
    ("l" ide-hydra-inflection/body "inflection…"))
   "Configure"
   (("C" (lambda (arg) (interactive "P") (ide-cpp-configure-release arg)) "Configure rel")
    ("c" (lambda (arg) (interactive "P") (ide-cpp-configure-debug arg)) "Configure dbg")
    ("I" (lambda (arg) (interactive "P") (ide-cpp-initialize-release arg)) "Initialize rel")
    ("i" (lambda (arg) (interactive "P") (ide-cpp-initialize-debug arg)) "Initialize dbg")
    ("e" ide-common-env-select-and-edit "Environment")
    ("a" ide-hydra-appearance/body "appearance…")
    ("v" ide-hydra-behavior/body "behavior…"))
   "Build"
   (("p" (lambda (arg) (interactive "P") (ide-cpp-pack-debug arg)) "Pack")
    ("n" (lambda (arg) (interactive "P") (ide-cpp-install-debug arg)) "Install")
    ("b" (lambda (arg) (interactive "P") (ide-cpp-build-debug arg)) "Build"))
   "Breakpoints"
   (("T" dap-breakpoint-delete-all "Delete all")
    ("t" dap-breakpoint-toggle "toggle"))
   "Launch"
   (("X" ide-common-debug-disconnect-and-delete-all-sessions "Terminate All" :color blue)
    ("x" ide-common-debug-disconnect-all-sessions "Disconnect All")
    ("r" (lambda (arg) (interactive "P") (ide-common-launch-execute arg)) "Run" :color blue)
    ("d" (lambda (arg) (interactive "P") (ide-common-debug-run-debugger arg)) "Debug" :color blue))))

(defun ide-cpp-show-single-config-hydra-p ()
  "Return non-nil if this is a single-config CMake project."
  (and
   ;; is this a cmake project?
   (if (buffer-file-name)
       ide-cpp-cmake-project-p
     (ide-cpp-is-cmake-project))
   ;; does it use a single-config generator?
   (not (ide-cpp-multi-config-p))))

(ide-register-hydra
 'ide-cpp-hydra-build-single-config/body
 #'ide-cpp-show-single-config-hydra-p
 10)

(pretty-hydra-define ide-cpp-hydra-build-multi-config
  (:title (concat
           (format "%s C++ Build & Run\n" (all-the-icons-material "build"))
           (propertize
            (format "multi-config, %s environment\n" (ide-common-env-get-current-profile (ide-common-get-current-context-project-root)))
            'face 'shadow)
           (propertize
            (substitute-command-keys
             "Press `\\[universal-argument]' to modify arguments") 'face 'shadow))
   :quit-key ("q" "ESC")
   :color teal)
  ("Edit"
   (("s" ide-hydra-spellchecker/body "spelling…")
    ("f" ide-hydra-folding/body "folding…")
    ("l" ide-hydra-inflection/body "inflection…"))
  "Configure"
   (("c" (lambda (arg) (interactive "P") (ide-cpp-configure-debug arg)) "Configure")
    ("i" (lambda (arg) (interactive "P") (ide-cpp-initialize-debug arg)) "Initialize")
    ("e" ide-common-env-select-and-edit "Environment")
    ("a" ide-hydra-appearance/body "appearance…")
    ("v" ide-hydra-behavior/body "behavior…"))
   "Build"
   (("P" (lambda (arg) (interactive "P") (ide-cpp-pack-release arg)) "Pack rel")
    ("p" (lambda (arg) (interactive "P") (ide-cpp-pack-debug arg)) "Pack dbg")
    ("N" (lambda (arg) (interactive "P") (ide-cpp-install-release arg)) "Install rel")
    ("n" (lambda (arg) (interactive "P") (ide-cpp-install-debug arg)) "Install dbg")
    ("B" (lambda (arg) (interactive "P") (ide-cpp-build-release arg)) "Build rel")
    ("b" (lambda (arg) (interactive "P") (ide-cpp-build-debug arg)) "Build dbg"))
   "Breakpoints"
   (("T" dap-breakpoint-delete-all "Delete all")
    ("t" dap-breakpoint-toggle "Toggle"))
   "Launch"
   (("X" ide-common-debug-disconnect-and-delete-all-sessions "Terminate All" :color blue)
    ("x" ide-common-debug-disconnect-all-sessions "Disconnect All")
    ("r" (lambda (arg) (interactive "P") (ide-common-launch-execute arg)) "Run" :color blue)
    ("d" (lambda (arg) (interactive "P") (ide-common-debug-run-debugger arg)) "Debug" :color blue))))

(defun ide-cpp-show-multi-config-hydra-p ()
  "Return non-nil if this is a multi-config CMake project."
  (and
   ;; is this a cmake project?
   (if (buffer-file-name)
       ide-cpp-cmake-project-p
     (ide-cpp-is-cmake-project))
   ;; does it use a multi-config generator?
   (ide-cpp-multi-config-p)))

(ide-register-hydra
 'ide-cpp-hydra-build-multi-config/body
 #'ide-cpp-show-multi-config-hydra-p
 20)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; menu

(defvar ide-cpp-build-menu-map (make-sparse-keymap)
  "Keymap for the Build menu minor mode.")

(define-minor-mode ide-cpp-build-menu-mode
  "C++ Build menu for project buffers."
  :group 'ide-cpp
  :keymap ide-cpp-build-menu-map)

(defun ide-cpp-submenu-initialize-items ()
  "Return Initialize submenu variants for the Build menu."
  (list
   ;; multi-config variant
   (list
    :when #'ide-cpp-multi-config-p
    :menu
    '("Initialize"
      ["Initialize" ide-cpp-initialize-debug t]
      ["Initialize…" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-initialize-debug))) t]))
   ;; single-config variant
   (list
    :when (lambda () (not (ide-cpp-multi-config-p)))
    :menu
    '("Initialize"
      ("Initialize"
       ["Debug" ide-cpp-initialize-debug t]
       ["Release" ide-cpp-initialize-release t])
      ("Initialize…"
       ["Debug" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-initialize-debug))) t]
       ["Release" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-initialize-release))) t])))))

(defun ide-cpp-submenu-configure-items ()
  "Return Configure submenu variants for the Build menu."
  (list
   ;; multi-config variant
   (list
    :when #'ide-cpp-multi-config-p
    :menu
    '("Configure"
      ["Configure" ide-cpp-configure-debug t]
      ["Configure…" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-configure-debug))) t]))
   ;; single-config variant
   (list
    :when (lambda () (not (ide-cpp-multi-config-p)))
    :menu
    '("Configure"
      ("Configure"
       ["Debug" ide-cpp-configure-debug t]
       ["Release" ide-cpp-configure-release t])
      ("Configure…"
       ["Debug" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-configure-debug))) t]
       ["Release" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-configure-release))) t])))))

(defun ide-cpp-submenu-build-items ()
  "Return Build submenu variants for the Build menu."
  (list
   ;; multi-config variant
   (list
    :when #'ide-cpp-multi-config-p
    :menu
    '("Build"
      ("Build"
       ["Debug" ide-cpp-build-debug t]
       ["Release" ide-cpp-build-release t])
      ("Build…"
       ["Debug" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-build-debug))) t]
       ["Release" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-build-release))) t])))
   ;; single-config variant
   (list
    :when (lambda () (not (ide-cpp-multi-config-p)))
    :menu
    '("Build"
      ["Build" ide-cpp-build-debug t]
      ["Build…" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-build-debug))) t]))
))

(defun ide-cpp-submenu-install-items ()
  "Return Install submenu variants for the Build menu."
  (list
   ;; multi-config variant
   (list
    :when #'ide-cpp-multi-config-p
    :menu
    '("Install"
      ("Install"
       ["Debug" ide-cpp-install-debug t]
       ["Release" ide-cpp-install-release t])
      ("Install…"
       ["Debug" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-install-debug))) t]
       ["Release" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-install-release))) t])))
   ;; single-config variant
   (list
    :when (lambda () (not (ide-cpp-multi-config-p)))
    :menu
    '("Install"
      ["Install" ide-cpp-install-debug t]
      ["Install…" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-install-debug))) t]))
))

(defun ide-cpp-submenu-pack-items ()
  "Return Pack submenu variants for the Build menu."
  (list
   ;; multi-config variant
   (list
    :when #'ide-cpp-multi-config-p
    :menu
    '("Pack"
      ("Pack"
       ["Debug" ide-cpp-pack-debug t]
       ["Release" ide-cpp-pack-release t])
      ("Pack…"
       ["Debug" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-pack-debug))) t]
       ["Release" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-pack-release))) t])))
   ;; single-config variant
   (list
    :when (lambda () (not (ide-cpp-multi-config-p)))
    :menu
    '("Pack"
      ["Pack" ide-cpp-build-debug t]
      ["Pack…" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-cpp-pack-debug))) t]))
))

(defun ide-cpp-select-menu-items (items)
  "Select menu ITEMS whose :when predicate is non-nil."
  (mapcar
   (lambda (item) (plist-get item :menu))
   (seq-filter (lambda (item) (funcall (plist-get item :when))) items)))

(easy-menu-define ide-cpp-menu-build ide-cpp-build-menu-map
  "C++ build and run menu."
  `("Build/Run"
    ["Environment" ide-common-env-select-and-edit t]
    "---"
    ,@(ide-cpp-select-menu-items (ide-cpp-submenu-initialize-items))
    ,@(ide-cpp-select-menu-items (ide-cpp-submenu-configure-items))
    "---"
    ,@(ide-cpp-select-menu-items (ide-cpp-submenu-build-items))
    ,@(ide-cpp-select-menu-items (ide-cpp-submenu-install-items))
    ,@(ide-cpp-select-menu-items (ide-cpp-submenu-pack-items))
    "---"
    ("Debug"
     ["Debug" ide-common-debug-run-debugger t]
     ["Debug…" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-common-debug-run-debugger))) t])
    ("Run"
     ["Run" ide-common-launch-execute t]
     ["Run…" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'ide-common-launch-execute))) t])
    "---"
    ["Delete All Breakpoints" dap-breakpoint-delete-all t]
    ["Toggle Breakpoint" dap-breakpoint-toggle t]
    "---"
    ["Terminate All Sessions" ide-common-debug-disconnect-and-delete-all-sessions t]
    ["Disconnect All Sessions" ide-common-debug-disconnect-all-sessions t]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; bootstrap

;; register known C++ debuggers and their DAP adapters
(with-eval-after-load 'ide-common-debug
  ;; GDB adapters
  (ide-common-debug-register-adapter
   "GDB" "gdb" "gdb" 'dap-gdb
   "GNU Debugger with dap-gdb")
  (ide-common-debug-register-adapter
   "GDB" "gdb" "cpptools" 'dap-cpptools
   "GNU Debugger with dap-cpptools (VSCode C++ backend)")
  (ide-common-debug-register-adapter
   "GDB" "gdb" "gdb" 'dap-gdb-lldb
   "GNU Debugger with dap-gdb-lldb")

  ;; LLDB adapters
  (ide-common-debug-register-adapter
   "LLDB" "lldb" "lldb-vscode" 'dap-lldb
   "LLVM Debugger with dap-lldb")
  (ide-common-debug-register-adapter
   "LLDB" "lldb" "lldb-mi" 'dap-gdb-lldb
   "LLVM Debugger with dap-gdb-lldb")

  ;; MSVC/Windows adapters
  (ide-common-debug-register-adapter
   "MSVC" "msvcdbg.exe" "cppvsdbg" 'dap-cpptools
   "Microsoft Visual C++ debugger with dap-cpptools"))

;; add C++ project creation item to treemacs context menu
(with-eval-after-load 'treemacs-mouse-interface
  (with-eval-after-load 'ide-common
    (add-to-list
     (with-no-warnings 'treemacs--mouse-project-list-functions)
     '("Create new C++ project" . ide-cpp-project-mouse-selection-menu)
     :append)))

;; load persistent cache
(ide-cpp-load-all)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end of file

(provide 'ide-cpp)
;;; ide-cpp.el ends here
