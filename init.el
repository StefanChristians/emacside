;;; init.el --- Emacs initialization -*- lexical-binding: t; -*-

;; Version: 0.0.1

;; Copyright (C) 2022, 2023, 2024, 2025 Stefan Christians
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Personal Emacs settings and configuration

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; customization cache

;; use external file to cache custom-set-variables and custom-set-faces
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file) (write-region "" nil custom-file))
(load custom-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; package management

;; initialize repositories
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://table.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

;; install use-package macro
(unless (package-installed-p 'use-package) (package-refresh-contents) (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; automatically update packages
(use-package auto-package-update
  :ensure t
  :hook (
	 (auto-package-update-before . (lambda () (message "starting package update")))
	 (auto-package-update-after . (lambda () (message "done updating packages")))
	 )
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  )

;; allow installation of packages from source
(use-package quelpa
  :ensure t
  :init
  (setq quelpa-update-melpa-p nil)
  )

;; add :quelpa keyword to use-package macro
(use-package quelpa-use-package :ensure t)

;; add :diminish keyword to use-package macro
(use-package diminish :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; base settings

;; remove title (Mr., Mrs., Ms.) from user name
(setq user-full-name (replace-regexp-in-string "^M[rs]+\.\s*" "" user-full-name))

;; increase garbage collection threshold
(setq gc-cons-threshold (* 64 (* 1024 1024)))

;; use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set default encoding to UTF-8
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)

;; Use UTF-8 for file names
(set-file-name-coding-system 'utf-8)

;; Automatically save files with UTF-8 encoding
(add-hook 'before-save-hook (lambda () (set-buffer-file-coding-system 'utf-8)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; packages

;;;; packages AAAAAAAAAAAAAAAAAAAAAAAAAA

;; all-the-icons
;; A utility package to collect various Icon Fonts and propertize
;; them within Emacs
(use-package all-the-icons
  :ensure t
  :init
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

;; adoc-mode
;; A major-mode for editing AsciiDoc files in Emacs
(use-package adoc-mode :ensure t)

;; asm-mode
;; mode for editing assembler code
(use-package asm-mode :ensure nil)

;; async
;; Simple library for asynchronous processing in Emacs
(use-package async :ensure t)

;; autoinsert
;; automatic mode-dependent insertion of text into new files
(use-package autoinsert
  :ensure nil
  :commands (auto-insert)
  :hook (find-file . auto-insert)
  :custom
  (auto-insert-query nil)
  (auto-insert-alist nil)
  :init
  (auto-insert-mode t))

;; avy
;; Jump to things in Emacs tree-style
(use-package avy
  :ensure t
  :bind (("M-c" . avy-goto-char-2)
         ("M-w" . avy-goto-word-1))
  :init
  (avy-setup-default))

;;;; packages BBBBBBBBBBBBBBBBBBBBBBBBBB

;; beacon
;; A light that follows your cursor around so you don't lose it
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode t))

;; button
;; clickable buttons
(use-package button :ensure nil)

;;;; packages CCCCCCCCCCCCCCCCCCCCCCCCCC

;; cc-mode
;; major mode for editing C and similar languages
(use-package cc-mode :ensure nil)

;; clang-format+
;; Emacs minor mode for automatic clang-format application
(use-package clang-format+
  :diminish clang-format+-mode
  :hook (c-mode-common . clang-format+-mode)
  :quelpa (clang-format+
           :fetcher github
           :repo "SavchenkoValeriy/emacs-clang-format-plus"))

;; cmake-font-lock
;; Emacs Font-lock rules for CMake
(use-package cmake-font-lock
    :ensure t
    :after cmake-mode)

;; cmake-mode
;; major-mode for editing CMake sources
(use-package cmake-mode
  :ensure t
  :defines company-backends
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook (cmake-mode . (lambda () (add-to-list 'company-backends 'company-cmake))))

;; company
;; Modular in-buffer completion framework
(use-package company
  :ensure t
  :diminish company-mode
  :hook (prog-mode . global-company-mode)
  :commands (company-mode company-indent-or-complete-common)
  :bind (:map company-active-map
              ("ESC" . company-abort))
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 14)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  (company-global-modes '(not erc-mode message-mode help-mode gud-mode))
  (company-auto-complete nil)
  (company-auto-complete-chars nil)
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil))

;; company-box
;; A company front-end with icons
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; company-prescient
;; sort using prescient
(use-package company-prescient
  :ensure t
  :after (company prescient)
  :config
  (company-prescient-mode t))

;; company-yasnippet
;; company-mode completion backend for Yasnippet
(use-package company-yasnippet
  :bind ("C-M-y" . company-yasnippet)
  :after (yasnippet company)
  :custom
  (yas-triggers-in-field t))

;; consult
;; Consulting completing-read
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("M-y"   . consult-yank-pop)
         ("C-s"   . consult-line)
         ("C-c r" . consult-ripgrep)
         ("C-c l" . consult-line-multi)
         ("C-x C-r" . consult-recent-file)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)))

;; copyright
;; update the copyright notice in current buffer
(use-package copyright
  :ensure nil
  :custom
  (copyright-name-regexp user-full-name)
  :hook (before-save . copyright-update))

;; cua-base
;; emulate CUA key bindings (cut/copy/paste/undo)
(use-package cua-base
  :ensure nil
  :custom
  (cua-auto-tabify-rectangles nil)
  (cua-keep-region-after-copy t)
  :config
  (cua-mode t))

;;;; packages DDDDDDDDDDDDDDDDDDDDDDDDDD

;; dash
;; A modern list library for Emacs
(use-package dash :ensure t)

;; default-text-scale
;; Easily adjust the font size in all Emacs frames
(use-package default-text-scale :ensure t)

;; delsel
;; delete selection if you insert
(use-package delsel
  :ensure nil
  :custom
  (delete-selection-mode t))

;; diff-hl
;; Emacs package for highlighting uncommitted changes
(use-package diff-hl
  :ensure t
  :hook ((prog-mode org-mode) . diff-hl-mode)
  :config
  (diff-hl-flydiff-mode 1))

;; display-fill-column-indicator
;; interface for display-fill-column-indicator
(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator-column 80)
  :hook ((prog-mode . display-fill-column-indicator-mode)))

;; doom-modeline
;; A fancy and fast mode-line inspired by minimalism design
(use-package doom-modeline
  :ensure t
  :after (doom-themes)
  :config
  (doom-modeline-mode 1))

;; doom-themes
;; A megapack of themes for GNU Emacs
(use-package doom-themes
  :ensure t
  :after (all-the-icons major-mode-hydra)
  :custom
  (doom-themes-treemacs-theme "doom-colors")
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)

  ;; remove hide-modeline hook from doom-themes-treemacs-config
  (with-eval-after-load 'treemacs
    (remove-hook 'treemacs-mode-hook #'doom-themes-hide-modeline))

  ;; load and toggle themes
  (defvar light-theme 'doom-acario-light "Name of light theme.")
  (defvar dark-theme 'doom-acario-dark "Name of dark theme.")
  (defvar dark-theme-toggle t "None-nil if theme is dark.")
  (defun dark-theme-toggle ()
    "Toggle theme between light and dark modes."
    (interactive)
    (setq dark-theme-toggle (not dark-theme-toggle))
    (let ((theme (if dark-theme-toggle dark-theme light-theme)))
      (if (custom-theme-enabled-p theme)
          (enable-theme theme)
        (load-theme theme t nil))))
  (dark-theme-toggle))

;;;; packages EEEEEEEEEEEEEEEEEEEEEEEEEE

;; emacs
;; c source code
(use-package emacs
  :ensure nil
  :defer
  :custom
  (c-basic-offset 2)
  (case-fold-search t)
  (fill-column 80)
  (help-window-select t)
  (indent-tabs-mode nil)
  (indicate-empty-lines t)
  (menu-bar-mode t)
  (ring-bell-function 'ignore)
  (tab-width 2)
  (tool-bar-mode nil)
  (transient-mark-mode t)
  (x-stretch-cursor t)
  (read-process-output-max (* 3 (* 1024 1024))))

;; emacs-clang-rename
;; Emacs package to call clang-rename
(use-package emacs-clang-rename
  :quelpa (emacs-clang-rename
           :fetcher github
           :repo "nilsdeppe/emacs-clang-rename"))

;; emacs-gdb
;; GDB graphical interface for GNU Emacs
;; drop-in replacement for internal gdb-mi
(use-package gdb-mi
  :bind ("C-c d" . gdb-executable)
  :quelpa (gdb-mi :fetcher git
                  :url "https://github.com/weirdNox/emacs-gdb.git"
                  :files ("*.el" "*.c" "*.cpp" "*.h" "*.hpp" "Makefile"))
  :init
  (fmakunbound 'gdb)
  (fmakunbound 'gdb-enable-debug))

;; embark
;; Emacs Mini-Buffer Actions Rooted in Keymaps
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; embark-consult
;; Consult integration for Embark
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; packages FFFFFFFFFFFFFFFFFFFFFFFFFF

;; f
;; Modern API for working with files and directories
(use-package f :ensure t)

;; files
;; file input and output commands for Emacs
(use-package files
  :ensure nil
  :hook
  ;; make shell scripts (with shebang) executable on saving
  (after-save . executable-make-buffer-file-executable-if-script-p)
  :custom
  (auto-save-default nil)
  (make-backup-files nil))

;; find-file-in-project
;; Quick access to project files in Emacs
(use-package find-file-in-project
  :ensure t
  :config
  (setq ffip-use-rust-fd t))

;; flycheck
;; On the fly syntax checking for GNU Emacs
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :config
  (global-flycheck-mode t))

;; flyspell
;; on-the-fly spell checker
(use-package flyspell
  :ensure t
  :commands flyspell-goto-next-error
  :diminish flyspell-mode
  :hook (((text-mode org-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (flyspell-issue-welcome-flag nil))

;; flyspell-correct
;; Distraction-free words correction with flyspell via selected interface
(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-'" . flyspell-correct-wrapper)))

;; flyspell-correct-popup
;; Correcting words with flyspell via popup interface
(use-package flyspell-correct-popup
  :ensure t
  :after flyspell-correct)

;; font-lock
;; Electric font lock mode
(use-package font-lock
  :ensure nil
  :hook
  (prog-mode .
      (lambda () (font-lock-add-keywords nil
      '(("\\<\\(FIXME\\|TODO\\|DEBUG\\|INPROGRESS\\|CONTINUE HERE\\|DONE\\)"
         1 font-lock-warning-face t))))))

;; frame
;; multi-frame management independent of window systems
(use-package frame
  :ensure nil
  :bind (("C-M-0" . frame-transparency-toggle))
  :config
  (defun frame-transparency-state ()
    "Return non-nil if the frame is transparent."
    (let* ((pair (or (frame-parameter nil 'alpha) '(100 100)))
           (alpha (apply '+ pair)))
      (not (or (null alpha) (eq alpha 200) (eq alpha 2.0)))))
  (defun frame-transparency-toggle ()
    "Toggle frame transparency."
    (interactive)
    (set-frame-parameter
     nil 'alpha
     (if (frame-transparency-state) '(100 100) '(60 30)))))

;;;; packages GGGGGGGGGGGGGGGGGGGGGGGGGG

;; gitignore-snippets
;; gitignore.io templates for yasnippet
(use-package gitignore-snippets
  :ensure t
  :after yasnippet
  :init
  (gitignore-snippets-init))

;; git-modes
;; Emacs major modes for Git configuration files
(use-package git-modes :ensure t)

;; git-timemachine
;; Walk through git revisions of a file
(use-package git-timemachine
  :ensure t
  :bind (("M-g M-t" . git-timemachine)))

;;;; packages HHHHHHHHHHHHHHHHHHHHHHHHHH

;; hl-line
;; highlight the current line
(use-package hl-line
  :ensure nil
  :custom
  (global-hl-line-mode t))

;; hydra
;; make Emacs bindings that stick around
(use-package hydra :ensure t)

;;;; packages IIIIIIIIIIIIIIIIIIIIIIIIII

;; ibuffer
;; operate on buffers like dired
(use-package ibuffer
  :ensure nil
  :bind (([remap list-buffers] . ibuffer)))

;; isearch
;; incremental search minor mode
(use-package isearch
  :ensure nil
  :bind (("C-f" . isearch-forward)
         :map isearch-mode-map
         ("C-f" . isearch-repeat-forward))
  :custom
  (search-highlight t))

;;;; packages JJJJJJJJJJJJJJJJJJJJJJJJJJ

;; json-mode
;; Major mode for editing JSON files with emacs
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" "\\.imp\\'"))

;;;; packages KKKKKKKKKKKKKKKKKKKKKKKKKK

;;;; packages LLLLLLLLLLLLLLLLLLLLLLLLLL

;; license-snippets
;; LICENSE templates for yasnippet
(use-package license-snippets
  :ensure t
  :after yasnippet
  :init
  (license-snippets-init))

;; linum
;; display line numbers in the left margin
(use-package linum
  :ensure nil
  :custom
  (global-linum-mode nil))

;; loop
;; friendly imperative loop structures for Emacs lisp
(use-package loop :ensure t)

;; lsp-mode
;; Emacs client/library for the Language Server Protocol
(use-package lsp-mode
  :ensure t
  :diminish lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-restart 'ignore)
  :hook ((c-mode-common . lsp)
         (shell-mode . lsp)
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  ;; suppress semgrep/rulesRefreshed warning popup
  ;; solution by Arun Kumar Khattri
  ;; https://emacs.stackexchange.com/questions/81247/with-lsp-mode-why-do-i-get-an-unknown-notification-about-refreshed-rules-from-s
  (defun ak-lsp-ignore-semgrep-rulesRefreshed (workspace notification)
    "Ignore semgrep/rulesRefreshed notification."
    (when (equal (gethash "method" notification) "semgrep/rulesRefreshed")
      (lsp--info "Ignored semgrep/rulesRefreshed notification")
      t)) ;; Return t to indicate the notification is handled
  (advice-add 'lsp--on-notification :before-until #'ak-lsp-ignore-semgrep-rulesRefreshed))

;; lsp-treemacs
;; lsp-mode loves treemacs
(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :init
  (lsp-treemacs-sync-mode 1))

;; lsp-ui
;; UI integrations for lsp-mode
(use-package lsp-ui
  :ensure t
  :diminish lsp-ui-mode
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ("C-c u" . lsp-ui-imenu)))

;; lsp-origami
;; origami support for lsp mode
(use-package lsp-origami
  :ensure t
  :after (origami lsp-mode)
  :hook (lsp-after-open . lsp-origami-try-enable))

;;;; packages MMMMMMMMMMMMMMMMMMMMMMMMMM

;; magit
;; It's Magit! A Git porcelain inside Emacs
(use-package magit
  :ensure t
  :after vertico
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g" . magit-file-dispatch))
  :custom
  (magit-define-global-key-bindings nil))

;; major-mode-hydra
;; Spacemacs-esque major mode leader key powered by Hydra
(use-package major-mode-hydra
  :ensure t
  :after (hydra dash s))

;; marginalia
;; Marginalia in the minibuffer
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; markdown-mode
;; Emacs Markdown Mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.markdown\\'"))

;; modern-cpp-font-lock
;; C++ font-lock for Emacs
(use-package modern-cpp-font-lock
  :ensure t
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode)
  :config
  (modern-c++-font-lock-global-mode t))

;; mouse
;; window system-independent mouse support
(use-package mouse
  :ensure nil
  :config
  (context-menu-mode 1))

;;;; packages NNNNNNNNNNNNNNNNNNNNNNNNNN

;;;; packages OOOOOOOOOOOOOOOOOOOOOOOOOO

;; org
;; Outline-based notes management and organizer
(use-package org
  :ensure nil
  :custom
  (org-log-done 'time)
  (org-todo-keywords '((sequence "FIXME" "TODO" "DEBUG" "INPROGRESS" "CONTINUE HERE" "DONE")))
  (org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
  (org-support-shift-select t))

;; origami
;; A folding minor mode for Emacs
(use-package origami
  :ensure t
  :hook (prog-mode . origami-mode))

;; ox-md
;; Markdown Back-End for Org Export Engine
(use-package ox-md :ensure nil)

;; ox-asciidoc
;; AsciiDoc Back-End for Org Export Engine
(use-package ox-asciidoc :ensure t)

;;;; packages PPPPPPPPPPPPPPPPPPPPPPPPPP

;; paren
;; highlight matching paren
(use-package paren
  :ensure nil
  :custom
  (show-paren-mode t))

;; po-mode
;; Major mode for GNU gettext PO files
(use-package po-mode
  :ensure t
  :mode ("\\.po\\'\\|\\.po\\." . po-mode))

;; prescient
;; Simple but effective sorting and filtering for Emacs
(use-package prescient
  :ensure t
  :functions prescient-persist-mode
  :config
  (prescient-persist-mode t))

;;;; packages QQQQQQQQQQQQQQQQQQQQQQQQQQ

;;;; packages RRRRRRRRRRRRRRRRRRRRRRRRRR

;; rainbow-delimiters
;; color highlighted delimiters by nesting level
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; recentf
;; keep track of recently opened files
(use-package recentf
  :ensure nil
  :bind ("C-x C-r" . recentf-open-files)
  :custom
  (recentf-max-menu-items 20)
  (recentf-max-saved-items 20)
  :init
  (recentf-mode 1))

;; request
;; Easy HTTP request for Emacs Lisp
(use-package request :ensure t)

;; rg
;; Emacs search tool based on ripgrep
(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

;;;; packages SSSSSSSSSSSSSSSSSSSSSSSSSS

;; s
;; The long lost Emacs string manipulation library
(use-package s :ensure t)

;; scroll-bar
;; window system-independent scroll bar support
(use-package scroll-bar
  :ensure nil
  :custom
  ;; disable scroll bars
  (scroll-bar-mode nil)
  )

;; seq
;; Sequence manipulation functions
(use-package seq :ensure nil)

;;; simple
;; basic editing commands for Emacs
(use-package simple
  :ensure nil
  :hook
  ;; no wrapping in prog-mode
  (prog-mode . (lambda () (auto-fill-mode -1)))
  :custom
  ;; display column number in status bar
  (column-number-mode t)
  ;; globally turn on word-wrapping mode
  (turn-on-auto-fill))

;; smart-hungry-delete
;; An emacs package to delete whitespace between words,
;; parenthesis and other delimiters in a (not very) smart way
(use-package smart-hungry-delete
  :ensure t
  :bind (("C-<backspace>" . smart-hungry-delete-backward-char)
         ("C-<delete>" . smart-hungry-delete-forward-char))
  :init
  (smart-hungry-delete-add-default-hooks))

;; spdx
;; Insert SPDX license and copyright headers
(use-package spdx
  :ensure t
  :custom
  (spdx-project-detection nil)
  )

;; startup
;; process Emacs shell arguments
(use-package startup
  :ensure nil
  :defer
  :hook
  ;; don't split window when opening file from command line
  (emacs-startup . (lambda () (when (> (length (window-list)) 2) (delete-other-windows))))
  :custom
  (inhibit-startup-screen t))

;; string-inflection
;; underscore -> UPCASE -> CamelCase conversion of names
(use-package string-inflection :ensure t)

;;;; packages TTTTTTTTTTTTTTTTTTTTTTTTTT

;; text-mode
;; text mode, and its idiosyncratic commands
(use-package text-mode
  :ensure nil
  :mode ("LICENSE\\'" "README\\'"))

;; treemacs
;; a tree layout file explorer for Emacs
(use-package treemacs
  :ensure t
  :custom
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-commit-diff-mode t)
  ;; display workspace treemacs modeline and use mouse to switch workspace
  (treemacs-user-mode-line-format
   (with-no-warnings (eval '(progn
                           (require 'doom-modeline)
                           (doom-modeline-def-segment treemacs-workspace-name
                             "Display treemacs workspace."
                             (propertize (format "%s" (treemacs-workspace->name (treemacs-current-workspace)))
                                         'face (doom-modeline-face 'doom-modeline-buffer-major-mode)
                                         'mouse-face 'doom-modeline-highlight
                                         'help-echo "Workspace name\nmouse-1: Switch workspace"
                                         'local-map (make-mode-line-mouse-map 'mouse-1 #'treemacs-switch-workspace)))
                           (doom-modeline-def-modeline 'treemacs '(bar treemacs-workspace-name))
                           (doom-modeline 'treemacs)))))
  :config
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'deferred)
  (treemacs-hide-gitignored-files-mode 1)

  ;; add treemacs context menu to context-menu-mode
  (add-hook 'context-menu-functions #'treemacs-rightclick-menu)

  ;; start with treemacs window visible
  (treemacs))

;; treemacs-icons-dired
;; Treemacs icons for dired
(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; treemacs-magit
;; Magit integration for treemacs
(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

;;;; packages UUUUUUUUUUUUUUUUUUUUUUUUUU

;; undo-tree
;; visualize undo states as graphical tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

;; uuid
;; The UUID module from the EmacsWiki
(use-package uuid :ensure t)

;;;; packages VVVVVVVVVVVVVVVVVVVVVVVVVV

;; vc-hooks
;; resident support for version-control
(use-package vc-hooks
  :ensure nil
  :custom
  (vc-follow-symlinks t))

;; vertico
;; VERTical Interactive COmpletion
(use-package vertico
  :ensure t
  :init
  (vertico-mode t))

;; vertico-mouse
;; support for scrolling and candidate selection
(use-package vertico-mouse
  :ensure nil
  :after vertico
  :config
  (vertico-mouse-mode t))

;; vertico-prescient
;; prescient integration with vertico
(use-package vertico-prescient
  :ensure t
  :after (prescient vertico)
  :config
  (vertico-prescient-mode t))

;; vlf
;; View Large Files in Emacs
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

;;;; packages WWWWWWWWWWWWWWWWWWWWWWWWWW

;; warnings
;; log and display warnings
(use-package warnings
  :ensure nil
  :config
  (add-to-list 'warning-suppress-log-types '(yasnippet backquote-change)))

;; web-mode
;; web template editing mode for emacs
(use-package web-mode
  :ensure t
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'"))

;; which-func
;; print current function in mode line
(use-package which-func
  :ensure nil
  :custom
  (which-function-mode t))

;; which-key
;; Emacs package that displays available keybindings in popup
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-right-bottom))

;; window
;; GNU Emacs window commands aside from those written in C
(use-package window
  :ensure nil
  :bind (([remap split-window-below] .
          (lambda () (interactive)(split-window-below) (other-window 1)))
         ([remap split-window-right] .
          (lambda () (interactive)(split-window-right) (other-window 1)))))

;; winum
;; Navigate windows and frames using numbers
(use-package winum
  :ensure t
  :after treemacs
  :bind (:map winum-keymap
              ("M-0" . treemacs-select-window)
              ("M-1" . winum-select-window-1)
              ("M-2" . winum-select-window-2)
              ("M-3" . winum-select-window-3)
              ("M-4" . winum-select-window-4)
              ("M-5" . winum-select-window-5)
              ("M-6" . winum-select-window-6)
              ("M-7" . winum-select-window-7)
              ("M-8" . winum-select-window-8)
              ("M-9" . winum-select-window-9))
  :custom
  (winum-scope 'frame-local)
  (winum-mode t))

;; writegood-mode
;; Minor mode for Emacs to improve English writing
(use-package writegood-mode
  :ensure t
  :hook ((org-mode . writegood-mode)
         (adoc-mode . writegood-mode)))

;; ws-butler
;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :hook ((prog-mode . ws-butler-mode)))

;;;; packages XXXXXXXXXXXXXXXXXXXXXXXXXX

;;;; packages YYYYYYYYYYYYYYYYYYYYYYYYYY

;; yasnippet
;; a template system for Emacs
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands (yas-minor-mode-on)
  :functions yas-reload-all
  :hook (((text-mode prog-mode conf-mode snippet-mode) . yas-minor-mode-on)
         (snippet-mode . (lambda () (setq-local require-final-newline nil))))
  :bind ("C-c C-s" . yas-insert-snippet)
  :config
  (yas-reload-all))

;; yasnippet-snippets
;; a collection of yasnippet snippets for many languages
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;;; packages ZZZZZZZZZZZZZZZZZZZZZZZZZZ

;; zzz-to-char
;; Fancy replacement for zap-to-char in Emacs
(use-package zzz-to-char
  :ensure t
  :bind (("M-z" . zzz-up-to-char)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; hydra menus

;; dispatcher
(pretty-hydra-define dispatcher
  (:title (format "%s Main Dispatcher" (all-the-icons-faicon "keyboard-o"))
          :exit t :foreign-keys warn :quit-key ("q" "ESC"))
  ("Menus"
   (("a" appearance/body "appearance...")
    ("b" behavior/body "behavior..."))

   "Editing"
   (("s" spellchecker/body "spellchecker...")
    ("f" folding/body "folding...")
    ("i" inflection/body "inflection..."))))
(bind-key "C-c h" 'dispatcher/body)

;; appearance
(pretty-hydra-define appearance
  (:title (format "%s Appearance" (all-the-icons-faicon "desktop"))
          :exit nil :foreign-keys warn :quit-key ("q" "ESC"))
  ("Theme"
   (("d" dark-theme-toggle "dark mode" :toggle t)
    ("0" frame-transparency-toggle "transparency" :toggle (frame-transparency-state))
    ("+" default-text-scale-increase "zoom in")
    ("=" default-text-scale-reset "zoom reset")
    ("-" default-text-scale-decrease "zoom out"))

   "Window"
   (("x" toggle-frame-maximized "maximize" :toggle t)
    ("f" toggle-frame-fullscreen "full screen" :toggle t)
    ("," treemacs "treemacs"))

   "UI"
   (("m" menu-bar-mode "menu" :toggle t)
    ("t" tool-bar-mode "toolbar" :toggle t)
    ("s" scroll-bar-mode "scrollbars" :toggle t)
    ("c" column-number-mode "column number" :toggle t)
    ("l" global-linum-mode "line numbers" :toggle t)
    ("e" (setq-local indicate-empty-lines (not indicate-empty-lines)) "empty lines" :toggle indicate-empty-lines))

   "Content"
   (("h" global-hl-line-mode "highlight line" :toggle t)
    ("b" beacon-mode "beacon" :toggle t)
    ("p" show-paren-mode "parentheses" :toggle t)
    ("r" rainbow-delimiters-mode "rainbow parens" :toggle t)
    ("_" (setq x-stretch-cursor (not x-stretch-cursor)) "stretch cursor" :toggle x-stretch-cursor))))

;; behavior
(pretty-hydra-define behavior
  (:title (format "%s Behavior" (all-the-icons-faicon "cogs"))
          :exit nil :foreign-keys warn :quit-key ("q" "ESC"))
  ("White space"
   (("f" (setq-local require-final-newline (not require-final-newline)) "add final newline" :toggle require-final-newline)
    ("t" ws-butler-mode "remove trailing white space" :toggle t))))

;; spellchecker
(pretty-hydra-define spellchecker
  (:title (format "%s Spellchecker" (all-the-icons-faicon "check-circle-o"))
          :exit nil :foreign-keys nil :quit-key ("q" "ESC"))
  ("Check Spelling"
   (("b" flyspell-buffer "buffer")
    ("r" flyspell-region "region"))

   "Correct Words"
   (("p" flyspell-correct-previous "previous word")
    ("." flyspell-auto-correct-at-point "word at point")
    ("n" flyspell-correct-next "next word"))))

;; folding
(pretty-hydra-define folding
  (:title (format "%s Folding" (all-the-icons-faicon "level-down"))
          :exit nil :foreign-keys nil :quit-key ("q" "ESC"))
  ("Fold Code"
   (("t" origami-recursively-toggle-node "toggle")
    ("s" origami-show-only-node "show node")
    ("o" origami-open-all-nodes "open all")
    ("c" origami-close-all-nodes "close all")
    ("p" origami-previous-fold "previous")
    ("n" origami-next-fold "next")
    ("u" origami-undo "undo")
    ("r" origami-redo "redo"))))

;; inflection
(pretty-hydra-define inflection
  (:title (format "%s Inflection" (all-the-icons-faicon "language"))
          :exit nil :foreign-keys nil :quit-key ("q" "ESC"))
  ("String inflection"
   (("i" string-inflection-all-cycle "cycle")
    ("c" string-inflection-lower-camelcase "camelCase")
    ("p" string-inflection-camelcase "PascalCase")
    ("-" string-inflection-kebabcase "kebab-case")
    ("_" string-inflection-underscore "snake_case")
    ("u" string-inflection-upcase "UPPER_CASE"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; extensions

;; add private extensions to load path
(defvar extensions-dir (expand-file-name "lisp" user-emacs-directory))
(unless (file-directory-p extensions-dir) (make-directory extensions-dir))
(push extensions-dir load-path)

(use-package ide-common
  :demand
  :bind ("C-c y" . ide-common-yassify))

(use-package ide-lisp)

(use-package ide-cpp
  :custom
  (ide-common-default-project-parent "~/development/sandbox/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end of file

(provide 'init)
;;; init.el ends here
