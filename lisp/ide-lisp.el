;;; ide-lisp.el --- support for lisp -*- lexical-binding: t; -*-

;; Version: 0.0.1

;; Copyright (C) 2022, 2024 Stefan Christians
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; settings and functions to support editing Emacs Lisp files

;;; Code:

(require 'yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; auto-inserts

(defun ide-lisp-auto-insert-lisp ()
  "Auto-insert header in new Lisp files."
  (yas-expand-snippet (yas-lookup-snippet "header" 'emacs-lisp-mode))
  (setq lexical-binding t))

(define-auto-insert "\\.el\\'" [ide-lisp-auto-insert-lisp])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end of file

(provide 'ide-lisp)
;;; ide-lisp.el ends here
