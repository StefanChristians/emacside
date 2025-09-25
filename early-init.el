;;; early-init.el --- Emacs pre-initialization -*- lexical-binding: t; -*-

;; Version: 0.0.1

;; Copyright (C) 2022 Stefan Christians
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; startup configuration before init file is processed

;;; Code:

;; suppress cl deprecation warning
(setq byte-compile-warnings '(cl-functions))

;; start with wider window in top right corner
(setq initial-frame-alist '((top . 0) (left . (- 0)) (width . 120)))

(provide 'early-init)
;;; early-init.el ends here
