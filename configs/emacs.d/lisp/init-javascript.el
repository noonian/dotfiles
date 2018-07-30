;;; init-javascript.el --- JavaScript config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rjsx-mode
  :ensure t
  :defer 30
  :init
  ;; Indent body of js switch statement
  (setq js-switch-indent-offset 2)
  ;; Indent 2 spaces
  (setq js-indent-level 2)
)

(provide 'init-javascript)
;;; init-javascript.el ends here
