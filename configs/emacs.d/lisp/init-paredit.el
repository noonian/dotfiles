;;; init-paredit.el --- Paredit configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package paredit
  :straight t
  :commands (paredit-mode)
  :delight
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)))

(provide 'init-paredit)
;;; init-paredit.el ends here
