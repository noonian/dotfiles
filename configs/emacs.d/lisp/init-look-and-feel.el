;;; init-look-and-feel.el --- Configure visual aesthetic -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :demand t
  :delight
  (auto-revert-mode)
  (text-scale-mode)
  (eldoc-mode)
  (emacs-lisp-mode "elisp" :major)
  (lisp-interaction-mode "elisp-interaction" :major)
  :init
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (setq inhibit-startup-message t)
  (setq echo-keystrokes 0.1
	use-dialog-box nil
	visible-bell t)
  ;; (setq	ring-bell-function 'ignore)
)

(use-package autorevert
  :delight auto-revert-mode)

(use-package linum
  :defer t
  :commands (linum-mode)
  :config
  (set-face-attribute 'linum nil :height 140))

(use-package sublime-themes
  ;; :disabled t
  :straight t
  :config
  ;; (load-theme 'white-sand t)
  ;; (load-theme 'wheatgrass t)
  ;; (load-theme 'brin t)
  ;; (load-theme 'hickey t)
  ;; (load-theme 'fogus t)
  ;; (load-theme 'graham t)
  ;; (load-theme 'granger t)
  (load-theme 'odersky t)
  ;; (load-theme 'dorsey t)
  ;; (load-theme 'mccarthy t)
  ;; (load-theme 'wilson t)
  ;; (load-theme 'junio t)
  ;; (load-theme 'spolsky t)
  ;; (load-theme 'ritchie t)

  ;; Make highlighted region readable in themes with poor default
  ;; (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
  )

(use-package powerline
  :straight t
  :config
  (powerline-default-theme)
  ;; (powerline-center-theme)
  ;; (powerline-center-evil-theme)
  ;; (powerline-vim-theme)
  ;; (powerline-nano-theme)
  )

(provide 'init-look-and-feel)
;;; init-look-and-feel.el ends here
