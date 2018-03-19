;;; init-look-and-feel.el --- Configure visual aesthetic -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'init-global-functions)

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

  (add-hook 'after-change-major-mode-hook (lambda () (text-scale-set 1)))

  (let ((input-mono "Input Mono 16")
	(inconsolata "Inconsolata 18")
	(menlo "Menlo"))
    (cond
     ((my/font-installed? input-mono) (set-frame-font input-mono nil t))
     ((my/font-installed? inconsolata) (set-frame-font inconsolata nil t))
     (t menlo))))

;; (set-frame-font "Input Mono 16" nil t)
;; (set-frame-font "Inconsolata 18" nil t)

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
  (load-theme 'fogus t)
  ;; (load-theme 'graham t)
  ;; (load-theme 'granger t)
  ;; (load-theme 'odersky t)
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

(use-package doom-themes
  :straight t
  :config
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-one-light t)
  ;; (load-theme 'doom-citylights t)
  ;; (load-theme 'doom-darcula t)
  ;; (load-theme 'doom-molokai t)
  ;; (load-theme 'doom-nord t)
  ;; (load-theme 'doom-nova t)
  ;; (load-theme 'doom-peacock t)
  ;; (load-theme 'doom-solarized-light t)
  ;; (load-theme 'doom-spacegrey t)
  ;; (load-theme 'doom-tomorrow-night t)
  ;; (load-theme 'doom-vibrant t)
  )

(use-package airline-themes
  :straight t
  :config
  ;; (load-theme 'airline-badwolf t)
  ;; (load-theme 'airline-base16-gui-dark t)
  ;; (load-theme 'airline-base16-gui-light t)
  ;; (load-theme 'airline-base16-shell-dark t)
  ;; (load-theme 'airline-behelit t)
  ;; (load-theme 'airline-cool t)
  ;; (load-theme 'airline-dark t)
  ;; (load-theme 'airline-distinguished t)
  ;; (load-theme 'airline-doom-molokai t)
  ;; (load-theme 'airline-doom-one t)
  ;; (load-theme 'airline-durant t)
  ;; (load-theme 'airline-hybridline t)
  ;; (load-theme 'airline-kalisi t)
  ;; (load-theme 'airline-kolor t)
  ;; (load-theme 'airline-light t)
  ;; (load-theme 'airline-luno t)
  ;; (load-theme 'airline-molokai t)
  ;; (load-theme 'airline-murmur t)
  ;; (load-theme 'airline-papercolor t)
  ;; (load-theme 'airline-powerlineish t)
  ;; (load-theme 'airline-raven t)
  ;; (load-theme 'airline-serene t)
  ;; (load-theme 'airline-simple t)
  ;; (load-theme 'airline-sol t)
  ;; (load-theme 'airline-solarized-alternate-gui t)
  ;; (load-theme 'airline-solarized-gui t)
  ;; (load-theme 'airline-ubaryd t)
  ;; (load-theme 'airline-understated t)
  ;; (load-theme 'airline-wombat t)
  )

(provide 'init-look-and-feel)
;;; init-look-and-feel.el ends here
