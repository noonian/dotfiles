;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Configure straight

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Don't load stale byte-compiled files
(setq load-prefer-newer t)

;; Configure load path
(defconst my/lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path my/lisp-dir)
(let ((default-directory my/lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Install use-package

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

;; Used to remove modes from minibar

(use-package delight
  :straight t
  :demand t
  :init
  (eval-after-load "undo-tree-mode" '(delight 'undo-tree))
  (use-package face-remap :delight (text-scale-mode)))

(use-package bind-key :straight t :demand t)

;; Configure packages

;; Autocompletion

(use-package company
  :straight t
  :delight
  :commands (company-mode)
  :init
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (add-hook 'emacs-lisp-mode-hook (lambda () (company-mode 1))))

(require 'init-look-and-feel)
(require 'init-paredit)

(use-package groovy-mode
  :straight t
  :init
  (setq groovy-indent-offset 2))

(use-package projectile
  :straight t
  :init
  (use-package counsel-projectile
    :straight t
    :bind (("C-c p f" . counsel-projectile-find-file))))

(use-package init-global-functions
  :commands (my/byte-compile-init-dir)
  :bind
  (("s-\\" . my/delete-other-window)
   ("<f7>" . (lambda () (interactive) (find-file user-init-file)))
   ("C-c k" . eshell/clear)
   ("C-c s s" . my/start-shell)
   ("s-/" . my/comment-or-uncomment-region-or-line)))

;; Narrowing completion

(use-package ivy
  :straight t
  :delight
  :commands (ivy-mode)
  :bind (("C-s" . swiper)
	 ("C-x C-f" . counsel-find-file))
  :config
  (ivy-mode 1))

;; Emacs init.el profiling
(use-package esup
  :straight t
  :commands (esup))

(use-package yasnippet
  :straight t
  :defer 10
  :delight yas-minor-mode
  :init (yas-global-mode))

(use-package magit
  :straight t
  :defer 5
  :bind (("C-c g" . magit-status))
  :config
  ;; Don't show recent commits unless there are local commits not
  ;; pushed to a configured remote
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)
  )

(use-package evil
  :straight t
  :config
  (use-package evil-surround
    :straight t
    :config
    ;; (global-evil-surround-mode -1)
    )
  (progn
    (evil-mode -1))
  )

(use-package clojure-mode
  :straight t)

(provide 'init)

;;; init.el ends here
