;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst emacs-start-time (current-time))

;; Found this in John Wiegley's dot-emacs; it reduces startup time by
;; ~.5 seconds. Looks like it is preventing garbage collection during
;; init and explicitly running it once afterward and restoring
;; settings to reasonable defaults.

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Disable "For information about GNU Emacs and the GNU system, type
;; C-h C-a." message on startup
(setq inhibit-startup-echo-area-message "jed")

;; Configure package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (package-initialize)

;; Don't load stale byte-compiled files
(setq load-prefer-newer t)

;; Configure load path
(defconst my/lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path my/lisp-dir)
(let ((default-directory my/lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Keep emacs Custom-settings in separate file and load it on init
(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(defun my/package-installed-p (pkg)
  "Check if package is installed. In emacs26 package-installed-p
seems to require package-initialize iff the package is *not*
installed. This prevents calling package-initialized if all
packages are already installed which improves startup time."
  (condition-case nil
      (package-installed-p pkg)
    (error
     (package-initialize)
     (package-installed-p pkg))))

(when (not (my/package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Used to remove modes from minibar

(use-package delight
  :ensure t
  :demand t
  :init
  (use-package undo-tree :ensure t :delight)
  (use-package face-remap :delight (text-scale-mode)))

(use-package bind-key :ensure t :demand t)

;; Configure packages

(use-package dash
  :ensure t)

;; Autocompletion

(use-package company
  :ensure t
  :defer 10
  :delight
  :commands (company-mode global-company-mode)
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)

              ;; I'm slowing learning vim bindings
              ;; ("h" . company-select-previous)
              ;; ("j" . company-select-next)
              ;; ("k" . company-select-previous)
              ;; ("l" . company-select-next)
              )
  :init
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-downcase nil)
  (global-company-mode 1)
  ;; (add-hook 'emacs-lisp-mode-hook (lambda () (company-mode 1)))
  )

(require 'init-org)
(require 'init-shell)
(require 'init-clojure)
(require 'init-javascript)
(require 'init-evil)

(use-package linum
  :init
  (global-linum-mode 1))

(use-package smartparens
  :ensure t
  :demand t
  :config
  (use-package smartparens-config)
  (smartparens-global-strict-mode 1)
  :bind (("C-S-<left>" . sp-forward-barf-sexp)
         ("C-S-<right>" . sp-forward-slurp-sexp)))

(use-package align
  :config
  ;; Always use tabs and don't add unneeded space
  (defadvice align-regexp (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil))
      ad-do-it)))

;; Fix whitespace on save and always use spaces
(use-package files
  :init
  (setq mode-require-final-newline t)
  (setq-default indent-tabs-mode nil)
  :config
  (add-hook 'before-save-hook
            (lambda ()
              (if (not indent-tabs-mode)
                  (untabify (point-min) (point-max)))
              (delete-trailing-whitespace))))

(use-package haskell-mode
  :ensure t
  :init
  (setq haskell-process-args-stack-ghci
        '("--ghci-options=-ferror-spans -fshow-loaded-modules" "--no-build" "--no-load"))
  :bind (:map haskell-mode-map
         ("C-`" . haskell-interactive-bring)
         ("C-c C-l" . haskell-process-load-file)
         )
  :config
  (use-package hindent
    :ensure t
    :config
    (add-hook 'haskell-mode-hook #'hindent-hook)))

(use-package groovy-mode
  :defer 30
  :ensure t
  :config
  (setq groovy-indent-offset 4))

(use-package yaml-mode
  :defer 30
  :ensure t)

(use-package web-mode
  :defer 30
  :ensure t
  :init
  (setq web-mode-markup-indent-offset 2))

(use-package edit-indirect :ensure t)
(use-package markdown-mode :ensure t)

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package projectile
  :ensure t
  :defer 30
  :init
  (use-package counsel-projectile
    :ensure t
    :bind (("C-c p f" . counsel-projectile-find-file))
    :config
    (evil-leader/set-key
      "p f" 'counsel-projectile-find-file)))

(use-package init-global-functions
  :commands (my/byte-compile-init-dir
             my/set-frame-size-and-position-to-something-reasonable)
  :bind
  (("s-k" . kill-current-buffer)
   ("s-\\" . my/delete-other-window)
   ("<f7>" . (lambda () (interactive) (find-file user-init-file)))
   ("C-c k" . my/shell-clear)
   ("C-c s s" . my/start-shell)
   ("s-/" . my/comment-or-uncomment-region-or-line)
   ("C-]" . my/just-one-space-in-region)
   ("C-c l d" . my/lein-to-cli-dep)
   ("s-r" . cider-save-and-refresh)))

;; Narrowing completion

(use-package ivy
  :ensure t
  :defer 30
  :delight
  :commands (ivy-mode)
  :bind (("C-s" . swiper)
         ("C-x C-f" . counsel-find-file))
  :config
  (ivy-mode 1))

;; Emacs init.el profiling
(use-package esup
  :ensure t
  :defer 30
  :commands (esup))

;; (use-package yaml-mode
;;   :ensure t)

(use-package yasnippet
  :ensure t
  :defer 10
  :delight yas-minor-mode
  :init (yas-global-mode))

(use-package magit
  :ensure t
  :defer 5
  :bind (("C-c g" . magit-status)
         ("C-c m c" . magit-clone)
         ("C-c m s" . magit-status))
  :config

  (evil-leader/set-key
    "g" 'magit-status)
  ;; Close splits when opening magit
  (use-package fullframe
    :ensure t
    :config
    (fullframe magit-status magit-mode-quit-window))

  ;; Don't show recent commits unless there are local commits not
  ;; pushed to a configured remote
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)
  )

(require 'init-look-and-feel)

;;; Finalization

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

;; (provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
