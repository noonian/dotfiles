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

;; Configure straight

;; (let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
;;       (bootstrap-version 3))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

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

;; Install use-package

;; (straight-use-package 'use-package)

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
  :delight
  :commands (company-mode)
  :init
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (add-hook 'emacs-lisp-mode-hook (lambda () (company-mode 1))))

(require 'init-paredit)
(require 'init-org)
(require 'init-shell)

(use-package align
  :config
  ;; Always use tabs and don't add unneeded space
  (defadvice align-regexp (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil))
      ad-do-it)))

;; Fix whitespace on save
(use-package files
  :init (setq mode-require-final-newline t)
  :config
  (add-hook 'before-save-hook
            (lambda ()
              (if (not indent-tabs-mode)
                  (untabify (point-min) (point-max)))
              (delete-trailing-whitespace))))

;; (use-package groovy-mode
;;   :ensure t
;;   :init
;;   (setq groovy-indent-offset 2))

;; (use-package haskell-mode
;;   :ensure t)

(use-package projectile
  :ensure t
  :init
  (use-package counsel-projectile
    :ensure t
    :bind (("C-c p f" . counsel-projectile-find-file))))

(use-package init-global-functions
  :commands (my/byte-compile-init-dir)
  :bind
  (("s-\\" . my/delete-other-window)
   ("<f7>" . (lambda () (interactive) (find-file user-init-file)))
   ("C-c k" . eshell/clear)
   ("C-c s s" . my/start-shell)
   ("s-/" . my/comment-or-uncomment-region-or-line)
   ("C-]" . my/just-one-space-in-region)))

;; Narrowing completion

(use-package ivy
  :ensure t
  :delight
  :commands (ivy-mode)
  :bind (("C-s" . swiper)
	 ("C-x C-f" . counsel-find-file))
  :config
  (ivy-mode 1))

;; Emacs init.el profiling
(use-package esup
  :ensure t
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
  :ensure t
  :config
  (use-package evil-surround
    :ensure t
    :config
    ;; (global-evil-surround-mode -1)
    )
  (progn
    (evil-mode -1))
  )

(use-package clojure-mode
  :ensure t)

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
