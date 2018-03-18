;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Configure package.el
(setq package-enable-at-startup nil)
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Configure load path
(defconst my/lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path my/lisp-dir)
(let ((default-directory my/lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Required to enable some use-package functionality
(use-package delight :ensure t :demand t)
(use-package bind-key :ensure t :demand t)

;; Essentials

;; Configure packages

(require 'init-look-and-feel)
(require 'init-paredit)

(use-package projectile
  :ensure t
  :bind (("C-c p f" . projectile-find-file)))

(use-package init-global-functions
  :bind
  (("s-\\" . my/delete-other-window)
   ("<f7>" . (lambda () (interactive) (find-file user-init-file)))))

;; (use-package yasnippet
;;   :ensure t
;;   :defer 10
;;   :delight yas-minor-mode
;;   :init (yas-global-mode))

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


(provide 'init)
;;; init.el ends here
