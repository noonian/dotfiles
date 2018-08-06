;;; init-evil.el --- Evil config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :ensure t
  :defer 30
  :init
  (setq evil-start-state 'emacs)
  :config

  (use-package evil-collection
    :ensure t
    :config
    (evil-collection-init))

  (use-package evil-magit
    :ensure t
    )

  (use-package evil-commentary
    :ensure t
    :bind (:map evil-normal-state-map
                ("gc" . evil-commentary)))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode 1)
    (evil-leader/set-leader ",")
    (define-key evil-normal-state-map (kbd "\\") 'evil-repeat-find-char-reverse)
    )

  (use-package evil-cleverparens
    :ensure t
    :commands (evil-cleverparens-mode)
    :config
    (dolist (hook '(clojure-mode-hook clojurescript-mode-hook))
      (add-hook hook #'evil-cleverparens-mode))
    )

  (use-package evil-lisp-state
    :ensure t
    :config
    (evil-lisp-state-leader ", l"))


  ;; (use-package evil-surround
  ;;   :ensure t
  ;;   :config
  ;;   (global-evil-surround-mode 1)
  ;;   )

  )

(provide 'init-evil)
;;; init-evil.el ends here
