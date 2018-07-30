;;; init-evil.el --- Evil config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :ensure t
  :defer 30
  :init
  (setq evil-start-state 'emacs)
  :config

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode 1))

  (use-package evil-lisp-state
    :ensure t
    :config
    (evil-lisp-state-leader ", l")
    )

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1)
    )

  )

(provide 'init-evil)
;;; init-evil.el ends here
