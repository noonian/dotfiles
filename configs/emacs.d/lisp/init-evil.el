;;; init-evil.el --- Evil config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :ensure t
  :commands (evil-mode evil-define-key)
  :config
  (evil-mode 1)

  (use-package evil-leader
    :ensure t))

(provide 'init-evil)
;;; init-evil.el ends here
