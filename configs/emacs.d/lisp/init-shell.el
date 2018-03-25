;;; init-shell.el --- Shell configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(provide 'init-shell)
;;; init-shell.el ends here

