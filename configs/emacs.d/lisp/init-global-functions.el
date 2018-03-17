;;; init-global-functions.el --- Globally available functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/delete-other-window ()
  "Delete the OTHER window..."
  (interactive)
  (other-window 1)
  (delete-window))

(defun my/byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(provide 'init-global-functions)
;;; init-global-functions.el ends here
