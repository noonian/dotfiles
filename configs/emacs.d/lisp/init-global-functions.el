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

  (defun eshell/clear ()
    "Clear eshell output buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun directory-name-base (dirpath)
    (file-name-nondirectory (directory-file-name dirpath)))

  (defun my/start-shell ()
    "Start a shell named after the current buffer."
    (interactive)
    (shell (format "*shell*<%s>" (directory-name-base default-directory))))

(provide 'init-global-functions)
;;; init-global-functions.el ends here
