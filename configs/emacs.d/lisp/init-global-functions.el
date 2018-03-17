;;; init-global-functions.el --- Globally available functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/delete-other-window ()
  "Delete the OTHER window..."
  (interactive)
  (other-window 1)
  (delete-window))

(provide 'init-global-functions)
;;; init-global-functions.el ends here
