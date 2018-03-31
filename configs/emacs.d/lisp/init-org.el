;;; init-org.el --- org-mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Hope and pray that this brings in org 9
;; org-version
(use-package org
  :ensure t
  :defer t
  :init
  (define-obsolete-function-alias 'org-define-error 'define-error)

  (setq org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil)))

  :config
  (use-package org-bullets :ensure t)
  (use-package ox-reveal :ensure t)
  (use-package ox-gfm :ensure t)

  (add-hook 'org-mode-hook
            (lambda () (org-bullets-mode 1)))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '(

                                 (shell . t)
                                 (js . t)

                                 ))

  )

(provide 'init-org)
;;; init-org.el ends here
