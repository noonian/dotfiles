;;; init-org.el --- org-mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Hope and pray that this brings in org 9
;; org-version
(use-package org
  :ensure t
  :defer 30
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c t a" . my/pop-to-org-agenda))
  :init
  (define-obsolete-function-alias 'org-define-error 'define-error)

  (setq org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i!)" "|" "DONE(d!)" "CANCELED(c@)")))

  (setq org-agenda-files '("~/git/projects/life/"))
  :config
  (use-package org-bullets :ensure t)
  (use-package ox-reveal :ensure t)
  (use-package ox-gfm :ensure t)
  (use-package org-capture
    :init
    (setq org-default-notes-file "~/.emacs.d/notes.org")
    (setq org-capture-templates
          '(("t"
             "Todo list item"
             entry
             (file+headline org-default-notes-file "Tasks")
             "* TODO %?\n  %i\n Context: [[%l][%f]]")))

    (setq org-agenda-custom-commands
          '(("c" "Simple agenda view"
             ((tags "PRIORITY=\"A\""
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "High-priority unfinished tasks:")))
              (agenda "" ((org-agenda-span (quote day))))
              (alltodo ""
                       ((org-agenda-skip-function
                         '(or (my/org-skip-subtree-if-priority ?A)
                              (org-agenda-skip-if nil '(scheduled deadline)))))))
             ((org-agenda-compact-blocks t)))))
    )



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
