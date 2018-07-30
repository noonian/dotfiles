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
         ("C-c t a" . my/pop-to-org-agenda)
         :map org-mode-map
         ("DEL" . sp-backward-delete-char)
         ("SPC" . self-insert-command))
  :init
  (define-obsolete-function-alias 'org-define-error 'define-error)

  (setq org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)")))

  (setq org-agenda-files '("~/git/projects/life/"
                           "~/git/projects/projects.org"
                           "~/.emacs.d/notes.org"))

  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-span (quote day))))
            ;; Show all todos that have not been scheduled or have a
            ;; deadline because they will show for the scheduled
            ;; day. Also omit urgent todos as they will show up in the
            ;; first section.
            (alltodo ""
                     ((org-agenda-skip-function
                       '(or (my/org-skip-subtree-if-priority ?A)
                            (org-agenda-skip-if nil '(scheduled deadline)))))))
           ((org-agenda-compact-blocks t)))

          ("p" "Project list"
           ((tags "project" ()))
           ((org-agenda-compact-blocks t)
            (org-tags-match-list-sublevels 'indented))))
        )

  :config
  (use-package init-global-functions :commands (my/org-skip-subtree-if-priority))
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
    )
  (use-package ob-async :ensure t)

  (add-hook 'org-mode-hook
            (lambda () (org-bullets-mode 1)))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '(

                                 (shell . t)
                                 (js . t)

                                 ))

  ;; Tramp config

  ;; (setq tramp-remote-path
  ;;       (append tramp-remote-path
  ;;               '("~/.guix-profile/bin"
  ;;                 "~/.guix-profile/sbin"
  ;;                 "/run/current-system/profile/bin"
  ;;                 "/run/current-system/profile/sbin")))

  (defconst my/tramp-guix-ssh-method
    '("ssh"
      (tramp-remote-shell "/gnu/store/ars9lm9jk9hgdifg0gqvf1jrvz5mdg1j-bash-4.4.12/bin/bash")
      (tramp-login-program "ssh")
      (tramp-encoding-shell "/bin/bash")
      ;; (tramp-decoding-shell "/gnu/store/ars9lm9jk9hgdifg0gqvf1jrvz5mdg1j-bash-4.4.12/bin/bash")
      (tramp-login-args
       (("-l" "%u")
        ("-p" "%p")
        ("%c")
        ("-e" "none")
        ("%h")))
      (tramp-async-args
       (("-q")))
      (tramp-remote-shell-login
       ("-l"))
      (tramp-remote-shell-args
       ("-c"))))

  (defconst my/tramp-guix-scp-method
    '("scp"
      (tramp-login-program "ssh")
      (tramp-login-args
       (("-l" "%u")
        ("-p" "%p")
        ("%c")
        ("-e" "none")
        ("%h")))
      (tramp-async-args
       (("-q")))
      (tramp-remote-shell "/gnu/store/ars9lm9jk9hgdifg0gqvf1jrvz5mdg1j-bash-4.4.12/bin/bash")
      (tramp-remote-shell-login
       ("-l"))
      (tramp-remote-shell-args
       ("-c"))
      (tramp-copy-program "scp")
      (tramp-copy-args
       (("-P" "%p")
        ("-p" "%k")
        ("-q")
        ("-r")
        ("%c")))
      (tramp-copy-keep-date t)
      (tramp-copy-recursive t)))

  (defun my/tramp-guix-setup ()
    (interactive)
    (add-to-list 'tramp-methods my/tramp-guix-ssh-method)
    (add-to-list 'tramp-methods my/tramp-guix-scp-method))

  )

(provide 'init-org)
;;; init-org.el ends here
