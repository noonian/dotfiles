;;; init-clojure.el --- Clojure config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package clojure-mode
  :ensure t
  :defer 30
  :config
  (add-hook 'cljure-mode-hook #'smartparens-strict-mode)
  (add-hook 'cljurescript-mode-hook #'smartparens-strict-mode)

  (use-package cider
    :ensure t
    :diminish t
    :commands cider-mode
    :config
    (add-hook 'cider-repl-mode-hook
              (lambda ()
                (smartparens-strict-mode 1)
                (company-mode 1)))
    :bind (("C-c M-j" . cider-jack-in)
           ("C-c M-c" . cider-connect)
           ("C-c v" . cider-eval-buffer)
           ("s-t" . cider-test-run-ns-tests)))

  (use-package init-clojure-indentations
    :config
    (dolist (item my/clojure-indentations)
      (put-clojure-indent (car item) (cdr item))))

)

(provide 'init-clojure)
;;; init-clojure.el ends here
