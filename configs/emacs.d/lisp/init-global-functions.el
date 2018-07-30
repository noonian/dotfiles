;;; init-global-functions.el --- Globally available functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'dash)

(defun my/rename-string-in-buffer (s replacement)
  (interactive)
  (save-excursion
    (point-min)
    (while (re-search-forward s nil t)
      (replace-match replacement))))

;; Doesn't work!
(defun my/recursively-rename-string-in-files (dir regex s replacement)
  "Recursively rename occurances of S in DIR with
REPLACEMENT. DIR may also be a file."
  (interactive)
  (let ((files (directory-files-recursively dir regex)))
    (save-excursion
      (dolist (file files)
        (find-file file)
        (my/rename-string-in-buffer s replacement)))))

(defun my/delete-other-window ()
  "Delete the OTHER window..."
  (interactive)
  (other-window 1)
  (delete-window))

(defun my/byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun my/shell-clear ()
  "Clear shell output buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (comint-send-input)))

(defun directory-name-base (dirpath)
  (file-name-nondirectory (directory-file-name dirpath)))

(defun my/buffer-open? (name)
  (let ((result nil))
    (dolist (buf (buffer-list))
      (when (string= name (buffer-name buf))
        (setq result buf)))
    result))

(defun my/next-shell-name (&optional suffix)
  "Return a string named after the current buffer. If the default
  name already exists increment it by one and return that."
  (format "*shell*<%s%s>"
          (directory-name-base default-directory)
          (if suffix suffix "")))

(defun my/start-shell (&optional create-new?)
  "Start a shell named after the current buffer."
  (interactive "P")
  (shell (my/next-shell-name))
  (let ((n "")
        (name (my/next-shell-name)))
    (while (and create-new? (my/buffer-open? name))
      (setq n (1+ (if (stringp n) 1 n)))
      (setq name (my/next-shell-name n)))
    (shell name)))

(defun my/font-installed? (font-name)
  (find-font (font-spec :name font-name)))

(defun my/init-font-stack (fonts)
  "Set frame font to first installed font in FONTS."
  (-when-let (font (car fonts))
    (if (my/font-installed? font)
        (set-frame-font font nil nil)
      (my/init-font-stack (cdr fonts)))))

(defconst my/large-frame-width 1000) ;pixels
(defconst my/large-frame-height 400)
(defconst my/bad-math-offset 24
  "For some
    reason (- (display-pixel-width) (my/large-frame-width)) is
    slightly larger than the offset from the left side of the
    screen needs to be.")
(defconst my/doc-size-offset 36)

(defun my/single-display-workarea ()
  "Return the width of one display even in the presence of
    multiple monitors."
  (let* ((monitors (display-monitor-attributes-list))
         (monitor (car monitors)))
    (assoc 'workarea monitor)))

(defun my/workarea-width (workarea)
  (nth 3 workarea))

(defun my/workarea-height (workarea)
  (nth 4 workarea))

(defun my/set-frame-size-and-position-to-something-reasonable ()
  "Set frame size and position.

  Attempt to place frame on the right side of the desktop and
  give a largish width if the desktop has a high enough
  resolution."
  (interactive)
  (if window-system
      (let* ((workarea (my/single-display-workarea))
             (workarea-width (my/workarea-width workarea))
             (workarea-height (my/workarea-height workarea))
             (frame-height (- workarea-height my/doc-size-offset))
             (left-offset (- workarea-width
                             my/large-frame-width
                             my/bad-math-offset)))
        (set-frame-width (selected-frame) my/large-frame-width nil t)
        (set-frame-height (selected-frame) frame-height nil t)
        ;; Position frame on the right side of the screen for optimal
        ;; web development workflow.
        (set-frame-position nil left-offset 0))))

(defun my/comment-or-uncomment-region-or-line ()
  "Comment or uncomment the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; http://stackoverflow.com/questions/8674912/how-to-collapse-whitespaces-in-a-region
(defun my/just-one-space-in-region (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

;; https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(defun my/pop-to-org-agenda (split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  (org-agenda-list)
  (when (not split)
    (delete-other-windows)))

(defun my/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun my/lein-to-cli-dep ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (indent-for-tab-command)
    (forward-char)
    (sp-forward-barf-sexp)
    (sp-forward-barf-sexp)
    (sp-backward-delete-char)
    (sp-forward-sexp)
    (insert " {:mvn/version}")
    (backward-char)
    (sp-forward-slurp-sexp)))

(defun my/haskell-eval-last ()
  (interactive)

  (let ((buf (buffer-name)))
    (switch-to-buffer-other-window "*hask*")
    (haskell-interactive-mode-history-previous 1)
    (haskell-interactive-mode-return)
    (switch-to-buffer-other-window buf))
  )

(defun cider-save-and-refresh ()
  (interactive)
  (save-buffer)
  (call-interactively 'cider-refresh))

(provide 'init-global-functions)
;;; init-global-functions.el ends here
