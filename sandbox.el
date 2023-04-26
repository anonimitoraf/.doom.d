(message "Loading sandbox")

(setq
  clojure-toplevel-inside-comment-form t
  cider-show-error-buffer nil
  cider-auto-jump-to-error nil
  cider-inspector-max-coll-size 32)

(defvar ++ov-cider-inspector-keywords)
(defvar ++ov-cider-inspector-method-names)
(defvar ++ov-cider-inspector-method-param-lists)
(defun ++ov-cider-inspector ()
  (ov-clear)
  (let ((case-fold-search nil))
    (setq ++ov-cider-inspector-keywords (ov-regexp "public\\|private\\|protected\\|final\\|native\\|static")
     ++ov-cider-inspector-method-names (ov-regexp "\\([a-zA-Z0-9.$]\\)+(")
      ++ov-cider-inspector-method-param-lists (ov-regexp "(\\([][<>a-zA-Z0-9.$,]*\\))?")))
  (ov-set ++ov-cider-inspector-keywords 'face `(:foreground ,(doom-color 'magenta)))
  (ov-set ++ov-cider-inspector-method-names 'face `(:foreground ,(doom-color 'red)))
  (ov-set ++ov-cider-inspector-method-param-lists 'face `(:foreground "white")))
(defun ++ov-cider-inspector-with-delay ()
  (run-at-time 0 nil #'++ov-cider-inspector))
(add-hook 'cider-inspector-mode-hook #'++ov-cider-inspector-with-delay)

(map! :map doom-leader-map
  "g t" #'git-timemachine
  "g T" #'git-timemachine-switch-branch)

(defun ++open-or-close-scratch-buffer ()
  (interactive)
  (if-let ((window (get-buffer-window "*doom:scratch*")))
    (quit-window nil window)
    (doom/open-scratch-buffer)))
(map! :map doom-leader-map
  "p x" #'++open-or-close-scratch-buffer)

(defun ++get-dir-name (path)
  (file-name-nondirectory (directory-file-name (file-name-directory path))))

(defun ++open-or-close-project-scratch-buffer ()
  (interactive)
  (if-let ((window (get-buffer-window (format "*doom:scratch (%s)*" (++get-dir-name (projectile-project-root))))))
    (quit-window nil window)
    (doom/open-project-scratch-buffer)))
(map! :map doom-leader-map
  "x" #'++open-or-close-project-scratch-buffer)

(map! :map org-mode-map
  "C-k" #'+org/insert-item-above
  "C-j" #'+org/insert-item-below)
