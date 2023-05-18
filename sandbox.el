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

(auto-dim-other-buffers-mode -1)

(map! :map vertico-map
  "C-h" #'++backward-delete-word)

(setq lsp-headerline-arrow "")
(defvar ++killed-file-buffers nil)

(defun ++track-killed-file-buffer ()
  ;; (message "Tracking kiled file buffer %s" buffer-file-name)
  (condition-case ex
    (when buffer-file-name
      (add-to-list #'++killed-file-buffers buffer-file-name))
    ('error (message (format "Failed to track killed file buffer %s because %s" buffer-file-name ex)))))

(defun ++reopen-killed-file-buffer ()
  (interactive)
  (condition-case ex
    (when-let ((file-to-reopen (pop ++killed-file-buffers)))
      ;; (message "Reopening killed buffer %s" file-to-reopen)
      (find-file file-to-reopen))
    ('error (message (format "Failed to reopen file buffer %s because %s" buffer-file-name ex)))))

(advice-add #'kill-this-buffer :before #'++track-killed-file-buffer)
(map! :map doom-leader-map
  "b o" #'++reopen-killed-file-buffer)

(defun pin-buffer ()
  "Pin buffer to current window.
Primarily useful for embark + dired since I want to keep the dired
window open while opening the files in it."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "pinned buffer" "un-pinned buffer")))
