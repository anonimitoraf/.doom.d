;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 'dash)
(require 'cl-lib)

(setq user-full-name "Rafael Nicdao"
      user-mail-address "nicdaoraf@gmail.com")

(defun ++kb (bytes) (* bytes 1024))
(defun ++mb (bytes) (* (++kb bytes) 1024))

(defun ++with-face (str &rest face-plist)
  (propertize str 'face face-plist))

(defun ++async-shell-command (command &optional callback)
  "Execute shell COMMAND asynchronously in the background.

Return the temporary output buffer which command is writing to
during execution.

When the command is finished, call CALLBACK with the resulting
output as a string."
  (let* ((output-buffer (generate-new-buffer " *++async-shell-command*"))
         (callback-fn (or callback (lambda (_)))))
    (set-process-sentinel
     (start-process "++async-shell-command" output-buffer shell-file-name shell-command-switch command)
     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback-fn output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defmacro ++comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defmacro ++spy (form)
  `(message (concat (prin1-to-string ',form) " => %s") ,form))

(++comment
 (++spy (+ 1 1)) ;; "(+ 1 1) => 2"
 (++spy IS-LINUX) ;; "IS-LINUX => t"
 )

(defmacro ++advice-lambda (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))

(defun ++suppress-messages (old-fun &rest args)
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
         (apply old-fun args)
      (advice-remove 'message #'silence))))

(defun ++silence-messages (orig-fun &rest args)
  "Advice function that silences all messages in ORIG-FUN."
  (let ((inhibit-message t)      ;Don't show the messages in Echo area
        (message-log-max nil))   ;Don't show the messages in the *Messages* buffer
    (apply orig-fun args)))

(defvar ++window-id (shell-command-to-string "xdotool getwindowfocus getactivewindow | tr -d '\n'"))

(defun ++js-root-dir ()
  (locate-dominating-file (or buffer-file-name default-directory) "package.json"))

(defun ++js-prettier-path ()
  (let ((local (expand-file-name (concat (++js-root-dir) "node_modules/.bin/prettier")))
         (global (executable-find "prettier")))
    (if (file-exists-p local) local global)))

(defvar ++sync-folder-path "~/Dropbox/emacs")

(map! :map global-map
  :nvi "C-." #'repeat)

(setq savehist-file (concat ++sync-folder-path "/savehist"))

(advice-add #'kill-current-buffer :override #'kill-this-buffer)

(map! :map doom-leader-map
      "f g" #'find-file-other-window)

(map! :map doom-leader-map "w SPC" #'ace-select-window)

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red")))

(require 'alert)
(setq alert-default-style (if IS-MAC
                            'growl
                            'dunst+i3)
      alert-fade-time 120)

(use-package! all-the-icons
  :config (setq all-the-icons-scale-factor 0.90))

(use-package apheleia
  :config
  (apheleia-global-mode t))

(use-package! auto-dim-other-buffers
  :init
  (defun ++adob-never-dim (buffer)
    ;; For some reason, some vertico bufferes still get dimmed
    ;; so we manually exclude them
    (-some (lambda (buffer-name-pattern)
             (string-match buffer-name-pattern (buffer-name buffer)))
           '("\\*Minibuf-[0-9]+\\*")))
  (setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil
        auto-dim-other-buffers-never-dim-buffer-functions '(++adob-never-dim))
  :config
  (auto-dim-other-buffers-mode +1)
  (custom-set-faces!
    '(auto-dim-other-buffers-face :background "grey5")
    '(auto-dim-other-buffers-hide-face :background "grey5")))

(setq avy-timeout-seconds 0.1)

(setq avy-keys (number-sequence ?a ?z))

(setq bookmark-default-file (concat ++sync-folder-path "/bookmarks"))

(setq bookmark-save-flag 1)

(map! :leader
      :desc "Find-replace" "r" #'anzu-query-replace)
(map! :leader
      :desc "Find-replace (regexp)" "R" #'anzu-query-replace-regexp)

(use-package! chatgpt
  :defer t
  :config
  (unless (boundp 'python-interpreter)
    (defvaralias 'python-interpreter 'python-shell-interpreter))
  (setq chatgpt-repo-path (expand-file-name "straight/repos/ChatGPT.el/" doom-local-dir))
  (set-popup-rules!
    '(("*ChatGPT*"
       :quit 'current
       :side right
       :size 0.4
       :select nil)))
  (defun ++chatgpt-restart ()
    (interactive)
    (chatgpt-stop)
    (shell-command-to-string "ps aux | grep playwright | awk '$0=$2' | xargs kill -9")
    (chatgpt-init))
  (map! :map doom-leader-map
        "?" #'chatgpt-query
        "!" #'++chatgpt-restart))

(use-package! cider
  :config
  (defun ++cider-pprint-eval-last-sexp-to-repl ()
    (interactive)
    (cider-pprint-eval-last-sexp-to-repl t))
  (setq cider-repl-pop-to-buffer-on-connect nil
        cider-dynamic-indentation nil
        cider-font-lock-dynamically nil
        cider-font-lock-reader-conditionals nil
        nrepl-force-ssh-for-remote-hosts t)
  (map! :map clojure-mode-map
        :nv "SPC m p p" #'cider-pprint-eval-last-sexp-to-comment
        :nv "SPC m p P" #'++cider-pprint-eval-last-sexp-to-repl)
  (map! :map cider-repl-mode-map
        :nvi "C-k" #'cider-repl-previous-input
        :nvi "C-j" #'cider-repl-next-input)
  (map! :map cider-inspector-mode-map
        :nv "C-k" #'cider-inspector-previous-inspectable-object
        :nv "C-j" #'cider-inspector-next-inspectable-object
        :nv "C-h" #'cider-inspector-pop
        :nv [mouse-3] #'cider-inspector-pop
        :nv "C-l" #'cider-inspector-operate-on-point)
  (add-hook 'cider-mode-hook (lambda () (setq-local completion-styles '(cider))))
  (add-hook 'cider-repl-mode-hook (lambda () (symex-mode +1))))

(advice-add 'cider-eldoc :around #'ignore)

;; (defun nrepl--ssh-tunnel-connect (host port)
;;   "Connect to a remote machine identified by HOST and PORT through SSH tunnel."
;;   (message "[nREPL] Establishing SSH tunneled connection to %s:%s ..." host port)
;;   (let* ((remote-dir (if host (format "/ssh:%s:" host) default-directory))
;;          (local-port (nrepl--random-free-local-port))
;;          (ssh (or (executable-find "ssh")
;;                   (error "[nREPL] Cannot locate 'ssh' executable")))
;;          (cmd (nrepl--ssh-tunnel-command ssh remote-dir port local-port))
;;          (tunnel-buf (nrepl-tunnel-buffer-name
;;                       `((:host ,host) (:port ,port))))
;;          (tunnel (start-process-shell-command "nrepl-tunnel" tunnel-buf cmd)))
;;     (process-put tunnel :waiting-for-port t)
;;     (set-process-filter tunnel (nrepl--ssh-tunnel-filter local-port))
;;     (while (and (process-live-p tunnel)
;;                 (process-get tunnel :waiting-for-port))
;;       (accept-process-output nil 0.005))
;;     (if (not (process-live-p tunnel))
;;         (error "[nREPL] SSH port forwarding failed.  Check the '%s' buffer" tunnel-buf)
;;       (message "[nREPL] SSH port forwarding established to localhost:%s" local-port)
;;       (let ((endpoint (nrepl--direct-connect "localhost" local-port)))
;;         (thread-first endpoint
;;           (plist-put :tunnel tunnel)
;;           (plist-put :remote-host host))))))

;; (defun nrepl--random-free-local-port ()
;;   (let* ((random-free-local-port-cmd (concat "comm -23 "
;;                                              "<(seq 1024 65535 | sort) "
;;                                              "<(ss -Htan | awk '{print $4}' | cut -d':' -f2 | sort -u) | "
;;                                              "shuf | head -n 1")))
;;     (with-temp-buffer
;;            (insert (string-trim-right (shell-command-to-string random-free-local-port-cmd)))
;;            (buffer-string))))

;; (defun nrepl--ssh-tunnel-command (ssh dir remote-port local-port)
;;   "Command string to open SSH tunnel to the host associated with DIR's PORT."
;;   (with-parsed-tramp-file-name dir v
;;      ;; this abuses the -v option for ssh to get output when the port
;;     ;; forwarding is set up, which is used to synchronise on, so that
;;     ;; the port forwarding is up when we try to connect.
;;     (format-spec
;;      "%s -v -N -L %l:localhost:%p %u'%h'"
;;      `((?s . ,ssh)
;;        (?l . ,local-port)
;;        (?p . ,remote-port)
;;        (?h . ,v-host)
;;        (?u . ,(if v-user (format "-l '%s' " v-user) ""))))))

(add-to-list 'auto-mode-alist '("\\*cider-error\\*" . cider-stacktrace-mode))

(defun cider--client-tramp-filename (name &optional buffer)
  "Return the tramp filename for path NAME relative to BUFFER.
If BUFFER has a tramp prefix, it will be added as a prefix to NAME.
If the resulting path is an existing tramp file, it returns the path,
otherwise, nil."
  (let* ((buffer (or buffer (current-buffer)))
         (name (replace-regexp-in-string "^file:" "" name))
         (name (concat (cider-tramp-prefix buffer) name)))
    (if (and (tramp-tramp-file-p name)
             (tramp-handle-file-exists-p name))
        name)))

(defun ++kill-disconnected-cider-buffer (process _message)
  (when-let* ((client-buffer (process-buffer process)))
    (kill-buffer client-buffer)))

(advice-add #'nrepl-client-sentinel :after #'++kill-disconnected-cider-buffer)

(use-package! clipetty
  :config
  (unless (display-graphic-p)
    (global-clipetty-mode +1)))

(use-package! clippo)

(use-package! consult
  :config
  (consult-customize
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-xref
    consult--source-bookmark
    +default/search-project
    +default/search-cwd
    +default/search-other-cwd
    :preview-key '(:debounce 0.2 any))
  (map! :map doom-leader-map
        "y" #'consult-yank-from-kill-ring))

(map! :map global-map
  "C-S-k" #'drag-stuff-up
  "C-S-j" #'drag-stuff-down)

(use-package! dotenv-mode
  :config (add-to-list 'auto-mode-alist '("\\.env\\.?" . dotenv-mode)))

(use-package! dwim-shell-command
  :config
  (require 'dwim-shell-commands))

(with-eval-after-load 'org
  (require 'edraw-org)
  (edraw-org-setup-default))

(defun ++edraw-new ()
  (interactive)
  (let ((filepath (concat ++sync-folder-path "/edraw/" (file-name-nondirectory buffer-file-name) "_" (org-id-uuid) ".edraw.svg"))
        (link-desc (read-string "Link description: " )))
    (insert (format "[[edraw:file=%s][%s]]" filepath link-desc))))

(autoload 'edraw-mode "edraw-mode")
(add-to-list 'auto-mode-alist '("\\.edraw\\.svg$" . edraw-mode))

(define-key evil-insert-state-map (kbd "C-j") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-motion-state-map (kbd "<tab>") nil)

(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)

(evil-add-command-properties #'projectile-find-file :jump t)
(evil-add-command-properties #'find-file :jump t)
(evil-add-command-properties #'consult-recent-file :jump t)
(evil-add-command-properties #'doom/find-file-in-private-config :jump t)
(evil-add-command-properties #'+default/search-buffer :jump t)
(evil-add-command-properties #'+default/search-project :jump t)

(setq +evil-want-o/O-to-continue-comments nil)

(unbind-key "K" evil-normal-state-map)
(unbind-key "K" evil-visual-state-map)
(unbind-key "K" evil-motion-state-map)

(setq evil-want-fine-undo t)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(use-package! evil-collection
  :config
  (setq evil-collection-setup-minibuffer t)
  (advice-add 'cider-pprint-eval-last-sexp-to-comment
    :around 'evil-collection-cider-last-sexp)
  (advice-add 'cider-pprint-eval-last-sexp-to-repl
    :around 'evil-collection-cider-last-sexp))

(setq evil-kill-on-visual-paste nil)

(use-package! evil-easymotion
  :config
  (unbind-key "s" evil-normal-state-map)
  (evilem-default-keybindings "s")
  (map! :map evil-normal-state-map
    "s l" #'evilem-motion-forward-word-begin
    "s h" #'evilem-motion-backward-word-begin)
  (custom-set-faces!
    '(avy-lead-face :foreground "red" :background nil :weight bold)
    `(avy-lead-face-0 :foreground ,(doom-color 'yellow) :background nil)))

(use-package! evil-matchit
  :config
  (global-evil-matchit-mode +1))

(use-package! elfeed
  :config
  (setq rmh-elfeed-org-files (list (concat doom-private-dir "elfeed.org"))
        elfeed-db-directory "~/Dropbox/emacs/elfeed")
  (add-hook 'elfeed-search-mode-hook (lambda ()
                                       (elfeed-update)
                                       (setq-local browse-url-browser-function 'eww-browse-url))))

(after! elfeed
  (setq elfeed-search-filter "@5-year-ago +unread"))

(map! :map global-map
      "C-'" #'embark-act)

(setq embark-quit-after-action nil)

(use-package! exercism
  :config
  (map! :map global-map :nv "SPC o e" #'exercism))

(use-package! exec-path-from-shell
  :config
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(define-fringe-bitmap 'flycheck-fringe-bitmap-beam
  (vector #b11000000
          #b11000000
          #b11000000
          #b11000000
          #b11000000
          #b11000000
          #b11000000
          #b11000000
          #b11000000
          #b11000000
          #b11000000
          #b11000000
          #b11000000
          #b11000000
          #b11000000))

(flycheck-define-error-level 'error
  :severity 30
  :compilation-level 2
  :overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'flycheck-fringe-bitmap-beam
  :fringe-face 'flycheck-fringe-error
  :error-list-face 'flycheck-error-list-error)

(flycheck-define-error-level 'warning
  :severity 20
  :compilation-level 2
  :overlay-category 'flycheck-warning-overlay
  :fringe-bitmap 'flycheck-fringe-bitmap-beam
  :fringe-face 'flycheck-fringe-warning
  :error-list-face 'flycheck-error-list-warning)

(flycheck-define-error-level 'info
  :severity 10
  :compilation-level 2
  :overlay-category 'flycheck-info-overlay
  :fringe-bitmap 'flycheck-fringe-bitmap-beam
  :fringe-face 'flycheck-fringe-info
  :error-list-face 'flycheck-error-list-warning)

(setq flycheck-display-errors-delay 0.01)

(use-package! flycheck-popup-tip
  :config
  (setq flycheck-popup-tip-error-prefix " "))

(use-package! flycheck
  :config
  (setq flycheck-error-list-format
        `[("File" 32)
          ("Line" 8 flycheck-error-list-entry-<)
          ("Col" 8 nil)
          ("Level" 32 flycheck-error-list-entry-level-<)
          ("ID" 32 t)
          (#("Message (Checker)" 0 7
             (face flycheck-error-list-error-message)
             9 16
             (face flycheck-error-list-checker-name))
           0 t)])
  (add-hook 'flycheck-error-list-mode-hook
            (lambda () (tabulated-list-sort 3)))
  (set-popup-rules!
    '(("*Flycheck errors*"
       :quit nil
       :side bottom
       :size 10
       :select nil))))

(use-package! flycheck-posframe
  :config
  (setq flycheck-posframe-position 'window-bottom-left-corner))

(setq flycheck-posframe-info-prefix " "
      flycheck-posframe-warning-prefix " "
      flycheck-posframe-error-prefix " "
      flycheck-posframe-prefix " ")

(use-package! google-translate
  :config
  (map! :leader :desc "Google translate" "s a" #'google-translate-smooth-translate)
  (setq google-translate-translation-directions-alist
        '(("en" . "ja") ("ja" . "en")))
  ;; Workaround: see https://github.com/atykhonov/google-translate/issues/137
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130)))
(use-package! google-translate-smooth-ui)

(use-package! hackernews)

(require 'keychain-environment)
(keychain-refresh-environment)

(require 'i3wm-config-mode)

(setq ielm-noisy nil
      ielm-prompt "λ> ")

(require 'itail)

(setq ispell-dictionary "en"
      ispell-personal-dictionary (concat ++sync-folder-path "/spell/personal-dictionary.pws"))

(use-package! lsp-mode
  :config
  (setq lsp-completion-enable t
        lsp-idle-delay 0.1)
  (add-hook! '(typescript-tsx-mode-hook
               typescript-mode-hook
               web-mode-hook
               js-mode-hook
               js2-mode-hook)
             ;; Use `tide' for completions and formatting instead since LSP is too laggy
             (setq-local lsp-completion-enable t
                         lsp-completion-show-detail nil
                         lsp-typescript-format-enable nil)
             ;; (when (-contains? '(typescript-tsx-mode
             ;;                     typescript-mode
             ;;                     web-mode
             ;;                     js-mode
             ;;                     js2-mode)
             ;;                   major-mode)
             ;;   (setq-local completion-at-point-functions (mapcar #'cape-company-to-capf
             ;;                                                     (list #'company-tide))))
             )
  (set-popup-rules!
    '(("*lsp-help*"
       :quit t
       :side right
       :size 0.4
       :select t
       :modeline t))))

(after! lsp-mode
  (setq lsp-lens-enable nil
        lsp-log-io nil
        lsp-use-plists t
        lsp-completion-no-cache nil
        lsp-completion-use-last-result nil
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-eldoc-enable-hover nil
        lsp-lens-place-position 'end-of-line
        lsp-enable-indentation t
        lsp-signature-auto-activate t
        lsp-signature-function 'lsp-signature-posframe
        lsp-signature-posframe-params '(:poshandler posframe-poshandler-point-bottom-left-corner-upward
                                        :height 10
                                        :width 120
                                        :border-width 1
                                        :min-width 120)
        lsp-auto-execute-action nil
        lsp-enable-snippet nil
        lsp-auto-touch-files nil
        xref-prompt-for-identifier '(not xref-find-references
                                        xref-find-definitions
                                        xref-find-definitions-other-window
                                        xref-find-definitions-other-frame))
  (map! :map evil-normal-state-map
        "g t" #'lsp-find-type-definition
        "g D" #'lsp-find-implementation
        "g f" #'xref-find-references)

  (map! :map lsp-signature-mode-map
        "C-j" #'lsp-signature-next
        "C-k" #'lsp-signature-previous))

(after! lsp-mode
  ;; Clojure(Script)
  (dolist (to-ignore '("[/\\\\]\\.clj-kondo$"
                       "[/\\\\]\\.shadow-cljs$"
                       "[/\\\\]resources$"))
    (add-to-list 'lsp-file-watch-ignored to-ignore)))

(use-package! lsp-mode
  :config
  (setq lsp-clients-typescript-server-args '("--stdio")
        lsp-javascript-preferences-rename-shorthand-properties nil
        lsp-typescript-preferences-rename-shorthand-properties nil))

(advice-add 'lsp-deferred :override #'lsp)

(after! lsp-ui
  (define-key lsp-ui-peek-mode-map (kbd "j") 'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "k") 'lsp-ui-peek--select-prev)
  (define-key lsp-ui-peek-mode-map (kbd "C-k") 'lsp-ui-peek--select-prev-file)
  (define-key lsp-ui-peek-mode-map (kbd "C-j") 'lsp-ui-peek--select-next-file)
  (map! :map lsp-mode-map
        :nv "SPC c m" #'lsp-ui-imenu
        :nv "SPC d" #'lsp-ui-doc-glance)
  (setq lsp-ui-peek-fontify 'always
        lsp-ui-peek-list-width 100
        lsp-ui-peek-peek-height 40
        lsp-ui-peek-always-show nil

        ;; I prefer xref now
        lsp-ui-peek-enable nil
        ;; These can be brought up on-demand with SPC d
        lsp-ui-doc-enable nil
        ;; Prevents LSP peek to disappear when mouse touches it
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 0
        lsp-ui-doc-position (if (display-graphic-p) 'at-point 'top)
        lsp-ui-doc-max-width 120
        lsp-ui-doc-max-height 120
        lsp-ui-doc-header nil


        lsp-ui-imenu-enable t

        ;; This is just annoying, really
        lsp-ui-sideline-enable nil))

(use-package! magit-filenotify
  :config
  (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'auto
        doom-modeline-height 0
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-modal-icon nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-enable-word-count nil
        doom-modeline-lsp nil))
(setq org-clock-mode-line-total 'current)
(setq display-time-default-load-average nil
      display-time-24hr-format t)

(display-time-mode +1)

(use-package! doom-modeline
  :config
  (doom-modeline-def-segment matches
    (let ((meta (concat (doom-modeline--macro-recording)
                        (doom-modeline--anzu))))
      (or meta "")))
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info repl lsp checker)
    '(buffer-position selection-info))
  (doom-modeline-def-modeline 'minimal
    '(bar matches buffer-info-simple)
    '(major-mode))
  (doom-modeline-def-modeline 'special
    '(bar matches buffer-info)
    '(window-number buffer-position selection-info))
  (doom-modeline-def-modeline 'project
    '(bar matches buffer-default-directory)
    '(window-number buffer-position selection-info))
  (doom-modeline-def-modeline 'dashboard
    '(bar matches buffer-default-directory-simple)
    '(window-number buffer-position selection-info))
  (doom-modeline-def-modeline 'vcs
    '(bar matches buffer-info-simple)
    '(window-number buffer-position selection-info))
  (doom-modeline-def-modeline 'info
    '(bar matches buffer-info)
    '(window-number info-nodes buffer-position selection-info))
  (doom-modeline-def-modeline 'media
    '(bar matches buffer-info)
    '(window-number media-info process))
  (doom-modeline-def-modeline 'message
    '(bar matches buffer-info-simple)
    '(window-number buffer-position selection-info))
  (doom-modeline-def-modeline 'pdf
    '(bar matches buffer-info)
    '(window-number pdf-pages process))
  (doom-modeline-def-modeline 'org-src
    '(bar matches buffer-info-simple lsp checker)
    '(buffer-position selection-info))
  (doom-modeline-def-modeline 'timemachine
    '(bar matches git-timemachine)
    '(buffer-position selection-info)))

(use-package! olivetti
  :init
  (setq olivetti-body-width 0.5
        olivetti-minimum-body-width 120
        olivetti-style t)
  (add-hook #'org-mode-hook #'olivetti-mode)
  (add-hook #'org-mode-hook (lambda () (vi-tilde-fringe-mode -1))))

(after! org
  (setq org-directory (concat ++sync-folder-path "/org")
        org-default-notes-file (concat org-directory "/notes/default.org")
        org-agenda-files (cl-map 'list (lambda (f) (concat org-directory "/" f))
                                 '("life"
                                   "work"
                                   "captures"
                                   "notes")))
  (setq org-agenda-span 14
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d"
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-window-setup 'other-window
        org-ellipsis " ▾"
        org-export-with-section-numbers nil
        org-hide-emphasis-markers t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-todo-keywords '((sequence "TODO(t)" "ONGOING(o)" "ON HOLD(h)" "|" "DONE(d)" "CANCELLED(c)")
                            (sequence "[ ](T)" "[-](O)" "[?](H)" "|" "[X](D)"))
        org-log-done 'time
        org-hide-leading-stars t
        org-superstar-headline-bullets-list '("•")
        org-superstar-cycle-headline-bullets 1
        org-superstar-special-todo-items 'hide
        org-superstar-item-bullet-alist '("-")
        org-tags-column -120
        org-image-actual-width nil
        ;; Don't log the time a task was rescheduled or redeadlined.
        org-log-redeadline nil
        org-log-reschedule nil
        ;; Prefer rescheduling to future dates and times
        org-read-date-prefer-future 'time))

(defun org-agenda-refresh ()
  "Refresh all `org-agenda' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-maybe-redo)))))

(defadvice org-schedule (after refresh-agenda activate)
  "Refresh org-agenda."
  (org-agenda-refresh))

(advice-add 'org-deadline       :after (++advice-lambda #'org-save-all-org-buffers))
(advice-add 'org-schedule       :after (++advice-lambda #'org-save-all-org-buffers))
(advice-add 'org-store-log-note :after (++advice-lambda #'org-save-all-org-buffers))
(advice-add 'org-todo           :after (++advice-lambda #'org-save-all-org-buffers))

(use-package! org-download
  :config (setq org-download-method 'attach))

(add-hook 'dired-mode-hook 'org-download-enable)

(add-hook 'org-mode-hook (lambda () (visual-line-mode -1)))

(defun ++org-babel-interpret-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))
(add-hook 'org-babel-after-execute-hook #'++org-babel-interpret-ansi)

(defun ++org-collapse-all-except-current ()
  (interactive)
  "Collapse all nodes except current"
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (recenter-top-bottom)
    (show-children)
    (recenter-top-bottom)))
(map! :map org-mode-map
      :nv "SPC m z" #'++org-collapse-all-except-current)

(add-hook 'org-mode-hook (lambda () (corfu-mode -1)))

(after! org
  (setq org-capture-templates
    '(("t" "" entry (file "~/Dropbox/org/captures/tasks.org")
        "* TODO %?\n%U"
        :kill-buffer t)
       ("t" "Task" entry (file "~/Dropbox/org/captures/tasks.org")
         "* TODO %?\n%U"
         :kill-buffer t)
       ("e" "From emacs" entry (file "~/Dropbox/org/captures/from-emacs.org")
         "* %i\n%?"
         :empty-lines 1
         :kill-buffer t)
       ("c" "From clipboard" entry (file "~/Dropbox/org/captures/from-clipboard.org")
         "* %x\n%?"
         :empty-lines 1
         :kill-buffer t)
       ("s" "Shopping list" entry (file "~/Dropbox/org/captures/shopping-list.org")
         "* [ ] %?"
         :jump-to-captured t
         :empty-lines 1
         :kill-buffer t)
       ;; ("L" "Org protocol link")
       ;; ("Lh" "Link (headless)" entry (file "~/Dropbox/emacs/web-bookmarks.org")
       ;;    "* %:annotation\n  %U\n\n  %i"
       ;;    :prepend t
       ;;    :immediate-finish t
       ;;    :kill-buffer t)
       ("Li" "Link (interactive)" entry (file "~/Dropbox/emacs/web-bookmarks.org")
          "* %:annotation %U\n  %?"
          :prepend t))))

(setenv "NODE_PATH"
        (concat
         (getenv "HOME") "/org/node_modules"  ":"
         (getenv "NODE_PATH")))

(use-package! ob-clojure
  :init (require 'cider)
  :config (setq org-babel-clojure-backend 'cider))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (Clojure . t)
   (Javascript . t)))

(after! org
  (setq org-babel-results-keyword "results")
  (custom-set-faces!
    `(org-level-1 :foreground ,(doom-color 'yellow))
    `(org-meta-line :foreground ,(doom-color 'grey))
    `(org-table :foreground ,(doom-color 'orange))
    `(org-block :background ,(doom-color 'black))
    `(org-block-begin-line :foreground ,(doom-color 'grey) :overline t)
    `(org-block-end-line :foreground ,(doom-color 'grey) :overline nil :underline t)
    `(org-code :foreground ,(doom-color 'teal))))

(use-package! org-sticky-header
  :config
  (org-sticky-header-mode +1))

(use-package! org-alert
  :config
  (setq org-alert-interval 300)
  (org-alert-enable))

(defun ++dunst+i3-notify (info)
  (async-start
    `(lambda ()
       ,(async-inject-variables "alert-default-icon")
       (shell-command-to-string (concat (executable-find "dunstify")
                                  (format " --action=\"forwardAction,Forward\" --appname=Emacs --icon=%s \"%s\" \"%s\" "
                                    alert-default-icon
                                    ,(plist-get info :buffer-name)
                                    ,(plist-get info :message))
                                  " | tr -d '\n'")))
    (lambda (dunstify-result)
      (when (equal dunstify-result "forwardAction")
        (async-start
          `(lambda ()
             ,(async-inject-variables "++window-id")
             (shell-command-to-string ,(format "i3-msg --socket %s [id=%s] focus"
                                         ;; See https://www.reddit.com/r/i3wm/comments/glhgo4/comment/fvntamj/?utm_source=share&utm_medium=web2x&context=3
                                         "\"/run/user/1000/i3/$(ls -t /run/user/1000/i3/ | awk '{print $1}' | grep ipc | head -n 1)\""
                                         ++window-id)))
          (lambda (i3-focus-result)
            (message "FOCUS RESULT FROM i3: %s !!!" i3-focus-result)
            (org-agenda-list)))))))

(require 'async)
(alert-define-style 'dunst+i3 :title "dunst + i3"
  :notifier
  (lambda (info)
    ;; buffer prop isn't serializable
    (plist-put info :buffer-name (buffer-name (plist-get info :buffer)))
    (plist-delete! info :buffer)
    (++dunst+i3-notify info)))

(use-package! org-excalidraw
  :config
  (setq org-excalidraw-directory (concat ++sync-folder-path "/excalidraw")))
(after! org (org-excalidraw-initialize))

(use-package! org-habit
  :config
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-habits-only-for-today nil
        org-habit-show-all-today nil
        org-habit-preceding-days 14
        org-habit-following-days 7))

(use-package! org-roam
  :config
  (defvar ++org-roam-dir "~/Dropbox/emacs/org-roam")
  (make-directory ++org-roam-dir 'parents)
  (setq org-roam-directory ++org-roam-dir)
  (org-roam-db-autosync-mode))

(use-package! org-sticky-header
  :config
  (add-hook 'org-mode-hook (lambda () (org-sticky-header-mode +1))))

(defun ++org-remark-notes-file-name ()
  (concat ++sync-folder-path "/org-remark/" (projectile-project-name) "/org-remark.org"))

(use-package! org-remark
  :init
  (setq org-remark-notes-file-name #'++org-remark-notes-file-name)
  :config
  (org-remark-mode +1))

(use-package! org-ros)

(setq persp-save-dir (concat ++sync-folder-path "/sessions/"))

(use-package! prescient
  :init
  (setq prescient-save-file (concat ++sync-folder-path "/prescient-save.el")
        prescient-sort-full-matches-first t
        prescient-sort-length-enable nil)
  :config
  (prescient-persist-mode +1))
(use-package! corfu-prescient
  :init
  (setq corfu-prescient-override-sorting t
        corfu-prescient-enable-filtering nil)
  :config
  (corfu-prescient-mode +1))

(defun ++set-projectile-cache-duration ()
  (setq projectile-files-cache-expire
        (if (and buffer-file-name
            (file-remote-p (file-truename buffer-file-name)))
       (* 10 60) ; Long-ish projectile cache for remote files
     10)))

(use-package! projectile
  :config
  (add-hook 'find-file-hook #'++set-projectile-cache-duration))

(use-package! pulsar
  :config
  (setq pulsar-pulse-functions
        '(recenter-top-bottom
          move-to-window-line-top-bottom
          reposition-window
          forward-page
          backward-page
          scroll-up-command
          scroll-down-command
          org-next-visible-heading
          org-previous-visible-heading
          org-forward-heading-same-level
          org-backward-heading-same-level
          outline-backward-same-level
          outline-forward-same-level
          outline-next-visible-heading
          outline-previous-visible-heading
          outline-up-heading
          evil-window-vsplit
          evil-window-split
          evil-window-left
          evil-window-right
          evil-window-up
          evil-window-down
          +workspace/close-window-or-workspace
          +shell/toggle
          better-jumper-jump-backward
          better-jumper-jump-forward))
  (setq pulsar-pulse-on-window-change t)
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.05)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-generic)
  (setq pulsar-highlight-face 'pulsar-generic)
  (pulsar-global-mode +1)
  ;; For some reason, some commands don't work despite being in pulsar-pulse-functions
  (setq ++pulsar-pulse-line-cmds
        '(evil-scroll-up
          evil-scroll-down
          evil-goto-line
          evil-goto-last-line
          evilem-motion-previous-line
          evilem-motion-next-line))
  (defun ++pulsar-pulse-line (func)
    (advice-add func :after (lambda (_f &rest _args) (pulsar-pulse-line))))
  (mapc #'++pulsar-pulse-line ++pulsar-pulse-line-cmds)
  ;; integration with the `consult' package:
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
  ;; integration with the built-in `imenu':
  (add-hook 'imenu-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry))

(use-package! shell
  :init
  (setq comint-buffer-maximum-size 8192
        comint-input-ring-size 1024
        comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
  :config
  (defun ++shell-setup ()
    (setq-local corfu-auto nil
                line-spacing nil)
    ;; Disable font-locking in this buffer to improve performance
    (font-lock-mode -1)
    ;; Prevent font-locking from being re-enabled in this buffer
    (make-local-variable 'font-lock-function)
    (setq font-lock-function (lambda (_) nil))
    (require 'xterm-color)
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)
    ;; TODO This doesn't work. How do I enable modeline?
    (doom-modeline-mode 1))
  (add-hook 'shell-mode-hook #'++shell-setup)
  (remove-hook 'shell-mode-hook #'hide-mode-line-mode)
  ;; Keybinds
  (map! :map shell-mode-map
        :nvi "C-r" #'comint-history-isearch-backward
        :nvi "C-k" #'comint-previous-input
        :nvi "C-j" #'comint-next-input
        :nvi "C-l" #'comint-clear-buffer))

(use-package! sidecar-locals
  :init
  (setq sidecar-locals-dir-name ".emacs"
        ;; Add to this as necessary
        sidecar-locals-paths-allow (-map (lambda (dir) (expand-file-name dir))
                                         '("~/personal/lc/"
                                           "~/personal/supa-sales/")))
  :config
  (sidecar-locals-mode))

(use-package! symex
  :config
  (add-hook! '(clojure-mode-hook
               clojurescript-mode-hook
               clojurec-mode-hook
               emacs-lisp-mode-hook
               inferior-emacs-lisp-mode-hook
               org-mode-hook)
    (symex-mode +1)
    (symex-initialize)
    (map! :map doom-leader-map "k" (cmd! (when symex-mode (symex-mode-interface))))
    (setq symex-modal-backend 'hydra)))

(defhydra+ hydra-symex (:columns 5
                        :post (progn
                                ;; TODO Avoid duplication by storing this beforehand
                                (set-face-attribute 'mode-line nil :background "#23102C")
                                (symex-exit-mode)))
  "Symex mode"
  ("C-j" symex-emit-backward "emit backward")
  ("C-h" symex-capture-backward "capture backward")
  ("C-l" symex-capture-forward "capture forward")
  ("C-k" symex-emit-forward "emit forward"))

(advice-add 'symex-mode-interface :after (lambda (&rest args)
                                           (symex-hide-menu)
                                           (set-face-attribute 'mode-line nil :background "#5a1111")))

(use-package! speed-dial
  :config
  (speed-dial-mode +1)
  (speed-dial-apply '(("C-c 1" . "~/Dropbox/work/audience-republic/misc.el")
                      ("C-c 2" . "~/Dropbox/life/todos.org")
                      ("C-c 3" . "~/Dropbox/blog/content-org")
                      ("C-c 4" . "~/Dropbox/work/audience-republic/contracting.org")
                      ("C-c 5" . "~/work/misc.org")
                      ("C-c 6" . "~/work/misc-2.org"))))

(use-package! speed-type
  :config
  (setq speed-type-default-lang 'English))

(use-package! spookfox
  :init
  (setq spookfox-enabled-apps '(spookfox-org-tabs
                                spookfox-tabs
                                spookfox-js-injection
                                spookfox-jscl))
  :config
  (require 'spookfox-org-tabs)
  (require 'spookfox-tabs)
  (require 'spookfox-js-injection)
  (require 'spookfox-jscl)
  (spookfox-init))

(use-package! sticky-shell
  :config
  (sticky-shell-global-mode t))

(use-package! thread-dump)

;; (defun setup-tide-mode ()
;;   (require 'company)
;;   (tide-setup)
;;   (eldoc-mode -1)
;;   (tide-hl-identifier-mode -1)
;;   (setq tide-completion-detailed nil
;;         tide-completion-ignore-case t
;;         tide-save-buffer-after-code-edit nil)
;;   (setq-local completion-at-point-functions
;;     (mapcar #'cape-company-to-capf
;;       (list #'company-tide)))
;;   (advice-add #'tide-eldoc-function :around #'ignore))

;; (use-package! tide
;;   :config
;;   (advice-remove 'tide-setup 'eldoc-mode)
;;   (add-hook! '(typescript-tsx-mode-hook
;;                typescript-mode-hook
;;                web-mode-hook
;;                js-mode-hook
;;                js2-mode-hook)
;;              #'setup-tide-mode))

(use-package! tree-sitter)
(use-package! tree-sitter-langs)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook (lambda (&rest args) (ignore-errors (tree-sitter-hl-mode +1))))
;; (tree-sitter-require 'tsx)
(add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
(add-to-list 'tree-sitter-major-mode-language-alist '(scss-mode . css))

(use-package! undohist
  :config
  (undohist-initialize)
  ;; Automatically recover undohist
  (defun ++yes (_) t)
  (defun ++yes-recover-undohist (orig-fn &rest args)
    (advice-add #'yes-or-no-p :override #'++yes)
    (ignore-errors (apply orig-fn args))
    (advice-remove #'yes-or-no-p #'++yes))
  (advice-add #'undohist-recover-1 :around #'++yes-recover-undohist))

(use-package! vertico
  :config
  (map! :map vertico-map
        "C-l" #'vertico-exit
        "C-;" #'vertico-insert)
  (when (display-graphic-p) ; Yabai on Mac sometimes hides posframes
    (require 'vertico-posframe)
    (vertico-multiform-mode)
    ;; Configure the display per command.
    ;; Use a buffer with indices for imenu
    ;; and a flat (Ido-like) menu for M-x.
    (setq vertico-multiform-commands
          '((execute-extended-command posframe)
            (helpful-callable posframe)
            (helpful-variable posframe)
            (find-file posframe)
            (find-file-other-window posframe)
            (projectile-find-file posframe)
            (doom/find-file-in-private-config posframe)
            (projectile-switch-project posframe)
            (consult-recent-file posframe)
            (consult-bookmark buffer)
            (consult-imenu buffer)
            (+default/search-buffer buffer)
            (yas-insert-snippet posframe)
            (lsp-execute-code-action posframe)
            (vertico-repeat-select posframe)
            (cider-connect-clj posframe)
            (cider-connect-cljs posframe)
            (org-roam-node-find posframe)
            (++open-ipad-notes posframe)
            (+lookup/references buffer)
            (++lookup/google posframe)))
    ;; Configure the display per completion category.
    ;; Use the grid display for files and a buffer
    ;; for the consult-grep commands.
    (setq vertico-multiform-categories
          '((consult-grep buffer)))))

(use-package! vertico-posframe
  :config
  ;; Top center with a bit of space at the top to align with header-line
  (defun ++posframe-poshandler-top-center-with-padding (info)
    (cons
      (/ (- (plist-get info :parent-frame-width)
           (plist-get info :posframe-width))
        2)
      2))
  (setq vertico-posframe-border-width 1
        vertico-posframe-parameters '((left-fringe . 10)
                                      (right-fringe . 10))
        vertico-posframe-poshandler #'++posframe-poshandler-top-center-with-padding))

(setq vertico-buffer-display-action '(display-buffer-in-side-window
                                       (side . right)
                                       (window-width . 0.4)))

(map! :map doom-leader-map
      "\"" #'vertico-repeat-select)

(setq vi-tilde-fringe-bitmap-array [#b00000000
                                    #b00000000
                                    #b00000000
                                    #b11111111
                                    #b11111111
                                    #b00000000
                                    #b00000000
                                    #b00000000])

(use-package! vundo
  :config
  (setq undohist-ignored-files '(".git/COMMIT_EDITMSG"))
  (map! :map global-map
        :nv "U" #'vundo))

(which-key-mode +1)

(use-package! whitespace
  :config
  (global-whitespace-mode)
  (setq whitespace-style '(face tabs tab-mark trailing)
        whitespace-display-mappings '((tab-mark 9 [124 9] [92 9])))
  (custom-set-faces
   '(whitespace-tab ((t (:foreground "#636363"))))))

(map! :map doom-leader-map "z" #'+zen/toggle-fullscreen)

(use-package! yasnippet
  :config
  (setq yas-snippet-dirs
    '("~/.doom.d/snippets"))
  (yas-global-mode +1))

(advice-add 'yas-insert-snippet :after (lambda (&rest _)
                                         (evil-insert-state)))

(map! :leader :desc "Lookup doc" :n "e" #'+lookup/documentation)

(use-package clojure-mode
  :config
  (setq cljr-add-ns-to-blank-clj-files nil))

(use-package apheleia
  :config
  (setf (alist-get 'zprint apheleia-formatters) `("zprint"))
  (add-to-list 'apheleia-mode-alist '(clojure-mode . zprint))
  (add-to-list 'apheleia-mode-alist '(clojurescript-mode . zprint))
  (add-to-list 'apheleia-mode-alist '(clojurec-mode . zprint)))

(put-clojure-indent 'defjob :defn)
(put 'defjob 'clojure-doc-string-elt 2)
(font-lock-add-keywords 'clojure-mode
                        `((,(concat "(\\(?:" clojure--sym-regexp "/\\)?"
                                    "\\(defjob\\)\\>")
                           1 font-lock-keyword-face)))

(font-lock-add-keywords 'clojure-mode
                        `((,(concat "(\\(?:" clojure--sym-regexp "/\\)?"
                                    "\\(comment\\)")
                           1 font-lock-comment-face)))

(set-popup-rules!
  '(("^\\*cider-repl"
     :quit nil
     :side right
     :size 0.33
     :select t
     :modeline t)
    ("^\\*\\(?:cider-doc\\)"
     :side bottom
     :size 0.2)))

(defun ++clojure-pretty-format ()
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "jet --pretty --edn-reader-opts '{:default tagged-literal}'"
   (current-buffer)
   t
   "*jet error buffer*"
   t))

;; TODO Instead of replacing buffer contents, copy to clipboard
(defun ++clojure-json->edn ()
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   (concat "jet"
           " --pretty"
           " --edn-reader-opts '{:default tagged-literal}'"
           " --from json"
           " --to edn"
           " --keywordize '#(-> % csk/->kebab-case keyword)'")
   (current-buffer)
   t
   "*jet error buffer*"
   t))

(defun ++erlang-compile ()
  (interactive)
  (erlang-compile)
  (set-buffer "*erlang*")
  (let ((root (projectile-project-root)))
    (++async-shell-command
     (format (concat
              "find %s -type f -name \"*.[he]rl\" | etags.emacs"
              " -o " (concat root "TAGS")
              " -")
             root)
     (lambda (_) (progn (message (concat "Generated tags for project " root))
                        (visit-tags-table root))))))

(add-hook 'erlang-shell-mode-hook (lambda () (company-mode -1)))

(map! :map erlang-shell-mode-map
      "C-SPC" #'erlang-complete-tag
      "C-l" 'comint-clear-buffer)

(map! :map erlang-mode-map
      "C-c C-k" #'++erlang-compile)

(use-package! gherkin-mode
  :config (add-to-list 'auto-mode-alist '("\\.feature\\'" . gherkin-mode)))

(define-derived-mode typescript-tsx-mode web-mode "TypeScript/TSX")

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

(use-package! lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration
    '(typescript-tsx-mode . "css-modules"))
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "cssmodules-language-server")
                     :priority -1
                     :add-on? t
                     :activation-fn (lsp-activate-on "css-modules")
                     :server-id 'css-modules)))

(add-hook 'scss-mode-hook (lambda ()
                            (setq-local comment-start "/* "
                                        comment-end " */")))

(use-package! lsp-mode
    :hook (groovy-mode . lsp-deferred)
    :commands (lsp lsp-deferred)
    :config (setq lsp-groovy-classpath
              ["/usr/local/opt/groovy/libexec/lib"
                "~/.gradle/caches/modules-2/files-2.1"]))

(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))

(use-package! lsp-mode
  :hook (prolog-mode . lsp)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection (list "swipl"
                                "-g" "use_module(library(lsp_server))."
                                "-g" "lsp_server:main"
                                "-t" "halt"
                                "--" "stdio"))
    :major-modes '(prolog-mode)
    :priority 1
    :multi-root t
    :server-id 'prolog-ls)))

(defvar ++lookup/google-history nil)
(defun ++lookup/google ()
  (interactive)
  (let ((query (consult--read ++lookup/google-history
                 :prompt "Search Google: ")))
    (add-to-list '++lookup/google-history query)
    (+lookup/online query "Google")))

(defun ++org-src-lang= (lang)
  (when (eq major-mode 'org-mode)
    (let ((src-lang (nth 0 (org-babel-get-src-block-info))))
      (equal src-lang lang))))

(defvar ++was-in-src-block? nil)
(defun ++activate-lsp-org ()
  (when (++org-src-lang= "sql")
    (let ((in-src? (and
                     (org-src--on-datum-p (org-element-at-point))
                     (org-in-src-block-p t))))
      (when (and in-src? (not ++was-in-src-block?))
        (lsp-org))
      (setq ++was-in-src-block? in-src?))))
(add-hook 'post-command-hook #'++activate-lsp-org)
(add-hook 'org-mode-hook (lambda () (setq-local lsp-warn-no-matched-clients nil)))

(defvar ++ov-date)
(defvar ++ov-time)
(defun ++highlight-timestamps ()
  (setq ++ov-date (ov-regexp "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T"))
  (setq ++ov-time (ov-regexp "\\([0-9]\\{2\\}:[0-9]\\{2\\}.[0-9]\\{2\\}\\)Z"))
  (ov-set ++ov-date 'face `(:foreground ,(doom-color 'cyan)))
  (ov-set ++ov-time 'face `(:foreground ,(doom-color 'red))))

(use-package! ov
  :config
  (add-hook 'after-save-hook (lambda ()
                               (ov-clear)
                               (++highlight-timestamps))))

(defun ++unpropertize-kill-ring (&rest args)
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(advice-add #'consult-yank-from-kill-ring :before #'++unpropertize-kill-ring)

(defun ++org-src-block-at-point ()
  (interactive)
  (kill-new (plist-get (cadr (org-element-at-point)) :value)))

(defun ++browse-file-remote ()
  (interactive)
  (browse-url
   (let
       ((rev (magit-rev-abbrev "HEAD"))
        (repo (forge-get-repository 'stub))
        (file (magit-file-relative-name buffer-file-name))
        (highlight
         (if
             (use-region-p)
             (let ((l1 (line-number-at-pos (region-beginning)))
                   (l2 (line-number-at-pos (- (region-end) 1))))
               (format "#L%d-L%d" l1 l2))
           ""
           )))
     (forge--format repo "https://%h/%o/%n/blob/%r/%f%L"
                    `((?r . ,rev) (?f . ,file) (?L . ,highlight))))))

(defun ++open-ipad-notes ()
  (interactive)
  (let ((default-directory (concat "~/Dropbox/Apps/GoodNotes 5/files/")))
    (call-interactively #'find-file)))

(map! :map doom-leader-map "o i" #'++open-ipad-notes)

(setq warning-minimum-level :error)

(defun ++shell/toggle
    (&optional command)
  "Toggle a persistent terminal popup window.\n\nIf popup is visible but unselected, selected it.\nIf popup is focused, kill it."
  (interactive)
  (let*
    ((workspace-name (if (and (boundp 'persp-mode) persp-mode)
                     (safe-persp-name (get-current-persp))
                   "main"))
      (buf-name (format "*doom:shell-popup:%s*" (or (projectile-project-name) workspace-name)))
     (buffer (get-buffer-create buf-name))
     (dir default-directory))
    (let*
        ((win (and t (get-buffer-window buffer))))
      (if win
        (let (confirm-kill-processes)
          (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
          (delete-window win))
        (progn
          (save-current-buffer
            (set-buffer buffer)
            (if (not (eq major-mode 'shell-mode))
              (shell buffer)
              (cd dir)
              (run-mode-hooks 'shell-mode-hook)))
          (pop-to-buffer buffer)
          (shell-cd dir)
          (let ((cmd (concat "cd " (shell-quote-argument dir) "\n")))
    (comint-send-string nil cmd)))))
    (+shell--send-input buffer command)))

(map! :map doom-leader-map
  "o t" #'++shell/toggle)

(defun ++close-buffers (filename &optional _trash)
  (-each (buffer-list) (lambda (b)
                         (when (equal (buffer-file-name b) (expand-file-name filename))
                           (kill-buffer b)))))
(advice-add #'delete-file :before #'++close-buffers)

(defun ++on-focus-lost () (save-some-buffers t))
(add-function :after after-focus-change-function #'++on-focus-lost)

(defvar ++consult--search-recent-dir-tracked nil)
(define-advice read-directory-name
  (:around (fn &rest args) ++consult--search-recent-dir-tracked)
  (let ((dir (apply fn args)))
    (add-to-list '++consult--search-recent-dir-tracked dir)
    dir))

(defvar ++consult--search-recent-dir-history nil)
(defun ++consult--search-recent-dir ()
  (interactive)
  (let ((default-directory (consult--read ++consult--search-recent-dir-tracked
                              :prompt "Search recent directory: "
                              :history ++consult--search-recent-dir-history)))
    (+default/search-cwd)))

(map! :map doom-leader-map "s r" #'++consult--search-recent-dir)

(defun ++remove-from-jump-list (file-name)
  (interactive)
  (message "Removing %s from jump-list" file-name)
  (let* ((context (better-jumper--get-current-context))
         (old-struct (better-jumper--get-struct))
         (struct (better-jumper--copy-struct old-struct))
         (jumps (ring-elements (better-jumper--get-struct-jump-list struct)))
         (jumps-filtered (->> jumps
                              (reverse)
                              (-filter (lambda (jump) (and jump (not (equal (car jump) file-name)))))))
         (pad-count (- better-jumper-max-length (length jumps-filtered)))
         (jump-list (ring-convert-sequence-to-ring jumps-filtered)))
    (ring-extend jump-list pad-count)
    (aset struct 1 jump-list)
    (better-jumper--set-struct context struct)
    (better-jumper--get-struct context)))

(defun ++remove-current-buffer-from-jump-list ()
  (condition-case ex
      (and buffer-file-name (++remove-from-jump-list buffer-file-name))
    ('error (message (format "Failed to remove buffer %s from jump list: %s" buffer-file-name ex)))))

(advice-add #'kill-this-buffer :before #'++remove-current-buffer-from-jump-list)
(advice-add #'kill-current-buffer :before #'++remove-current-buffer-from-jump-list)

(setq garbage-collection-messages nil)
(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold #x40000000)

;; When idle for N secs run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda () (k-time (garbage-collect)))))

(setq byte-compile-warnings '(not obsolete))

(add-to-list 'term-file-aliases '("alacritty" . "xterm"))

(global-visual-line-mode t)

(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

(setq ++safe-vars '((+format-on-save-enabled-modes . '())
                    (cider-required-middleware-version . "0.25.5")))
(-each ++safe-vars (lambda (pair)
                     (add-to-list 'safe-local-variable-values pair)))

(setq minibuffer-message-timeout 0.0)

(setq show-help-function nil)

(setq +format-on-save-enabled-modes
      '(emacs-lisp-mode
        erlang-mode))

(setq +ligatures-in-modes '())
(setq +ligatures-extras-in-modes '(org-mode))

(when (not (display-graphic-p))
  (setq debug-on-error nil))

(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode +1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(defun ++backward-delete-word ()
  "Like `backward-kill-word' but doesn't copy the deleted word"
  (interactive)
  (delete-region
    (point)
    (progn (forward-word -1) (point))))

(map! :map global-map "C-h" #'++backward-delete-word)

(setq x-select-enable-clipboard-manager nil)

(setq-default line-spacing 0.25)
(add-hook 'shell-mode-hook (lambda () (setq-local line-spacing nil)))

(defun ++tmux--new-session (session-name)
  (++async-shell-command (concat "alacritty --command"
                                 " \"tmux\""
                                 " \"new\""
                                 " \"-s\""
                                 " \"" session-name "\"")
                         (lambda (_) (message (concat "Created new tmux session: " session-name)))))

(defun ++tmux--new-session-quiet (session-name)
  (++async-shell-command (concat "tmux new -d -s " "\"" session-name "\"")
                         (lambda (_) (message (concat "Created new tmux session (quiet): " session-name)))))

(defun ++tmux--switch-session (session-name)
  (++async-shell-command (concat "alacritty --command"
                                 " \"tmux\""
                                 " \"attach-session\""
                                 " \"-t\""
                                 " \"" session-name "\"")
                         (lambda (_) (message (concat "Selected existing tmux session: " session-name)))))

(defun ++tmux--switch-session-quiet (session-name)
  (++async-shell-command (concat "tmux switch -t " "\"" session-name "\"")
                         (lambda (_) (message (concat "Selected existing tmux session (quiet): " session-name)))))

(defvar ++consult--tmux-history nil)
(defun ++consult-tmux (&optional quiet?)
  (interactive)
  (++async-shell-command "tmux list-sessions | awk '$0=$1' | sed s/://"
    (lambda (sessions-str)
      (let* ((no-sessions (string-match-p "^no server running on.*$" sessions-str))
              (sessions (if no-sessions '() (split-string sessions-str)))
              (selected-session (consult--read sessions
                                  :prompt (concat "Select tmux session " (if quiet? "(quiet) ") ": ")
                                  ;; TODO What should be here?
                                  :history 'consult--tmux-history)))
        (if (not (member selected-session sessions))
          ;; Create a new session
          (progn
            (message (concat "New tmux session: " selected-session))
            (if quiet?
              (++tmux--new-session-quiet selected-session)
              (++tmux--new-session selected-session)))
          ;; Switch to an existing session
          (progn
            (message (concat "Selecting existing session: " selected-session))
            (if quiet?
              (++tmux--switch-session-quiet selected-session)
              (++tmux--switch-session selected-session))))))))


(map! :n "SPC _" (cmd! (++consult-tmux))
      :n "SPC -" (cmd! (++consult-tmux t)))

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

(set-popup-rules!
  '(("^\\*helpful function"
     :quit nil
     :size 30)))

;; Stolen from https://emacs.stackexchange.com/a/19582
(defmacro ++with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))

(defun ++call-logging-hooks (command &optional verbose)
  "Call COMMAND, reporting every hook run in the process.
Interactively, prompt for a command to execute.

Return a list of the hooks run, in the order they were run.
Interactively, or with optional argument VERBOSE, also print a
message listing the hooks."
  (interactive "CCommand to log hooks: \np")
  (let* ((log     nil)
         (logger (lambda (&rest hooks)
                   (setq log (append log hooks nil)))))
    (++with-advice
     ((#'run-hooks :before logger))
     (call-interactively command))
    (when verbose
      (message
       (if log "Hooks run during execution of %s:"
         "No hooks run during execution of %s.")
       command)
      (dolist (hook log)
        (message "> %s" hook)))
    log))

(map! :map evil-normal-state-map
      "g T" #'next-buffer
      "g t" #'previous-buffer)

(map! :map doom-leader-map "l p" #'list-processes)

(defun ++ox-hugo-generate-headers ()
  (interactive)
  (save-excursion
    (insert "#+HUGO_BASE_DIR: ..\n")
    (insert "#+HUGO_SECTION: post\n")
    (insert (concat "#+HUGO_CUSTOM_FRONT_MATTER: :date " (format-time-string "%Y-%m-%d") " :pin false :summary \"TODO\"\n"))
    (insert "#+HUGO_TAGS: \"TODO\"\n")))

(plist-put +popup-defaults :modeline t)
(remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)

(add-hook '+dap-running-session-mode-hook #'doom-modeline-mode)

(remove-hook 'shell-mode-hook #'hide-mode-line-mode)
(add-hook 'shell-mode-hook #'doom-modeline-mode)

(after! lsp-mode
  (advice-remove #'lsp #'+lsp-dont-prompt-to-install-servers-maybe-a))

(if (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode +1)
  (setq scroll-margin 1
        scroll-step 1
        scroll-conservatively 10000
        scroll-preserve-screen-position 1))

(setq kill-ring-max (* 100 1000 ))

(setq recentf-max-menu-items 20
      recentf-max-saved-items 400)

(map! :map doom-leader-map "s x" #'async-shell-command)
(set-popup-rules!
    '(("*Async Shell Command*"
       :quit t
       :side bottom
       :size 10
       :select nil)))

(defun ++copy-dir-path ()
  (interactive)
  (let ((dir-path default-directory))
    (kill-new dir-path)
    (message "Copied dir path: %s into clipboard" dir-path)))

(map! :map doom-leader-map
  "+" #'calc
  "s d" #'+default/search-other-cwd)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(defun ++pacman-pkg-info ()
  (interactive)
  (let* ((completions (->> "pacman -Q"
                           (shell-command-to-string)
                           (s-trim)
                           (s-lines)
                           (--map (car (s-split " " it :no-nulls)))))
         (name (completing-read "Package: " completions)))
    (switch-to-buffer (get-buffer-create "*Package Info*"))
    (erase-buffer)
    (-> (format "pacman -Qi %s" name)
        (shell-command-to-string)
        (s-trim)
        (insert))
    (goto-char 0)
    (conf-mode)))

(add-hook 'savehist-mode-hook (lambda ()
                                (-each '(++consult--search-recent-dir-tracked
                                          ++consult--search-recent-dir-history
                                          ++lookup/google-history)
                                  (lambda (v) (add-to-list 'savehist-additional-variables v)))))

(advice-add #'doom-save-session :around #'++silence-messages)
(run-with-idle-timer 5 t #'doom-save-session)

(defun external-terminal ()
  (interactive "@")
  (setenv "INSIDE_EMACS" nil)
  (shell-command (concat "alacritty"
                         " -qq"
                         " --working-directory " (file-name-directory (or load-file-name buffer-file-name))
                         " & disown") nil nil))

(defvar ++random-melpa-pkg-timer nil)

(defun ++show-random-melpa-pkg ()
  (interactive)
  (package-list-packages-no-fetch)
  (with-current-buffer (get-buffer "*Packages*")
    (let* ((lines-num (count-lines (point-min) (point-max)))
           (line (random (1- lines-num)))
           (content (buffer-substring-no-properties
                     (line-beginning-position line)
                     (line-end-position line))))
      (prog1
          (alert content
                 :title "Random MELPA package trivia"
                 :id 'random-melpa-pkg)
        (kill-buffer)))))

(defun ++random-melpa-pkg-start ()
  (interactive)
  ;; Make sure starting is idempotent
  (unless ++random-melpa-pkg-timer
    (setq ++random-melpa-pkg-timer
          (run-at-time 0 20 #'++show-random-melpa-pkg))))

(defun ++random-melpa-pkg-stop ()
  (interactive)
  (when ++random-melpa-pkg-timer
    (cancel-timer ++random-melpa-pkg-timer)
    (setq ++random-melpa-pkg-timer nil)))

(defun ++open-current-file-in-new-buffer ()
  "Open the file that the current buffer is visiting in a new buffer."
  (interactive)
  (let* ((fn buffer-file-name)
         (buf (create-file-buffer fn)))
    (with-current-buffer buf
      (setq buffer-file-name fn)
      (revert-buffer t t))
    (switch-to-buffer-other-window buf)))

(defun ++cider-popup ()
  (interactive)
  (let* ((all-buffers (mapcar #'buffer-name (buffer-list)))
         (cider-buffers (seq-filter
                         (lambda (buf) (string-match-p (concat
                                                        "\\*"
                                                        "cider-repl "
                                                        ".*"
                                                        (projectile-project-name)
                                                        ":.+" ;; hostname
                                                        ":[0-9]+" ;; port
                                                        ".*"
                                                        "\\*")
                                                       buf))
                         all-buffers)))
    (ivy-read "Pop-up CIDER buffer: " cider-buffers
              :require-match t
              :action (lambda (buf-name)
                        (display-buffer buf-name
                                        '(pop-to-buffer . ((side . left)
                                                           (slot . -1))))))))

(map! :map clojure-mode-map :nv "SPC m r p" #'++cider-popup)
(map! :map clojurescript-mode-map :nv "SPC m r p" #'++cider-popup)
(map! :map clojurec-mode-map :nv "SPC m r p" #'++cider-popup)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'onetwo nil t)
(require 'skerrick nil t)

(defun ++org-table->csv (table-name)
  "Search for table named `TABLE-NAME` and export."
  (interactive "sTable name to export to CSV: ")
  (save-excursion
    (message "Exporting table %s to CSV" table-name)
    (outline-show-all)
    (goto-char (point-min))
    (let ((case-fold-search t))
     (if (search-forward-regexp (concat "#\\+TBLNAME: +" table-name) nil t)
       (progn
         (next-line)
         (org-table-export (format "%s.csv" table-name) "orgtbl-to-csv"))))))

(defun ++webcam-active? ()
  (interactive)
  (let ((v (shell-command-to-string "lsmod | grep uvcvideo | head -c -1 | awk 'NR==1 { printf $3 }'")))
    (message (if (equal v "1") "ACTIVE" "NOT ACTIVE"))))

(defun ++demo-recording ()
  (interactive)
  (map! :map doom-leader-map "m e r" #'skerrick-eval-region)
  (hide-mode-line-mode +1)
  (display-line-numbers-mode -1))

;;;###autoload
(defun lsp-completion-at-point ()
  "Get lsp completions."
  (message "HERE!")
  (when (or (--some (lsp--client-completion-in-comments? (lsp--workspace-client it))
                    (lsp-workspaces))
            (not (nth 4 (syntax-ppss))))
    (let* ((trigger-chars (->> (lsp--server-capabilities)
                               (lsp:server-capabilities-completion-provider?)
                               (lsp:completion-options-trigger-characters?)))
           (bounds-start (or (-some--> (cl-first (bounds-of-thing-at-point 'symbol))
                               (save-excursion
                                 (ignore-errors
                                   (goto-char (+ it 1))
                                   (while (lsp-completion--looking-back-trigger-characterp
                                           trigger-chars)
                                     (cl-incf it)
                                     (forward-char))
                                   it)))
                             (point)))
           result done?
           (candidates
            (lambda ()
              (lsp--catch 'input
                  (let ((lsp--throw-on-input lsp-completion-use-last-result)
                        (same-session? (and lsp-completion--cache
                                            ;; Special case for empty prefix and empty result
                                            (or (cl-second lsp-completion--cache)
                                                (not (string-empty-p
                                                      (plist-get (cddr lsp-completion--cache) :prefix))))
                                            (equal (cl-first lsp-completion--cache) bounds-start)
                                            (s-prefix?
                                             (plist-get (cddr lsp-completion--cache) :prefix)
                                             (buffer-substring-no-properties bounds-start (point))))))
                    (cond
                     ((or done? result) result)
                     ((and (not lsp-completion-no-cache)
                           same-session?
                           (listp (cl-second lsp-completion--cache)))
                      (setf result (apply #'lsp-completion--filter-candidates
                                          (cdr lsp-completion--cache))))
                     (t
                      (-let* ((resp (lsp-request-while-no-input
                                     "textDocument/completion"
                                     (plist-put (lsp--text-document-position-params)
                                                :context (lsp-completion--get-context trigger-chars))))
                              (completed (and resp
                                              (not (and (lsp-completion-list? resp)
                                                        (lsp:completion-list-is-incomplete resp)))))
                              (items (lsp--while-no-input
                                       (--> (cond
                                             ((lsp-completion-list? resp)
                                              (lsp:completion-list-items resp))
                                             (t resp))
                                         (if (or completed
                                                 (seq-some #'lsp:completion-item-sort-text? it))
                                             (lsp-completion--sort-completions it)
                                           it)
                                         (-map (lambda (item)
                                                 (lsp-put item
                                                          :_emacsStartPoint
                                                          (or (lsp-completion--guess-prefix item)
                                                              bounds-start)))
                                               it))))
                              (markers (list bounds-start (copy-marker (point) t)))
                              (prefix (buffer-substring-no-properties bounds-start (point)))
                              (lsp-completion--no-reordering (not lsp-completion-sort-initial-results)))
                        (lsp-completion--clear-cache same-session?)
                        (setf done? completed
                              lsp-completion--cache (list bounds-start
                                                          (cond
                                                           ((and done? (not (seq-empty-p items)))
                                                            (lsp-completion--to-internal items))
                                                           ((not done?) :incomplete))
                                                          :lsp-items nil
                                                          :markers markers
                                                          :prefix prefix)
                              result (lsp-completion--filter-candidates
                                      (cond (done?
                                             (cl-second lsp-completion--cache))
                                            (lsp-completion-filter-on-incomplete
                                             (lsp-completion--to-internal items)))
                                      :lsp-items items
                                      :markers markers
                                      :prefix prefix))))))
                (:interrupted lsp-completion--last-result)
                (`,res (setq lsp-completion--last-result res))))))
      (list
       bounds-start
       (point)
;; changed completion table
       (lambda (probe pred action &rest rest)
         (if (eq action 'metadata)
             '(metadata (category . lsp-capf)
                        (display-sort-function . identity)
                        (cycle-sort-function . identity))
           (complete-with-action action (funcall candidates) probe pred)))
;; end of changed completion table
       :annotation-function #'lsp-completion--annotate
       :company-kind #'lsp-completion--candidate-kind
       :company-deprecated #'lsp-completion--candidate-deprecated
       :company-require-match 'never
       :company-prefix-length
       (save-excursion
         (goto-char bounds-start)
         (and (lsp-completion--looking-back-trigger-characterp trigger-chars) t))
       :company-match #'lsp-completion--company-match
       :company-doc-buffer (-compose #'lsp-doc-buffer
                                     #'lsp-completion--get-documentation)
       :exit-function
       (-rpartial #'lsp-completion--exit-fn candidates)))))

(use-package! shell
  :config
  (defun +shell/toggle (&optional command)
    "Toggle a persistent terminal popup window.

If popup is visible but unselected, selected it.
If popup is focused, kill it."
    (interactive)
    (let ((buffer
           (get-buffer-create
            (format "*doom:shell-popup:%s*"
                    (if (bound-and-true-p persp-mode)
                        (safe-persp-name (get-current-persp))
                      "main"))))
          (dir default-directory))
      (if-let (win (get-buffer-window buffer))
          (let (confirm-kill-processes)
            (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
            (delete-window win))
        (with-current-buffer buffer
          (if (not (eq major-mode 'shell-mode))
              (shell buffer)
            (cd dir)
            (run-mode-hooks 'shell-mode-hook)))
        (pop-to-buffer buffer))
      (+shell--send-input buffer command))))

(defun +shell--send-input (buffer input &optional no-newline)
  (when input
    (with-current-buffer buffer
      (unless (number-or-marker-p (cdr comint-last-prompt))
        (message "Waiting for shell to start up...")
        (while (not (number-or-marker-p (cdr comint-last-prompt)))
          (sleep-for 0.1)))
      (goto-char (cdr comint-last-prompt))
      (delete-region (cdr comint-last-prompt) (point-max))
      (insert input)
      (comint-send-input no-newline))))

(use-package corfu-doc
  :config
  (setq corfu-doc-delay 0.2
        corfu-doc-max-width 80
        corfu-doc-max-height 40))

(use-package corfu
  :config
  (defun ++corfu-quit ()
    (interactive)
    (call-interactively 'corfu-quit)
    (evil-normal-state +1))
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0.01
        corfu-separator ?\s
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match t
        corfu-preview-current nil
        corfu-preselect-first t
        corfu-on-exact-match nil
        corfu-echo-documentation nil
        corfu-scroll-margin 10)
  (map! :map global-map
        :nvi "C-SPC" #'completion-at-point)
  (map! :map corfu-map
        :nvi "C-j" #'corfu-next
        :nvi "C-k" #'corfu-previous
        :nvi "C-l" #'corfu-insert
        :nvi "C-;" #'corfu-insert
        :nvi "TAB" #'corfu-insert
        :nvi "<tab>" #'corfu-insert
        :nvi "<escape>" #'++corfu-quit
        :nvi "ESC" #'++corfu-quit)
  (global-corfu-mode +1)
  (global-company-mode -1)
  (add-hook! '(prog-mode-hook
               text-mode-hook)
    (corfu-doc-mode +1)
    (unless (display-graphic-p)
      (corfu-terminal-mode +1)
      (corfu-doc-terminal-mode +1))))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; See https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless
(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

;; Add extensions
(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  (setq cape-dabbrev-min-length 2
        cape-dabbrev-check-other-buffers 'some))

(setq corfu-bar-width 0.5)
(custom-set-faces! `(corfu-bar :background ,(doom-color 'magenta)))

(defvar ++google-translate-kana->romaji-buffer "*Google Translate kana->romaji*")

(defun ++google-translate-kana->romaji (kana)
  (replace-regexp-in-string "\r?\n" ""
    (shell-command-to-string
     (format "echo \"%s\" | iconv -f utf8 -t eucjp | kakasi -i euc -w | kakasi -i euc -Ha -Ka -Ja -Ea -ka" kana))))

(defun ++google-translate-curr-region-kana->romaji ()
  (when (equal (buffer-name) "*Google Translate*")
    (if (region-active-p)
      (posframe-show ++google-translate-kana->romaji-buffer
        :string (++google-translate-kana->romaji (buffer-substring-no-properties
                                                   (region-beginning)
                                                   (+ (region-end) 1)))
        :position (point)
        :border-color (doom-color 'yellow)
        :border-width 1
        :foreground-color (doom-color 'yellow))
      (posframe-hide ++google-translate-kana->romaji-buffer))))

(defun ++google-translate-setup-hook ()
  (add-hook 'post-command-hook #'++google-translate-curr-region-kana->romaji))

(++google-translate-setup-hook)

(use-package! magit
  :config
  (defun th/magit--with-difftastic (buffer command)
    "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
    (let ((process-environment
           (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                         (number-to-string (frame-width)))
                 process-environment)))
      ;; Clear the result buffer (we might regenerate a diff, e.g., for
      ;; the current changes in our working directory).
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer))
      ;; Now spawn a process calling the git COMMAND.
      (make-process
       :name (buffer-name buffer)
       :buffer buffer
       :command command
       ;; Don't query for running processes when emacs is quit.
       :noquery t
       ;; Show the result buffer once the process has finished.
       :sentinel (lambda (proc event)
                   (when (eq (process-status proc) 'exit)
                     (with-current-buffer (process-buffer proc)
                       (goto-char (point-min))
                       (ansi-color-apply-on-region (point-min) (point-max))
                       (setq buffer-read-only t)
                       (view-mode)
                       (end-of-line)
                       ;; difftastic diffs are usually 2-column side-by-side,
                       ;; so ensure our window is wide enough.
                       (let ((width (current-column)))
                         (while (zerop (forward-line 1))
                           (end-of-line)
                           (setq width (max (current-column) width)))
                         ;; Add column size of fringes
                         (setq width (+ width
                                        (fringe-columns 'left)
                                        (fringe-columns 'right)))
                         (goto-char (point-min))
                         (pop-to-buffer
                          (current-buffer)
                          `(;; If the buffer is that wide that splitting the frame in
                            ;; two side-by-side windows would result in less than
                            ;; 80 columns left, ensure it's shown at the bottom.
                            ,(when (> 80 (- (frame-width) width))
                               #'display-buffer-at-bottom)
                            (window-width
                             . ,(min width (frame-width))))))))))))

  (defun th/magit-show-with-difftastic (rev)
    "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If REV is given, just use it.
            (when (boundp 'rev) rev)
            ;; If not invoked with prefix arg, try to guess the REV from
            ;; point's position.
            (and (not current-prefix-arg)
                 (or (magit-thing-at-point 'git-revision t)
                     (magit-branch-or-commit-at-point)))
            ;; Otherwise, query the user.
            (magit-read-branch-or-commit "Revision"))))
    (if (not rev)
        (error "No revision specified")
      (th/magit--with-difftastic
       (get-buffer-create (concat "*git show difftastic " rev "*"))
       (list "git" "--no-pager" "show" "--ext-diff" rev))))

  (defun th/magit-diff-with-difftastic (arg)
    "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If RANGE is given, just use it.
            (when (boundp 'range) range)
            ;; If prefix arg is given, query the user.
            (and current-prefix-arg
                 (magit-diff-read-range-or-commit "Range"))
            ;; Otherwise, auto-guess based on position of point, e.g., based on
            ;; if we are in the Staged or Unstaged section.
            (pcase (magit-diff--dwim)
              ('unmerged (error "unmerged is not yet implemented"))
              ('unstaged nil)
              ('staged "--cached")
              (`(stash . ,value) (error "stash is not yet implemented"))
              (`(commit . ,value) (format "%s^..%s" value value))
              ((and range (pred stringp)) range)
              (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
    (let ((name (concat "*git diff difftastic"
                        (if arg (concat " " arg) "")
                        "*")))
      (th/magit--with-difftastic
       (get-buffer-create name)
       `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

  (transient-define-prefix th/magit-aux-commands ()
    "My personal auxiliary magit commands."
    ["Auxiliary commands"
     ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
     ("s" "Difftastic Show" th/magit-show-with-difftastic)])

  (transient-append-suffix 'magit-dispatch "!"
    '("#" "My Magit Cmds" th/magit-aux-commands))

  (define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands))

(use-package! detached
  :init (detached-init)
  :bind (;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :config
  (setq detached-show-output-on-attach t
        detached-terminal-data-command system-type)
  (map! :map detached-shell-mode-map
         :nv "SPC d i" #'detached-shell-send-input
         :nv "SPC d a" #'detached-attach-session))

(defvar ++vscode-search-occ-bg "#470000")
(defvar ++vscode-search-occ-fg "#cccccc")

(setq doom-theme 'doom-one)

(use-package doom-themes
  :config
  ;; Use the colorful treemacs theme
  (setq doom-themes-treemacs-theme "doom-colors"
        doom-themes-enable-bold nil    ; if nil, bold is universally disabled
        doom-themes-enable-italic nil)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)

  ;; Custom style tweaks
  ;; See https://github.com/hlissner/emacs-doom-themes/blob/master/themes/doom-one-theme.el#L32
  ;; for the doom-colors
  (custom-set-faces!
    `(popup-tip-face :inherit popup-face
                     :foreground ,(doom-color 'yellow)
                     :box t)
    `(swiper-background-match-face-2 :background ,++vscode-search-occ-bg
                                     :foreground ,++vscode-search-occ-fg)
    `(swiper-match-face-2 :background ,++vscode-search-occ-bg
                          :foreground ,++vscode-search-occ-fg)
    `(swiper-line-face :background "DodgerBlue4"
                       :foreground ,++vscode-search-occ-fg)
    ;; TODO Move the LSP faces out of here?
    `(lsp-ui-peek-peek :background "#0a0014")
    `(lsp-ui-peek-selection :background ,++vscode-search-occ-bg
                            :foreground ,++vscode-search-occ-fg)
    `(lsp-ui-peek-list :background "grey5"
                       :height 1.0
                       :width condensed)
    `(lsp-ui-peek-header :background "#2a0e46"
                         :foreground "white"
                         :height 1.0
                         :width condensed)
    `(lsp-ui-peek-filename :foreground ,(doom-color 'yellow)
                           :height 1.0
                           :width condensed
                           :box (:line-width (1 . 10)
                                 :color "grey5"))
    `(lsp-ui-peek-line-number :foreground "grey5")
    `(lsp-ui-peek-highlight :background ,++vscode-search-occ-bg
                            :foreground "white"
                            :heght 1.0
                            :box nil
                            :inherit nil)
    '(show-paren-match :foreground nil
                       :background "#333"
                       :weight normal)
    `(ac-completion-face :foreground ,(doom-color 'yellow))
    `(ac-selection-face :foreground "black"
                        :background ,(doom-color 'magenta))
    '(hl-line :background "grey8")
    `(header-line :background "black"
                  :box (:line-width 1 :color "grey50"))
    `(popup-tip-face :foreground ,(doom-color 'yellow))
    ;; Ivy
    `(ivy-minibuffer-match-face-1 :foreground "white")
    ;; Tree-sitter
    '(tree-sitter-hl-face:punctuation :background nil)
    '(tree-sitter-hl-face:property :slant normal)
    `(tree-sitter-hl-face:string.special :weight normal :foreground ,(doom-color 'red))
    `(tree-sitter-hl-face:method.call :foreground ,(doom-color 'yellow))
    `(corfu-border :background "white")
    `(vertico-posframe-border :background ,(doom-color 'blue))
    `(minibuffer-prompt :foreground ,(doom-color 'blue))
    `(lsp-headerline-breadcrumb-path-face :foreground ,(doom-color 'yellow))
    `(lsp-headerline-breadcrumb-symbols-face :foreground ,(doom-color 'white))
    `(comint-highlight-prompt :foreground ,(doom-color 'yellow))
    `(highlight :background "#5a1111")
    `(doom-modeline-highlight :background ,(doom-color 'blue)
                              :foreground "black"))
  ;; GUI
  (if (display-graphic-p)
    (custom-set-faces!
      `(default :background "black")
      `(fill-column-indicator :foreground ,(doom-color 'base1))
      `(window-divider :foreground ,(doom-color 'magenta))
      '(flycheck-posframe-face
         :background "grey5"
         :foreground "white"
         :height 0.9
         :box (:line-width 1 :color "white"))
      `(flycheck-posframe-error-face
         :foreground ,(doom-color 'red)
         :box (:line-width 1 :color ,(doom-color 'red)))
      `(flycheck-posframe-warning-face
         :foreground ,(doom-color 'yellow)
         :box (:line-width 1 :color ,(doom-color 'yellow)))
      `(flycheck-posframe-info-face
         :foreground ,(doom-color 'green)
         :box (:line-width 1 :color ,(doom-color 'green))))
    ;; TERM
    (custom-set-faces!
      `(default :background "black")
      ;; Same as window-divider's
      `(header-line :background "#191b20")
      `(lsp-face-highlight-read :background "#34536c" :foreground "#dfdfdf")
      `(lsp-face-highlight-write :inherit lsp-face-highlight-read)
      `(lsp-face-highlight-textual :inherit lsp-face-highlight-read)
      `(flycheck-error :foreground ,(doom-color 'red) :underline t)
      `(flycheck-warning :foreground ,(doom-color 'yellow) :underline t))))

(setq window-divider-default-right-width 1
      window-divider-default-bottom-width 0)

(defvar ++font-size nil)
(defun ++screen-pixels->font-size (width-x-height)
  "Given WIDTH_X_HEIGHT, returns the adjusted font size"
  (let ((default-font-size 16))
    (cond ((member width-x-height
                   '((3440 1440))) 18)
          ((member width-x-height
                   '((1920 1080))) 14)
          ;; My Flux mac
          ((member width-x-height
                    '((1440 900))) 16)
          (t (progn
               (message (concat "Unhandled screen resolution " (prin1-to-string width-x-height) ". "
                                "Defaulting to font size " (prin1-to-string default-font-size)))
               default-font-size)))))

;; Stolen from https://github.com/hlissner/doom-emacs/issues/1500
(defun ++get-frame-list (&optional frame)
  "Return a list consisting of FRAME and all of FRAME's child frames."
  (let ((frame (or frame (selected-frame))))
    (cons (selected-frame)
          (cl-loop for fr in (frame-list)
                   if (eq (frame-parameter fr 'parent-frame) frame)
                   collect fr))))

(defun ++configure-font-size ()
  (let ((new-font-size (++screen-pixels->font-size
                        (cddr (frame-monitor-attribute 'geometry)))))
    (unless (equal new-font-size ++font-size)
      (setq doom-font (font-spec :family "Fantasque Sans Mono" :size new-font-size))
      (set-frame-font doom-font t (++get-frame-list)))
    (setq ++font-size new-font-size)))

(when (display-graphic-p)
 (run-at-time 0 nil (lambda () (++configure-font-size)))
 (setq ++adjust-font-timer (run-with-idle-timer 1 1 #'++configure-font-size)))

(after! doom-modeline
  (custom-set-faces!
    '(mode-line :background "#23102C" :height 0.9 :width condensed :box (:line-width 1 :color "grey40"))
    '(mode-line-inactive :height 0.9 :width condensed)
    '(mode-line-emphasis :inherit mode-line)
    '(doom-modeline-buffer-file :weight normal)))

(setq +doom-dashboard-functions '(doom-dashboard-widget-banner))

(setq display-time-default-load-average nil
      display-time-24hr-format t)

(setq show-paren-style 'expression)

(setq global-hl-line-modes nil)
(defun ++disable-hl-line ()
  (hl-line-mode -1))
(add-hook 'prog-mode-hook #'++disable-hl-line)
(add-hook 'text-mode-hook #'++disable-hl-line)

(setq display-line-numbers-type nil)

(setq-default frame-title-format '("Emacs"))
