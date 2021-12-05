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

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defvar ++sync-folder-path "~/Dropbox")

(defvar ++vscode-search-occ-bg "#48240a")
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
    `(swiper-background-match-face-2 :background ,++vscode-search-occ-bg
                                     :foreground ,++vscode-search-occ-fg)
    `(swiper-match-face-2 :background ,++vscode-search-occ-bg
                          :foreground ,++vscode-search-occ-fg)
    `(swiper-line-face :background "DodgerBlue4"
                       :foreground ,++vscode-search-occ-fg)
    ;; TODO Move the LSP faces out of here?
    `(lsp-ui-peek-peek :background "#000029")
    `(lsp-ui-peek-selection :background ,++vscode-search-occ-bg
                            :foreground ,++vscode-search-occ-fg)
    `(lsp-ui-peek-list :background "grey7"
                       :height 1.0
                       :width condensed)
    `(lsp-ui-peek-header :background "#000050"
                         :foreground "white"
                         :height 0.8
                         :width condensed)
    `(lsp-ui-peek-filename :foreground "#98be65"
                           :height 1.0
                           :width condensed
                           :box (:line-width (1 . 10)
                                 :color "grey7"))
    `(lsp-ui-peek-line-number :foreground "grey7")
    `(lsp-ui-peek-highlight :background ,++vscode-search-occ-bg
                            :foreground ,++vscode-search-occ-fg
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
    '(header-line :background "grey15"))
  ;; GUI
  (if (display-graphic-p)
      (custom-set-faces!
        `(default :background "black")
        `(fill-column-indicator :foreground ,(doom-color 'base1))
        `(window-divider :foreground "grey5")
        `(flycheck-posframe-error-face :background "firebrick"
                                       :foreground "white")
        `(flycheck-posframe-warning-face :background "dark goldenrod"
                                         :foreground "white"))
    ;; TERM
    (custom-set-faces!
      `(default :background "black")
      ;; Same as window-divider's
      `(header-line :background "#191b20")
      `(lsp-face-highlight-read :background "#34536c"
                                :foreground "#dfdfdf"))))

(setq window-divider-default-right-width 10)

(defvar ++font-size nil)
(defun ++screen-pixels->font-size (width-x-height)
  "Given WIDTH_X_HEIGHT, returns the adjusted font size"
  (let ((default-font-size 16))
    (cond ((member width-x-height
                   '((3440 1440))) 18)
          ((member width-x-height
                   '((1920 1080))) 15)
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
      (setq doom-font (font-spec :family "Ubuntu Mono" :size new-font-size))
      (set-frame-font doom-font t (++get-frame-list)))
    (setq ++font-size new-font-size)))

(when (display-graphic-p)
 (++configure-font-size)
 (setq ++adjust-font-timer (run-with-idle-timer 1 1 #'++configure-font-size)))

(defun ++ascii-banner-ansi-shadow ()
  (mapc (lambda (line)
          (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                              'face 'doom-dashboard-banner) " ")
          (insert "\n"))
        '("=================     ===============     ===============   ========  ========"
          "\\\\ . . . . . . .\\\\   //. . . . . . .\\\\   //. . . . . . .\\\\  \\\\. . .\\\\// . . //"
          "||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\\/ . . .||"
          "|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||"
          "||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||"
          "|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\\ . . . . ||"
          "||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\\_ . .|. .||"
          "|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\\ `-_/| . ||"
          "||_-' ||  .|/    || ||    \\|.  || `-_|| ||_-' ||  .|/    || ||   | \\  / |-_.||"
          "||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \\  / |  `||"
          "||    `'         || ||         `'    || ||    `'         || ||   | \\  / |   ||"
          "||            .===' `===.         .==='.`===.         .===' /==. |  \\/  |   ||"
          "||         .=='   ███████╗███╗   ███╗ █████╗  ██████╗███████╗  `==  \\/  |   ||"
          "||      .=='    _-██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝_  /|  \\/  |   ||"
          "||   .=='    _-'  █████╗  ██╔████╔██║███████║██║     ███████╗ `' |. /|  |   ||"
          "||.=='    _-'     ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║     `' |  /==.||"
          "=='    _-'        ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║         \\/   `=="
          "\\   _-'           ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝          `-_   /"
          "`''                                                                      ``'")))
(setq +doom-dashboard-ascii-banner-fn #'++ascii-banner-ansi-shadow)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(require 'clojure-rand-ref)

(defun ++dashboard-trivia ()
 (clojure-rand-ref
  (lambda (entry)
    (with-current-buffer +doom-dashboard-name
      (when entry
        (read-only-mode -1)
        (goto-char (point-min))
        (forward-line 5)
        (insert "Clojure Trivia\n\n")
        (insert-text-button (concat "  " (plist-get entry :symbol) "\n")
                            'action (lambda (_)
                                      (+doom-dashboard-reload t)
                                      (++dashboard-trivia)
                                      (browse-url (plist-get entry :link)))
                            'face 'doom-dashboard-menu-title
                            'mouse-face 'doom-dashboard-menu-title
                            'follow-link t)
        (insert "  " (plist-get entry :description) "\n")
        (read-only-mode +1))))))

(advice-add #'+doom-dashboard-init-h :after #'++dashboard-trivia)

(after! doom-modeline
  (custom-set-faces!
    '(mode-line :background "#23102C" :height 0.9 :width condensed)
    '(mode-line-inactive :height 0.9 :width condensed)
    '(mode-line-emphasis :inherit mode-line)
    '(doom-modeline-buffer-file :weight normal)))

(setq display-time-default-load-average nil
      display-time-24hr-format t
      display-line-numbers-type 'relative)

(setq show-paren-style 'expression)

(map! :map doom-leader-map "w SPC" #'ace-select-window)

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red")))

;; (use-package! aggressive-indent
;;   :config
;;   (defvar ++aggressive-indent-loc-threshold 500)
;;   (defun ++aggressive-indent-mode-setup ()
;;     (interactive)
;;     (unless (memql major-mode aggressive-indent-excluded-modes)
;;       (aggressive-indent-mode
;;        (if (< (count-lines (point-min) (point-max))
;;               ++aggressive-indent-loc-threshold)
;;            (progn (-> (format "ENABLING aggressive-index (LOC is < threshold %s)"
;;                               ++aggressive-indent-loc-threshold)
;;                       (propertize 'face '(:foreground "green"))
;;                       (message))
;;                   +1)
;;          (progn (-> (format "DISABLING aggressive-index (LOC is >= threshold %s)"
;;                             ++aggressive-indent-loc-threshold)
;;                     (propertize 'face '(:foreground "red"))
;;                     (message))
;;                 -1)))))
;;   (add-hook! '(clojure-mode-hook
;;                clojurescript-mode-hook
;;                clojurec-mode-hook
;;                lisp-mode-hook
;;                emacs-lisp-mode-hook
;;                css-mode-hook)
;;              #'++aggressive-indent-mode-setup))

(require 'alert)
(setq alert-default-style 'notifications
      alert-fade-time 30)

(use-package! all-the-icons
  :config (setq all-the-icons-scale-factor 0.90))

(setq avy-timeout-seconds 0.1)

(setq bookmark-default-file (concat ++sync-folder-path "/emacs/bookmarks"))

(setq bookmark-save-flag 1)

(map! :leader
      :desc "Find-replace" "r" #'anzu-query-replace)
(map! :leader
      :desc "Find-replace (regexp)" "R" #'anzu-query-replace-regexp)

(after! centaur-tabs
  (setq centaur-tabs-style "rounded"
        centaur-tabs-height 5
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-headline-match)
  (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t))

(use-package! cider
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil
        cider-dynamic-indentation nil
        cider-font-lock-dynamically nil
        cider-font-lock-reader-conditionals nil
        nrepl-force-ssh-for-remote-hosts t)
  (map! :map cider-inspector-mode-map
        :nv "C-k" #'cider-inspector-previous-inspectable-object
        :nv "C-j" #'cider-inspector-next-inspectable-object
        :nv "C-h" #'cider-inspector-pop
        :nv [mouse-3] #'cider-inspector-pop
        :nv "C-l" #'cider-inspector-operate-on-point))

(defun nrepl--ssh-tunnel-connect (host port)
  "Connect to a remote machine identified by HOST and PORT through SSH tunnel."
  (message "[nREPL] Establishing SSH tunneled connection to %s:%s ..." host port)
  (let* ((remote-dir (if host (format "/ssh:%s:" host) default-directory))
         (local-port (nrepl--random-free-local-port))
         (ssh (or (executable-find "ssh")
                  (error "[nREPL] Cannot locate 'ssh' executable")))
         (cmd (nrepl--ssh-tunnel-command ssh remote-dir port local-port))
         (tunnel-buf (nrepl-tunnel-buffer-name
                      `((:host ,host) (:port ,port))))
         (tunnel (start-process-shell-command "nrepl-tunnel" tunnel-buf cmd)))
    (process-put tunnel :waiting-for-port t)
    (set-process-filter tunnel (nrepl--ssh-tunnel-filter local-port))
    (while (and (process-live-p tunnel)
                (process-get tunnel :waiting-for-port))
      (accept-process-output nil 0.005))
    (if (not (process-live-p tunnel))
        (error "[nREPL] SSH port forwarding failed.  Check the '%s' buffer" tunnel-buf)
      (message "[nREPL] SSH port forwarding established to localhost:%s" local-port)
      (let ((endpoint (nrepl--direct-connect "localhost" local-port)))
        (thread-first endpoint
          (plist-put :tunnel tunnel)
          (plist-put :remote-host host))))))

(defun nrepl--random-free-local-port ()
  (let* ((random-free-local-port-cmd (concat "comm -23 "
                                             "<(seq 1024 65535 | sort) "
                                             "<(ss -Htan | awk '{print $4}' | cut -d':' -f2 | sort -u) | "
                                             "shuf | head -n 1")))
    (with-temp-buffer
           (insert (string-trim-right (shell-command-to-string random-free-local-port-cmd)))
           (buffer-string))))

(defun nrepl--ssh-tunnel-command (ssh dir remote-port local-port)
  "Command string to open SSH tunnel to the host associated with DIR's PORT."
  (with-parsed-tramp-file-name dir v
     ;; this abuses the -v option for ssh to get output when the port
    ;; forwarding is set up, which is used to synchronise on, so that
    ;; the port forwarding is up when we try to connect.
    (format-spec
     "%s -v -N -L %l:localhost:%p %u'%h'"
     `((?s . ,ssh)
       (?l . ,local-port)
       (?p . ,remote-port)
       (?h . ,v-host)
       (?u . ,(if v-user (format "-l '%s' " v-user) ""))))))

(after! company
  (setq company-idle-delay 0.0
        company-tooltip-idle-delay 0.2
        company-minimum-prefix-length 2
        company-selection-wrap-around t)
  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
  (if (display-graphic-p)
      (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
    ;; Terminal seems to work with just "TAB"
    (define-key company-active-map (kbd "TAB") 'company-complete-selection))
  (define-key company-mode-map (kbd "C-SPC") 'company-manual-begin))

(define-key global-map (kbd "C-j") nil)
(define-key global-map (kbd "C-k") nil)

(use-package! dotenv-mode
  :config (add-to-list 'auto-mode-alist '("\\.env\\..*" . dotenv-mode)))

;; (require 'edbi)

(require 'ejc-sql)
(require 'ejc-autocomplete)
(require 'ejc-direx)
(use-package! ejc-sql
  :config
  (setq ejc-ring-length 10000
        ejc-result-table-impl 'ejc-result-mode
        ejc-complete-on-dot t
        ejc-sql-separator "---")
  (set-popup-rules!
    '(("^database: "
       :quit nil
       :side left
       :size 75
       :select t)
      ("*ejc-sql-output*"
       :quit nil
       :side bottom
       :size 30
       :select nil)))
  (add-hook 'sql-mode-hook (lambda ()
                             (ejc-sql-mode t)
                             (map! :nv "SPC a" #'ejc-eval-user-sql-at-point)))
  (add-hook 'ejc-result-mode-hook (lambda () (visual-line-mode -1)))
  (add-hook 'ejc-sql-minor-mode-hook
            (lambda ()
              (company-mode -1)
              (auto-complete-mode +1)
              (ejc-ac-setup)
              ;; Fuzzy doesn't seem to work though. TODO Find out why
              (setq ac-use-fuzzy t
                    ac-fuzzy-enable t
                    ac-menu-height 10
                    ac-candidate-max 10
                    ac-delay 0.5
                    ac-auto-show-menu 0.5)
              (map! :map ac-completing-map
                    "C-k" #'ac-previous
                    "C-j" #'ac-next
                    "<tab>" #'ac-complete)))
  (add-hook 'ejc-sql-connected-hook (lambda ()
                                      (ejc-set-fetch-size 100)
                                      (ejc-set-max-rows 100)
                                      (ejc-set-show-too-many-rows-message t)
                                      (ejc-set-column-width-limit 50)
                                      (ejc-set-use-unicode t))))

(defun ejx-direx:make-buffer-prefixed ()
  (let ((current-ejc-db ejc-db)
        (buf (direx:ensure-buffer-for-root
              (make-instance 'ejc-direx:database
                             :name (format "database: %s" (ejc-get-db-name ejc-db))
                             :buffer (current-buffer)
                             :file-name (buffer-file-name)
                             :cache (cons nil (ejc-direx:get-structure))))))
    (with-current-buffer buf
      (setq-local ejc-db current-ejc-db))
    buf))

(defun ejx-direx:show ()
  (interactive)
  (pop-to-buffer (ejx-direx:make-buffer-prefixed)))

(defun start-elcord ()
  (interactive)
  (use-package! elcord
    :config
    (setq elcord-refresh-rate 5
          elcord-use-major-mode-as-main-icon t)
    (elcord-mode +1)
    (message "Started elcord")))

(defun stop-elcord ()
  (interactive)
  (elcord-mode -1)
  (message "Stopped elcord"))

(define-key evil-insert-state-map (kbd "C-j") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-motion-state-map (kbd "<tab>") nil)

(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "C-S-o") 'evil-jump-forward)

(setq +evil-want-o/O-to-continue-comments nil)

(use-package! evil-lisp-state
  :init (setq evil-lisp-state-global t)
  :config (evil-lisp-state-leader "SPC k"))

(unbind-key "K" evil-normal-state-map)
(unbind-key "K" evil-visual-state-map)
(unbind-key "K" evil-motion-state-map)

(setq evil-want-fine-undo t)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(when (display-graphic-p)
  (setq evil-want-minibuffer t))

(evil-collection-init)

(define-fringe-bitmap 'flycheck-fringe-bitmap-beam
  (vector #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111
          #b11111111))

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

(setq flycheck-display-errors-delay 0.01)

(require 'keychain-environment)
(keychain-refresh-environment)

(add-hook! '(text-mode-hook prog-mode-hook) #'idle-highlight-mode)

(require 'i3wm-config-mode)

(setq ielm-noisy nil
      ielm-prompt "λ> ")

(require 'itail)

(use-package! iscroll
  :config (iscroll-mode +1))

(setq ispell-dictionary "en")

(after! ivy-posframe
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-top-center)
  (setf (alist-get 'swiper ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-top-center)
  (setq ivy-posframe-border-width 10
        ivy-posframe-width 120
        ivy-posframe-parameters (append ivy-posframe-parameters '((left-fringe . 3)
                                                                  (right-fringe . 3)))))

(setq posframe-arghandler
      (lambda (_buffer-or-name key value)
        (or (eq key :lines-truncate)
            value)))

(setq ivy-extra-directories ())

(after! counsel
  (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s || true"))

(map! :map ivy-occur-grep-mode-map
      :n "c" (cmd! (setq ivy-calling (not ivy-calling))))

(add-hook 'ivy-occur-grep-mode-hook
          (cmd! (setq ivy-calling t)))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

(use-package lsp-mode
  :config
  (add-hook! '(prog-mode-hook)
    (setq lsp-completion-enable t))
  (add-hook! '(clojure-mode-hook
               clojurescript-mode-hook
               clojurec-mode-hook)
    (setq lsp-completion-enable nil)))

(after! lsp-mode
  (setq lsp-lens-enable t
        lsp-log-io nil
        lsp-idle-delay 0.2
        lsp-completion-no-cache nil
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-completion-sort-initial-results nil
        lsp-completion-use-last-result nil
        lsp-eldoc-enable-hover nil
        lsp-lens-place-position 'end-of-line
        lsp-enable-indentation t)
  (set-popup-rules!
    '(("*Flycheck errors*"
       :quit nil
       :side bottom
       :size 20
       :select nil)
      ("*lsp-help*"
       :quit t
       :side left
       :size 120
       :select t
       :modeline t))))

(after! lsp-mode
  ;; Clojure(Script)
  (dolist (to-ignore '("[/\\\\]\\.clj-kondo$"
                       "[/\\\\]\\.shadow-cljs$"
                       "[/\\\\]resources$"))
    (add-to-list 'lsp-file-watch-ignored to-ignore)))

(after! lsp-ui
  (define-key lsp-ui-peek-mode-map (kbd "j") 'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "k") 'lsp-ui-peek--select-prev)
  (define-key lsp-ui-peek-mode-map (kbd "C-k") 'lsp-ui-peek--select-prev-file)
  (define-key lsp-ui-peek-mode-map (kbd "C-j") 'lsp-ui-peek--select-next-file)
  (define-key evil-normal-state-map (kbd "g f") 'lsp-ui-peek-find-references)
  (map! :map lsp-mode-map
        :nv "SPC c m" #'lsp-ui-imenu
        :nv "SPC d" #'lsp-ui-doc-glance)
  (setq lsp-ui-peek-fontify 'always
        lsp-ui-peek-list-width 100
        lsp-ui-peek-peek-height 40

        ;; These can be brought up on-demand with SPC d
        lsp-ui-doc-enable nil
        ;; Prevents LSP peek to disappear when mouse touches it
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 0
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-max-width 120
        lsp-ui-doc-max-height 120
        lsp-ui-doc-header nil


        lsp-ui-imenu-enable t

        ;; This is just annoying, really
        lsp-ui-sideline-enable nil))

(when (display-graphic-p)
  (defun lsp-ui-peek--peek-display (src1 src2)
    (-let* ((win-width (frame-width))
            (lsp-ui-peek-list-width (/ (frame-width) 2))
            (string (-some--> (-zip-fill "" src1 src2)
                      (--map (lsp-ui-peek--adjust win-width it) it)
                      (-map-indexed 'lsp-ui-peek--make-line it)
                      (-concat it (lsp-ui-peek--make-footer)))))
      (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
      (posframe-show lsp-ui-peek--buffer
                     :string (mapconcat 'identity string "")
                     :min-width (frame-width)
                     :poshandler #'posframe-poshandler-frame-center)))

  (defun lsp-ui-peek--peek-destroy ()
    (when (bufferp lsp-ui-peek--buffer)
      (posframe-delete lsp-ui-peek--buffer))
    (setq lsp-ui-peek--buffer nil
          lsp-ui-peek--last-xref nil)
    (set-window-start (get-buffer-window) lsp-ui-peek--win-start))

  (advice-add #'lsp-ui-peek--peek-new :override #'lsp-ui-peek--peek-display)
  (advice-add #'lsp-ui-peek--peek-hide :override #'lsp-ui-peek--peek-destroy))

(require 'logview)

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style nil
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

(use-package! nyan-mode
  :config
  (setq nyan-minimum-window-width 100
        nyan-mark-modified-buffers t)
  (nyan-mode +1))

(after! org
  (setq org-directory (concat ++sync-folder-path "/org")
        org-default-notes-file (concat org-directory "/notes/default.org")
        org-agenda-files (cl-map 'list (lambda (f) (concat org-directory "/" f))
                                 '("life"
                                   "work"
                                   "captures"
                                   "notes")))
  (setq org-agenda-span 60
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
        org-superstar-headline-bullets-list '("▪")
        org-superstar-cycle-headline-bullets 1
        org-superstar-todo-bullet-alist '("▪")
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

(use-package! org-download
  :config (setq org-download-method 'attach))

(add-hook 'dired-mode-hook 'org-download-enable)

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
           :kill-buffer t))))

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

(use-package! chrome
  :config (setq chrome-auto-retrieve t
                ++chrome-host "127.0.0.1"
                ++chrome-port 9222))

(defun ++chrome-new-tab ()
  (interactive)
  (chrome--devtools-do
   (chrome-tab-create :host ++chrome-host
                      :port ++chrome-port
                      :session '(++chrome-port . ++chrome-host))
   "new"))

(map! :map chrome-mode-map
      :nv "l" #'chrome-visit-tab
      :nv "x" #'chrome-delete-tab
      :nv "n" #'++chrome-new-tab)

(setq ++chrome-tabs-retriever-timer nil)
(add-hook 'window-configuration-change-hook
          (lambda ()
            (if (eq major-mode 'chrome-mode)
                ;; Start
                (when (not ++chrome-tabs-retriever-timer)
                  (setq ++chrome-tabs-retriever-timer (run-at-time nil 1 #'chrome-retrieve-tabs)))
              ;; Stop
              (when ++chrome-tabs-retriever-timer
                (cancel-timer ++chrome-tabs-retriever-timer)
                (setq ++chrome-tabs-retriever-timer nil)))))

(setq persp-save-dir (concat ++sync-folder-path "/emacs/sessions/"))

(map! :nv "SPC f g" #'projectile-find-file-other-window)

(defun ++set-projectile-cache-duration ()
  (setq projectile-files-cache-expire
        (if (and buffer-file-name
            (file-remote-p (file-truename buffer-file-name)))
       (* 10 60) ; Long-ish projectile cache for remote files
     10)))

(use-package! projectile
  :config
  (add-hook 'find-file-hook #'++set-projectile-cache-duration))

(use-package! projectile-git-autofetch
  :config
  (setq projectile-git-autofetch-notify nil)
  (projectile-git-autofetch-mode +1))

(add-hook! '(text-mode-hook prog-mode-hook) (cmd! (rainbow-mode +1)))

(use-package! ranger
  :config
  (setq ranger-override-dired 'ranger
        ranger-show-hidden t))

(use-package! screenshot)

(add-hook 'shell-mode-hook (lambda () (company-mode -1)))

(use-package! slime
  :config
  (map! :map slime-mode-map
        :nv "SPC d" #'slime-describe-symbol
        :nv "SPC m e e" #'slime-eval-last-expression
        :nv "SPC m '" #'slime-connect))

(require 'smooth-scrolling)

(use-package! symex
  :config
  (symex-initialize)
  (map! :map doom-leader-map "k" #'symex-mode-interface)
  (setq symex-modal-backend 'evil))

(use-package! speed-dial
  :config
  (speed-dial-mode +1)
  (speed-dial-apply '(("C-c 1" . "~/Dropbox/work/audience-republic/misc.el")
                      ("C-c 2" . "~/Dropbox/life/todos.org")
                      ("C-c 3" . "~/Dropbox/blog/content-org"))))

(use-package! thread-dump)

(add-hook 'treemacs-mode-hook
          (lambda ()
            (when (display-graphic-p)
              (text-scale-decrease 1.5))))

(with-eval-after-load 'treemacs-icons
  (when (display-graphic-p)
    (treemacs-resize-icons 10)))

(use-package treemacs
  :commands (treemacs)
  :bind (("<f8>" . treemacs)
         ("<f9>" . treemacs-select-window))
  :config
  (progn
    (when window-system
      (setq treemacs-width 50
            treemacs-is-never-other-window t
            treemacs-file-event-delay 1000
            treemacs-show-cursor t
            treemacs--width-is-locked nil
            treemacs-space-between-root-nodes nil
            treemacs-filewatch-mode t
            treemacs-fringe-indicator-mode t
            treemacs-read-string-input 'from-minibuffer))
    (when (not (display-graphic-p))
      (setq treemacs-no-png-images t))))

(which-key-mode +1)

(map! :map doom-leader-map "z" #'writeroom-mode)

(map! :leader :desc "Lookup doc" :n "e" #'+lookup/documentation)

(use-package! clojure-mode
  :config
  (setq clojure-align-forms-automatically t))

(add-to-list 'dash-docs-docsets "Clojure")

(add-hook 'clojure-mode-hook
          '(lambda ()
             ;; Set some new syntax-highlighting rules.
             ;; Guardrail's >defn
             ;; Highlight particular macros similar to built-in stuff
             ;; For example, highlight ghostwheel's `>defn' similar
             ;; the same way as built-in `defn'
             (font-lock-add-keywords nil
                                     ;; So many escape codes! But we're really just saying:
                                     ;; Match the '(' character.
                                     ;; Match and group the string '>defn'.
                                     ;; Match some whitespace. \\s-+
                                     ;; Match and group some word characters. \\w+
                                     '(("(\\(>defn\\)\\s-+\\(\\w+\\)"
                                        ;; The first regexp group is a keyword.
                                        (1 font-lock-keyword-face)
                                        ;; The second regexp group is a name.
                                        (2 font-lock-function-name-face))))
             (put '>defn 'clojure-doc-string-elt 2)))

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

(plist-put! +ligatures-extra-symbols
            :lambda-prime "ƛ")

(set-ligatures! 'clojure-mode nil)
(set-ligatures! 'clojurescript-mode nil)
(set-ligatures! 'clojure-mode
  :lambda "fn")
(set-ligatures! 'clojurescript-mode
  ;; Account for re-frame debux forms
  :lambda "fn"
  :lambda-prime "fn-traced")

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

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map)))

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

;; TODO Should this be part of a use-package! call?
(setq typescript-indent-level 2)

;; TODO Is this redundant the setting of indentation somewhere else?
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

(use-package! gherkin-mode
  :config (add-to-list 'auto-mode-alist '("\\.feature\\'" . gherkin-mode)))

(add-hook 'vue-mode-hook #'lsp)

(setq byte-compile-warnings '(not obsolete))

(add-to-list 'term-file-aliases '("alacritty" . "xterm"))

(global-visual-line-mode t)

(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

(add-hook 'prog-mode-hook (cmd! (setq indent-tabs-mode nil)
                                (doom/set-indent-width 2)))

(setq ++safe-vars '((+format-on-save-enabled-modes . '())
                    (cider-required-middleware-version . "0.25.5")))
(-each ++safe-vars (lambda (pair)
                     (add-to-list 'safe-local-variable-values pair)))

(setq +format-on-save-enabled-modes
      '(emacs-lisp-mode
        erlang-mode))

(let ((modes '(clojure-mode
               clojurescript-mode
               clojurec-mode
               emacs-lisp-mode
               org-mode)))
  (setq +ligatures-in-modes modes)
  (setq +ligatures-extras-in-modes modes))

(when (not (display-graphic-p))
  (setq debug-on-error nil))

(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode +1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(map! :map global-map "C-h" #'backward-kill-word)

(setq x-select-enable-clipboard-manager nil)

(setq-default line-spacing 0.25)

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

(defun ++tmux-go (&optional quiet?)
  (interactive)
  (++async-shell-command "tmux list-sessions | awk '$0=$1' | sed s/://"
                         (lambda (sessions-str)
                           (let* ((no-sessions (string-match-p "^no server running on.*$" sessions-str))
                                  (sessions (if no-sessions '() (split-string sessions-str))))
                             (ivy-read "Select tmux session: " sessions
                                       :action (lambda (selected-session)
                                                 (if (not (member selected-session sessions))
                                                     ;; Create a new session
                                                     (progn
                                                       (message (concat "Creating new tmux session: " selected-session))
                                                       (if quiet?
                                                           (++tmux--new-session-quiet selected-session)
                                                         (++tmux--new-session selected-session)))
                                                   ;; Switch to an existing session
                                                   (progn
                                                     (message (concat "Selecting existing session " selected-session))
                                                     (if quiet?
                                                         (++tmux--switch-session-quiet selected-session)
                                                       (++tmux--switch-session selected-session))))))))))

(map! :n "SPC _" (cmd! (++tmux-go))
      :n "SPC -" (cmd! (++tmux-go t)))

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
      "g t" #'next-buffer
      "g T" #'previous-buffer)

(map! :map doom-leader-map "l p" #'list-processes)

(defun ++ox-hugo-generate-headers ()
  (interactive)
  (save-excursion
    (insert "#+HUGO_BASE_DIR: ..\n")
    (insert "#+HUGO_SECTION: post\n")
    (insert (concat "#+HUGO_CUSTOM_FRONT_MATTER: :date " (format-time-string "%Y-%m-%d") " :pin false :summary \"TODO\"\n"))
    (insert "#+HUGO_TAGS: \"TODO\"\n")))

(defun ++load-and-continuously-save (file)
  (interactive
   (let ((session-file (doom-session-file)))
     (list (or (read-file-name "Regularly saving session to: "
                               (file-name-directory session-file)
                               (file-name-nondirectory session-file))
               (user-error "No session selected. Aborting")))))
  (unless file
    (error "No session file selected"))
  ;; Load the session
  (doom/load-session file)
  ;; Clear any previous calls to this fn
  (when (boundp '++continuous-saving-timer)
    (cancel-timer ++continuous-saving-timer))
  ;; Save the session every 10 seconds
  (setq ++continuous-saving-timer
        (run-with-timer 1 10 (cmd!
                              ;; (message "Saving '%s' session" file)
                              (let ((message-log-max nil)
                                    (inhibit-message t))
                                (doom-save-session file))))))
(map! :map doom-leader-map "q N" '++load-and-continuously-save)

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
                         (lambda (buf) (string-match-p (concat "\\*"
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
                                        '(display-buffer-in-side-window . ((side . left)
                                                                           (slot . -1))))))))

(map! :map clojure-mode-map :nv "SPC m r p" #'++cider-popup)
(map! :map clojurescript-mode-map :nv "SPC m r p" #'++cider-popup)
(map! :map clojurec-mode-map :nv "SPC m r p" #'++cider-popup)
