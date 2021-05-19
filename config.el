;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 'dash)

(setq user-full-name "Rafael Nicdao"
      user-mail-address "nicdaoraf@gmail.com")

(defun ++kb (bytes) (* bytes 1024))
(defun ++mb (bytes) (* (++kb bytes) 1024))

(defun ++with-face (str &rest face-plist)
  (propertize str 'face face-plist))

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
                       :weight normal))
  ;; GUI
  (if (display-graphic-p)
      (custom-set-faces!
        `(default :background "black")
        `(fill-column-indicator :foreground ,(doom-color 'base1))
        `(window-divider :foreground ,(doom-color 'magenta))
        `(flycheck-posframe-error-face :background "firebrick"
                                       :foreground "white")
        `(flycheck-posframe-warning-face :background "dark goldenrod"
                                         :foreground "white"))
    ;; TERM (Alacritty)
    ;; Weirdly, "black" is more like "dark grey"
    (custom-set-faces!
      `(default :background "color-16")
      `(header-line :background "black"))))

(setq doom-font (font-spec :family "Ubuntu Mono"
                           :size (or (string-to-number (getenv "EMACS_FONT_SIZE"))
                                     16)))

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

(defvar phrase-api-url
  (nth (random 3)
       '(("https://corporatebs-generator.sameerkumar.website/" :phrase)
         ("https://useless-facts.sameerkumar.website/api" :data)
         ("https://dev-excuses-api.herokuapp.com/" :text))))
(defvar phrase-last nil)
(defvar phrase-timeout 5)

(defmacro phrase-generate-callback (token &optional format-fn ignore-read-only callback buffer-name)
  `(lambda (status)
     (unless (plist-get status :error)
       (goto-char url-http-end-of-headers)
       (let ((phrase (plist-get (json-parse-buffer :object-type 'plist) (cadr phrase-api-url)))
             (inhibit-read-only ,(when (eval ignore-read-only) t)))
         (setq phrase-last (cons phrase (float-time)))
         (with-current-buffer ,(or (eval buffer-name) (buffer-name (current-buffer)))
           (save-excursion
             (goto-char (point-min))
             (when (search-forward ,token nil t)
               (with-silent-modifications
                 (replace-match "")
                 (insert ,(if format-fn format-fn 'phrase)))))
           ,callback)))))

(defmacro phrase-insert-async (&optional format-fn token ignore-read-only callback buffer-name)
  `(let ((inhibit-message t))
     (if (and phrase-last
              (> phrase-timeout (- (float-time) (cdr phrase-last))))
         (let ((phrase (car phrase-last)))
           ,(if format-fn format-fn 'phrase))
       (url-retrieve (car phrase-api-url)
                     (phrase-generate-callback ,(or token "\ufeff") ,format-fn ,ignore-read-only ,callback ,buffer-name))
       ;; For reference, \ufeff = Zero-width no-break space / BOM
       ,(or token "\ufeff"))))

(defun doom-dashboard-phrase ()
  (phrase-insert-async
   (progn
     (setq-local phrase-position (point))
     (mapconcat
      (lambda (line)
        (+doom-dashboard--center
         +doom-dashboard--width
         (with-temp-buffer
           (insert-text-button
            line
            'action
            (lambda (_)
              (setq phrase-last nil)
              (+doom-dashboard-reload t))
            'face 'doom-dashboard-menu-title
            'mouse-face 'doom-dashboard-menu-title
            'help-echo "Random phrase"
            'follow-link t)
           (buffer-string))))
      (split-string
       (with-temp-buffer
         (insert phrase)
         (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
         (fill-region (point-min) (point-max))
         (buffer-string))
       "\n")
      "\n"))
   nil t
   (progn
     (goto-char phrase-position)
     (forward-whitespace 1))
   +doom-dashboard-name))

(defadvice! doom-dashboard-widget-loaded-with-phrase ()
  :override #'doom-dashboard-widget-loaded
  (setq line-spacing 0.2)
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (doom-display-benchmark-h 'return))
    'face 'doom-dashboard-loaded)
   "\n"
   (doom-dashboard-phrase)
   "\n"))

(after! doom-modeline
  (custom-set-faces!
    '(mode-line :height 0.9 :width condensed)
    '(mode-line-inactive :height 0.9 :width condensed)
    '(mode-line-emphasis :inherit mode-line)
    '(doom-modeline-buffer-file :weight normal))
  ;; TERM (Alacritty)
  (unless (display-graphic-p)
    (custom-set-faces!
      `(mode-line :background "darkred")
      `(mode-line-inactive :background "black"))))

(setq display-time-default-load-average nil
      display-time-24hr-format t
      display-line-numbers-type 'relative)

(add-hook 'hl-line-mode-hook (lambda () (setq hl-line-mode nil)))

(setq show-paren-style 'expression)

(map! :map doom-leader-map "w SPC" #'ace-select-window)

(use-package! all-the-icons
  :config (setq all-the-icons-scale-factor 0.90))

(setq avy-timeout-seconds 0.1)

(setq bookmark-default-file (concat ++sync-folder-path "/emacs/bookmarks"))

(setq bookmark-save-flag 1)

(map! :leader
      :desc "Query and replace within region" "r" #'query-replace)

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

(after! company
  (setq company-idle-delay 0.0
        company-tooltip-idle-delay 0.2
        company-minimum-prefix-length 2)
  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
  (if (display-graphic-p)
      (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
    ;; Terminal seems to work with just "TAB"
    (define-key company-active-map (kbd "TAB") 'company-complete-selection))
  (define-key company-mode-map (kbd "C-SPC") 'company-manual-begin))

(define-key global-map (kbd "C-j") nil)
(define-key global-map (kbd "C-k") nil)

(require 'edbi)

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

(use-package! evil-lisp-state
  :init (setq evil-lisp-state-global t)
  :config (evil-lisp-state-leader "SPC k"))

(setq ielm-noisy nil
      ielm-prompt "λ> ")

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

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

(after! lsp-mode
  (custom-set-faces!
    '(header-line :height 0.90))
  (setq lsp-lens-enable nil
        lsp-log-io nil
        lsp-completion-no-cache nil
        lsp-completion-enable t
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-completion-sort-initial-results nil
        lsp-completion-use-last-result nil))

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

        lsp-ui-doc-enable nil
        ;; Prevents LSP peek to disappear when mouse touches it
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 0
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-max-width 120
        lsp-ui-doc-max-height 120
        lsp-ui-doc-header t

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

(use-package! org
  :init
  (setq org-directory (concat ++sync-folder-path "/org")
        org-default-notes-file (concat org-directory "/notes/default.org")
        org-agenda-files (cl-map 'list (lambda (f) (concat org-directory "/" f))
                                 '("life"
                                   "work"
                                   "captures"
                                   "notes")))
  :config
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
        org-edit-src-content-indentation 2
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

(setq persp-save-dir (concat ++sync-folder-path "/emacs/sessions/"))

(map! :nv "SPC f g" #'projectile-find-file-other-window)

(use-package! projectile
  :config
  (setq projectile-files-cache-expire 10))

(add-hook! '(text-mode-hook prog-mode-hook) (cmd! (rainbow-mode +1)))

(add-hook 'shell-mode-hook (lambda () (company-mode -1)))

(require 'smooth-scrolling)

(require 'synosaurus)

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
  :init
  (progn
    (when window-system
      (setq treemacs-width 30
            treemacs-is-never-other-window t
            treemacs-file-event-delay 1000
            treemacs-show-cursor t
            treemacs--width-is-locked nil
            treemacs-space-between-root-nodes nil
            treemacs-filewatch-mode t
            treemacs-fringe-indicator-mode t))
    (when (not (display-graphic-p))
      (setq treemacs-no-png-images t))))

(which-key-mode +1)

(map! :leader :desc "Lookup doc" :n "e" #'+lookup/documentation)

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
     :side bottom
     :size 0.2
     :select t)
    ("^\\*\\(?:cider-doc\\|lsp-help\\)"
     :side right
     :size 0.5)))

(plist-put! +ligatures-extra-symbols
            :lambda-prime "ƛ")

(set-ligatures! 'clojurescript-mode
  ;; Account for re-frame debux forms
  :lambda-prime "fn-traced")

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map)))

;; TODO Should this be part of a use-package! call?
(setq typescript-indent-level 2)

;; TODO Is this redundant the setting of indentation somewhere else?
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

(use-package! gherkin-mode
  :config (add-to-list 'auto-mode-alist '("\\.feature\\'" . gherkin-mode)))

(setq byte-compile-warnings '(not obsolete))

(add-to-list 'term-file-aliases '("alacritty" . "xterm"))

(global-visual-line-mode t)

(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

(add-hook 'prog-mode-hook (cmd! (setq indent-tabs-mode nil)
                                (doom/set-indent-width 2)))

(setq +format-on-save-enabled-modes
      '(clojurec-mode
        clojure-mode
        clojurescript-mode
        emacs-lisp-mode))

(setq +ligatures-extras-in-modes
      '(clojure-mode
        clojurescript-mode
        clojurec-mode
        emacs-lisp-mode
        org-mode))

(when (not (display-graphic-p))
  (setq debug-on-error nil))

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

(defun external-gnome-terminal ()
  (interactive "@")
  (setenv "INSIDE_EMACS" nil) ;; We don't want gnome thinking that it's inside emacs
  (shell-command (concat "gnome-terminal"
                         " --working-directory " (file-name-directory (or load-file-name buffer-file-name))
                         " > /dev/null 2>&1 & disown") nil nil))

(defun external-xfce4-terminal ()
  (interactive "@")
  (setenv "INSIDE_EMACS" nil)
  (shell-command (concat "xfce4-terminal"
                         " --working-directory " (file-name-directory (or load-file-name buffer-file-name))
                         " > /dev/null 2>&1 & disown") nil nil))

(setq external-terminal-to-open 'xfce4)
(map! :n "SPC +" (cond ((eq external-terminal-to-open 'gnome) #'external-gnome-terminal)
                       ((eq external-terminal-to-open 'xfce4) #'external-xfce4-terminal)
                       (t (message (concat "Invalid value for variable `external-terminal-to-open:' " external-terminal-to-open)))))

(setq display-line-numbers-type nil)

(defvar ++random-melpa-pkg-timer nil)
(defvar ++random-melpa-pkg-buffer "*++random-melpa-pkg-posframe-buffer*")

(defun ++show-random-melpa-pkg ()
  (interactive)
  (package-list-packages-no-fetch)
  (with-current-buffer (get-buffer "*Packages*")
    (let* ((lines-num (count-lines (point-min) (point-max)))
           (line (random (1- lines-num))))
      (prog1
          (posframe-show ++random-melpa-pkg-buffer
                         :string (buffer-substring-no-properties
                                  (line-beginning-position line)
                                  (line-end-position line))
                         :background-color "white"
                         :foreground-color "black"
                         :internal-border-width 5
                         :poshandler #'posframe-poshandler-frame-bottom-center)
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
    (setq ++random-melpa-pkg-timer nil))
  (posframe-hide ++random-melpa-pkg-buffer))
