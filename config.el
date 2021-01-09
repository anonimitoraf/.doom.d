;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(defun kb (bytes) (* bytes 1024))
(defun mb (bytes) (* (kb bytes) 1024))

(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Rafael Nicdao"
      user-mail-address "nicdaoraf@gmail.com")

;; --- Appearance -----------------------------------------------------------------

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

(use-package doom-themes
  :config
  ;; Use the colorful treemacs theme
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  ;; Custom style tweaks
  ;; See https://github.com/hlissner/emacs-doom-themes/blob/master/themes/doom-one-theme.el#L32
  ;; for the doom-colors
  (let ((vscode-search-occ-bg "#4d1e00")
        (vscode-search-occ-fg "#cccccc"))
    (custom-set-faces!
     `(default :background "black")
     `(fill-column-indicator :foreground ,(doom-color 'base1))
     `(window-divider :foreground ,(doom-color 'magenta))
     `(flycheck-posframe-error-face :background "firebrick"
                                    :foreground "white")
     `(flycheck-posframe-warning-face :background "dark goldenrod"
                                      :foreground "white")
     `(swiper-background-match-face-2 :background ,vscode-search-occ-bg
                                      :foreground ,vscode-search-occ-fg)
     `(swiper-match-face-2 :background ,vscode-search-occ-bg
                           :foreground ,vscode-search-occ-fg)
     `(swiper-line-face :background "DodgerBlue4"
                        :foreground ,vscode-search-occ-fg))))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Ubuntu Mono" :size (let ((screen-size (getenv "EMACS_SCREEN_SIZE")))
                                                         (cond ((equal screen-size "small") 15)
                                                               ((equal screen-size "medium") 16)
                                                               ((equal screen-size "large") 17)
                                                               (t 17)))))
;; (setq doom-variable-pitch-font (font-spec :family "Roboto Mono Light" :size 14))

;; Enable rainbow-mode to visualize hex strings
(defun enable-rainbow-mode ()
  (rainbow-mode +1))
(add-hook 'text-mode-hook #'enable-rainbow-mode)
(add-hook 'prog-mode-hook #'enable-rainbow-mode)

;; Prevent hl-line-mode from overriding rainbow-mode
(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

;; Doom modeline
(setq display-time-default-load-average nil
      display-time-24hr-format t)

;; Header line
;; (defun set-header-line-format ()
;;   (after! doom-modeline
;;     (setq header-line-format (with-face (doom-modeline-buffer-file-name)
;;                                         :box '(:line-width 5
;;                                                ;; HACK I got this colour via using a colour-picker
;;                                                :color "#282c34")))))
;; (add-hook 'text-mode-hook #'set-header-line-format)
;; (add-hook 'prog-mode-hook #'set-header-line-format)

;; --------------------------------------------------------------------------------

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Disable highlighting of current line
(add-hook 'hl-line-mode-hook (lambda () (setq hl-line-mode nil)))

;; Structural editing
(use-package! evil-lisp-state
  :init (setq evil-lisp-state-global t)
  :config (evil-lisp-state-leader "SPC k"))

;; Company configuration
(after! company
  (setq company-idle-delay 0.0
        company-tooltip-idle-delay 0.2
        company-minimum-prefix-length 1)
  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-mode-map (kbd "C-SPC") 'company-manual-begin))

;; --- Evil stuff ---------------------------------------------------

;; Disable the annoying auto-comment on newline
(setq +evil-want-o/O-to-continue-comments nil)

;; Remove some conflicting keybindings with company-mode
(define-key global-map (kbd "C-j") nil)
(define-key global-map (kbd "C-k") nil)
;; (define-key global-map (kbd "TAB") nil)

(define-key evil-insert-state-map (kbd "C-j") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-motion-state-map (kbd "<tab>") nil)

(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "C-S-o") 'evil-jump-forward)

;; -------------------------------------------------------------------

;; Modeline
(after! doom-modeline
  (custom-set-faces!
    '(mode-line :height 0.9 :width condensed)
    '(mode-line-inactive :height 0.9 :width condensed)
    '(mode-line-emphasis :inherit mode-line))
  (setq
   doom-modeline-buffer-file-name-style nil
   doom-modeline-height 0
   doom-modeline-major-mode-icon t
   doom-modeline-major-mode-color-icon t
   doom-modeline-buffer-modification-icon t
   doom-modeline-modal-icon nil
   doom-modeline-buffer-state-icon nil
   doom-modeline-enable-word-count nil
   doom-modeline-lsp nil))

;; Centaur Tabs configuration
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

;; Lookup to not open browser
(setq +lookup-open-url-fn #'eww)

;; Highlight whole expression, not just the matching paren
(setq show-paren-style 'expression)
(custom-set-faces
 '(show-paren-match ((t (:foreground nil
                         :background "#333"
                         :weight normal)))))

;; org2blog
;; (require 'auth-source)
;; (let* ((credentials (auth-source-user-and-password "blog"))
;;        (username (nth 0 credentials))
;;        (password (nth 1 credentials))
;;        (config `("wordpress"
;;                  :url "http:///anonimitocom.wordpress.com/xmlrpc.php"
;;                  :username ,username
;;                  :password ,password)))
;;   (setq org2blog/wp-blog-alist config))
;; ;; org2blog
(setq org2blog/wp-blog-alist
      '(("blog"
          :url "http://anonimitocom.wordpress.com/xmlrpc.php"
          :username "anonimitoraf")))

;; --- Org-mode stuff ---

(setq org-clock-mode-line-total 'current)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(use-package org
  :init
  (setq org-directory "~/Dropbox/org"
        org-agenda-files (directory-files-recursively org-directory "\\.org$")
        org-default-notes-file "~/Dropbox/org/notes/default.org"))

(after! org
  (setq org-todo-keywords '((sequence "TODO(t)" "START(s)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(c)")
                            (sequence "[ ](T)" "[-](S)" "[?](H)" "|" "[X](D)"))
        org-log-done 'time
        org-hide-leading-stars t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-superstar-headline-bullets-list '("▪")
        org-superstar-cycle-headline-bullets 1
        org-superstar-todo-bullet-alist '("▪")))

;; Org by default uses the `default' face directly
;; Well, I don't want it to, hence this remapping
(custom-set-faces! '(org-default :font "Roboto Mono Light"))
(add-hook 'org-mode-hook (cmd! (setq display-line-numbers nil) ;; Line numbers are not needed and are just distracting
                               (face-remap-add-relative 'default '(org-default))))
;; Hacky, but seems to work. Basically, I use zen-mode/writeroom-mode for org.
;; It irks me that for some reason, the font remapping doesn't work, so I
;; explicitly re-apply the font remappint
(add-hook 'writeroom-mode-hook (cmd! (when (= major-mode "org-mode")
                                       (face-remap-add-relative 'default '(org-default)))))

(setq org-tags-column -120)

;; --- org-download (Allows pasting stuff into org-mode)
(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(setq org-download-method 'attach)
(setq org-image-actual-width nil)

;; --- Recur

;; See https://github.com/m-cat/org-recur
(use-package org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :demand t
  :config
  (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)

  ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
  (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
  (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)

  (setq org-recur-finish-done t
        org-recur-finish-archive t))

;; Refresh org-agenda after rescheduling a task.
(defun org-agenda-refresh ()
  "Refresh all `org-agenda' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-maybe-redo)))))

(defadvice org-schedule (after refresh-agenda activate)
  "Refresh org-agenda."
  (org-agenda-refresh))

;; Log time a task was set to Done.
(setq org-log-done (quote time))

;; Don't log the time a task was rescheduled or redeadlined.
(setq org-log-redeadline nil)
(setq org-log-reschedule nil)

;; Prefer rescheduling to future dates and times
(setq org-read-date-prefer-future 'time)

;; --- Habit

(require 'org-habit)
(setq org-habit-show-habits-only-for-today nil)

;; --- Agenda

(setq org-agenda-window-setup 'other-window)

;; --- Babel

;; NodeJS setup
(setenv "NODE_PATH"
        (concat
         (getenv "HOME") "/org/node_modules"  ":"
         (getenv "NODE_PATH")))

;; Clojure setup
(require 'ob-clojure)
(require 'cider)
(setq org-babel-clojure-backend 'cider)

(require 'ob-sql)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (Clojure . t)
   (Javascript . t)
   (sql . t)))

;; --- Capture

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
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

;; --- LSP stuff --------------------------------------------

;; Complements `find-defintions' (which is `g d')
(define-key evil-normal-state-map (kbd "g f") 'lsp-ui-peek-find-references)

(use-package! all-the-icons
    :config (setq all-the-icons-scale-factor 0.90))

(after! lsp-mode
  (custom-set-faces!
    '(header-line :height 0.90))
  (setq lsp-lens-enable t
        lsp-log-io nil
        lsp-headerline-breadcrumb-enable t))

(after! lsp-ui
  (define-key lsp-ui-peek-mode-map (kbd "j") 'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "k") 'lsp-ui-peek--select-prev)
  (define-key lsp-ui-peek-mode-map (kbd "C-k") 'lsp-ui-peek--select-prev-file)
  (define-key lsp-ui-peek-mode-map (kbd "C-j") 'lsp-ui-peek--select-next-file)

  (map! :map lsp-mode-map
        :nv "SPC c m" #'lsp-ui-imenu)

  (setq lsp-ui-peek-fontify 'always
        lsp-ui-peek-list-width 50
        lsp-ui-peek-peek-height 40

        lsp-ui-doc-enable t
        ;; Prevents LSP peek to disappear when mouse touches it
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-delay 0.1
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-max-width 120
        lsp-ui-doc-max-height 120

        lsp-ui-imenu-enable t

        ;; This is just annoying, really
        lsp-ui-sideline-enable nil)

  ;; Clojure/script stuff to ignore
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.clj-kondo$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.shadow-cljs$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]resources$"))

;; Automatically refresh LSP imenu when changing windows
;; (add-hook 'window-state-change-hook (cmd! (when (bound-and-true-p lsp-ui-mode)
;;                                             (let ((curr-window (selected-window)))
;;                                               (lsp-ui-imenu)
;;                                               ;; Otherwise we're stuck in an endless
;;                                               ;; loop of being in the imenu
;;                                               (select-window curr-window)))))

(setq read-process-output-max (mb 1))

;; --- Clojure stuff --------------------------------------------

;; See issue with sayid and nativecomp: https://github.com/clojure-emacs/sayid/pull/59
;; (eval-after-load 'clojure-mode
;;   '(sayid-setup-package))

;; Dash docsets
(add-to-list 'dash-docs-docsets "Clojure")

;; Highlight particular macros similar to built-in stuff
;; For example, highlight ghostwheel's `>defn' similar
;; the same way as built-in `defn'
(add-hook 'clojure-mode-hook
  '(lambda ()
    ;; Set some new syntax-highlighting rules.
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
            (2 font-lock-function-name-face))))))

;; --- (Type|Java)script stuff ---------------------------------------------------

(setq typescript-indent-level 2)
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

(require 'gherkin-mode)
(add-to-list 'auto-mode-alist '("\\.feature\\'" . gherkin-mode))

;; --- eshell stuff ---------------------------------------------------

;; Company mode in eshell makes it lag
(add-hook 'eshell-mode-hook (lambda () (company-mode -1)))

;; --- shell stuff ---------------------------------------------------

;; Company mode in shell is just annoying
(add-hook 'shell-mode-hook (lambda () (company-mode -1)))

;; --- Company stuff ---------------------------------------------------

;; (set-company-backend! 'clojurescript-mode
;;   'company-capf 'company-dabbrev-code 'company-dabbrev)

;; --- Treemacs stuff ---------------------------------------------------

(add-hook 'treemacs-mode-hook
          (lambda () (text-scale-decrease 1.5)))

(with-eval-after-load 'treemacs-icons (treemacs-resize-icons 10))

(use-package treemacs
  :commands (treemacs)
  :bind (("<f8>" . treemacs)
         ("<f9>" . treemacs-select-window))
  :init
  (when window-system
    (setq treemacs-width 30
          treemacs-is-never-other-window t
          treemacs-file-event-delay 1000
          treemacs-show-cursor t
          treemacs--width-is-locked nil
          treemacs-space-between-root-nodes nil
          treemacs-filewatch-mode t
          treemacs-fringe-indicator-mode t)))

;; --- Ivy ---------------------------------------------------

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

;; Enable search preview by default, for ivy-based commands
(setq-default ivy-calling nil)

;; --- Emacs-Anywhere ----------------------------------------

(defun popup-handler (app-name window-title x y w h)
  (set-frame-position (selected-frame) x (+ y (- h 400)))
  (unless (zerop w)
    (set-frame-size (selected-frame) w 400 t)))
(add-hook 'ea-popup-hook 'popup-handler)

;; --- Tramp ---------------------------------------------------

;; (require 'tramp)
;; (setq tramp-default-method "ssh")
;; (setq tramp-syntax 'simplified)

;; --- Misc ---------------------------------------------------

(defun bespoke/load-and-continuously-save (file)
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
  (when (boundp 'bespoke-continuous-saving-timer)
    (cancel-timer bespoke-continuous-saving-timer))
  ;; Save the session every 10 seconds
  (setq bespoke-continuous-saving-timer
        (run-with-timer 1 10 (cmd!
                              (message "Saving '%s' session" file)
                              (doom-save-session file)))))
(map! :map doom-leader-map "q N" 'bespoke/load-and-continuously-save)

;; Disable *Messages* from popping up when minibuffer is clicked
(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

;; Clock on modeline
(display-time-mode +1)

(require 'focus-autosave-mode)
(focus-autosave-mode +1)

(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice evil-window-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice evil-window-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice evil-window-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice evil-window-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; TODO: Maybe each mode has to be different
(global-set-key "\t" (lambda () (interactive) (insert-char 32 2))) ;; [tab] inserts two spaces
(add-hook 'prog-mode-hook (cmd! (doom/set-indent-width 2)))

(require 'explain-pause-mode)
(explain-pause-mode +1)

;; (Seemingly) Auto-focus newly-created window
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Fix the ispell dict
(setq ispell-dictionary "en")

;; Smooth scrolling through org-images, etc
(require 'iscroll)
(iscroll-mode +1)

;; Emacs as a WM
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-default)

;; Flex on Discord that we're using Emacs
(require 'elcord)
(elcord-mode)

;; See https://github.com/hlissner/doom-emacs/issues/3038
(after! counsel
  (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s || true"))

;; --- Godot ---

(require 'gdscript-mode)

;; --- Custom scripts/commands ---

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

;; --- Temporary stuff ---

;; Uncomment these for testing out Clojure LSP changes
;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((clojure-mode . lsp)
;;          (clojurec-mode . lsp)
;;          (clojurescript-mode . lsp))
;;   :config
;;   ;; add paths to your local installation of project mgmt tools, like lein
;;   (setenv "PATH" (concat
;;                    "/usr/local/bin" path-separator
;;                    (getenv "PATH")))
;;   (dolist (m '(clojure-mode
;;                clojurec-mode
;;                clojurescript-mode
;;                clojurex-mode))
;;      (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
;;   (setq lsp-clojure-server-command '("bash"
;;                                      "-c"
;;                                      "/home/anonimito/work/open-source/clojure/clojure-lsp/target/clojure-lsp") ;; Optional: In case `clojure-lsp` is not in your PATH
;;         lsp-enable-indentation nil))



;; ---------------------------------
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
