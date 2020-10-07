;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Rafael Nicdao"
      user-mail-address "nicdaoraf@gmail.com")

;; --- Appearance -----------------------------------------------------------------

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-dark-custom)
(setq doom-theme 'doom-dark-custom)

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
(setq doom-font (font-spec :family "Ubuntu Mono" :size 15))

;; --------------------------------------------------------------------------------

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org")

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
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))


;; --- Evil stuff ---------------------------------------------------

;; Disable the annoying auto-comment on newline
(setq +evil-want-o/O-to-continue-comments nil)

;; Remove some conflicting keybindings with company-mode
(define-key global-map (kbd "C-j") nil)
(define-key global-map (kbd "C-k") nil)
(define-key global-map (kbd "TAB") nil)

(define-key evil-insert-state-map (kbd "C-j") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-motion-state-map (kbd "<tab>") nil)

(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "C-S-o") 'evil-jump-forward)
;; Perstent folding
(use-package vimish-fold
  :ensure
  :after evil)

(use-package evil-vimish-fold
  :ensure t
  :after vimish-fold
  :init
  (setq evil-vimish-fold-mode-lighter " ⮒")
  (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  :config
  (global-evil-vimish-fold-mode))

;; -------------------------------------------------------------------

;; Modeline
(after! doom-modeline
  (setq
   doom-modeline-height 20
   doom-modeline-major-mode-icon t
   doom-modeline-major-mode-color-icon t
   doom-modeline-buffer-state-icon
   doom-modeline-buffer-modification-icon
   doom-modeline-lsp t))


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

;; Live markdown preview
(custom-set-variables
 '(livedown-autostart nil) ; automatically open preview when opening markdown files
 '(livedown-open t)        ; automatically open the browser window
 '(livedown-port 1337)     ; port for livedown server
 '(livedown-browser "firefox"))  ; browser to use

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

(after! org
  (setq org-todo-keywords '((sequence "TODO(t)" "START(s)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(c)")
                            (sequence "[ ](T)" "[-](S)" "[?](H)" "|" "[X](D)"))
        org-log-done 'time
        org-hide-leading-stars t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-superstar-headline-bullets-list '("▪")
        org-superstar-cycle-headline-bullets 1
        org-superstar-todo-bullet-alist '("▪")
        ))

;; (defun set-org-font ()
;;   "Sets the font of an org-mode buffer"
;;   (interactive)
;;   (setq buffer-face-mode-face '(:family "Ubuntu Mono" :height 120))
;;   (buffer-face-mode))
;; (add-hook 'org-mode-hook 'set-org-font)

(require 'ob-clojure)
(require 'cider)
(setq org-babel-clojure-backend 'cider)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (Clojure . t)
   (Javascript . t)))

;; --- LSP stuff --------------------------------------------

;; Complements `find-defintions' (which is `g d')
(define-key evil-normal-state-map (kbd "g f") 'lsp-ui-peek-find-references)

(add-hook 'lsp-ui-peek-mode-hook
          (lambda ()
            (define-key lsp-ui-peek-mode-map (kbd "j") 'lsp-ui-peek--select-next)
            (define-key lsp-ui-peek-mode-map (kbd "k") 'lsp-ui-peek--select-prev)
            (define-key lsp-ui-peek-mode-map (kbd "C-k") 'lsp-ui-peek--select-prev-file)
            (define-key lsp-ui-peek-mode-map (kbd "C-j") 'lsp-ui-peek--select-next-file)))

;; --- Clojure stuff --------------------------------------------

(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  ;; Optional: In case `clojure-lsp` is not in your PATH
  (setq lsp-clojure-custom-server-command '("bash" "-c" "/home/anonimito/.doom.d/misc/clojure-lsp")
        lsp-enable-indentation nil
        lsp-log-io t))

;; --- E-shell stuff ---------------------------------------------------
;; Company mode in eshell makes it lag
(add-hook 'eshell-mode-hook (lambda () (company-mode -1)))

;; --- Company stuff ---------------------------------------------------

(set-company-backend! 'clojurescript-mode
  'company-capf 'company-dabbrev-code 'company-dabbrev)

;; --- Treemacs stuff ---------------------------------------------------

(add-hook 'treemacs-mode-hook
          (lambda () (text-scale-decrease 1.5)))

(use-package treemacs
  :commands (treemacs
             treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode)
  :bind (("<f8>" . treemacs)
         ("<f9>" . treemacs-select-window))
  :init
  (when window-system
    (setq treemacs-width 30
          treemacs-is-never-other-window t
          treemacs--width-is-locked nil
          treemacs-space-between-root-nodes nil)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode nil)
    (treemacs)
    (other-window 1)))

;; --- Misc ---------------------------------------------------

;; Clock on modeline
(display-time-mode 1)

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

;; TODO Re-do the changes that were lost
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(livedown-autostart nil)
;;  '(livedown-browser "firefox")
;;  '(livedown-open t)
;;  '(livedown-port 1337)
;;  '(org-image-actual-width nil)
;;  '(org-outline-path-complete-in-steps nil)
;;  '(org-refile-use-outline-path 'file)
;;  '(org-todo-keywords
;;    '((sequence "TODO(t)" "START(s)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(c)")
;;      (sequence "[ ](T)" "[-](S)" "[?](H)" "|" "[X](D)"))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-block-begin-line ((t (:extend t :background "black" :foreground "#875F5F"))))
;;  '(org-code ((t (:foreground "#870000"))))
;;  '(org-document-title ((t (:foreground "#D75F00" :weight normal))))
;;  '(org-list-dt ((t (:foreground "#870000"))))
;;  '(org-special-keyword ((t (:foreground "#A8A8A8"))))
;;  '(org-table ((t (:foreground "peach puff"))))
;;  '(outline-1 ((t (:extend t :foreground "#ffd7af" :weight normal))))
;;  '(outline-2 ((t (:extend t :foreground "#FFAF5F" :weight normal))))
;;  '(outline-3 ((t (:extend t :foreground "#FFD75F" :weight normal))))
;;  '(outline-4 ((t (:extend t :foreground "#D75F00" :weight normal))))
;;  '(outline-5 ((t (:extend t :foreground "#5F87AF" :weight normal))))
;;  '(outline-6 ((t (:extend t :foreground "#a8a8a8" :weight normal))))
;;  '(outline-7 ((t (:extend t :foreground "#86c2a2" :weight normal))))
;;  '(outline-8 ((t (:inherit outline-4 :extend t :foreground "#818b35" :weight normal))))
;;  '(show-paren-match ((t (:foreground nil :background "#333" :weight normal)))))
