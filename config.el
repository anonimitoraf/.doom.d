;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Rafael Nicdao"
      user-mail-address "nicdaoraf@gmail.com")

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
(setq doom-font (font-spec :family "Hack" :size 14))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (load-theme 'doom-dark+ t)
(setq doom-theme 'doom-dark+)
(setq doom-dark+-blue-modeline t)
(custom-set-faces
  '(default ((t (:background "#000000")))))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Disable highlighting of current line
(add-hook 'hl-line-mode-hook
  (lambda ()
    (setq hl-line-mode nil)))

;; Structural editing
(use-package! evil-lisp-state
  :init (setq evil-lisp-state-global t)
  :config (evil-lisp-state-leader "SPC k"))

;; Company configuration
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2)
  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort))
(define-key global-map (kbd "C-j") nil)
(define-key global-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-j") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)

;; Centaur Tabs configuration
(after! centaur-tabs
   (setq centaur-tabs-style "rounded"
    centaur-tabs-height 25
    centaur-tabs-set-icons t
    centaur-tabs-set-modified-marker t
    centaur-tabs-show-navigation-buttons t
    centaur-tabs-gray-out-icons 'buffer)
   (centaur-tabs-headline-match)
   ;; (centaur-tabs-enable-buffer-reordering)
   ;; (setq centaur-tabs-adjust-buffer-order t)
   (centaur-tabs-mode t))

;; Lookup to not open browser
(setq +lookup-open-url-fn #'eww)

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
