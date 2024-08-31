;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

;; Remove title bar. See https://github.com/d12frosted/homebrew-emacs-plus#emacs-29-1
(when (string> emacs-version "29")
  (add-to-list 'default-frame-alist '(undecorated . t)))

(doom! :input

       :completion
       (vertico +icons)

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline          ; snazzy, Atom-inspired modeline, plus API
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding

       :emacs
       (dired +ranger
              +icons)             ; making dired pretty [functional]
       vc                ; version-control and Emacs, sitting in a tree

       :term
       shell             ; a terminal REPL for Emacs

       :checkers
       (syntax +childframe) ; tasing you for every semicolon you forget

       :tools
       editorconfig      ; let someone else argue about tabs vs spaces
       (eval +overlay)     ; run code, run (also, repls)
       (lookup +docsets
               +dictionary
               +offline)              ; navigate your code and its documentation
       (lsp)
       (magit)             ; a git porcelain for Emacs
       pdf               ; pdf enhancements
       (terraform +lsp)         ; infrastructure as code
       tmux              ; an API for interacting with tmux

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       (tty +osc)               ; improve the terminal Emacs experience

       :lang
       (clojure +lsp)           ; java with a lisp
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       (erlang +lsp)
       (gdscript +lsp)
       (java +lsp)                ; the poster child for carpal tunnel syndrome
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       (json +lsp)
       (lua +lsp)               ; one-based indices? one-based indices
       (markdown +grip)          ; writing docs for people to ignore
       (org         ; organize your plain life in plain text
        +pretty
        +dragndrop
        +hugo
        +roam2)
       (python +lsp
               +pyenv
               +pyright)            ; beautiful is better than ugly
       (sh +lsp)                ; she sells {ba,z,fi}sh shells on the C xor
       (web +lsp)               ; the tubes
       (yaml +lsp)

       :email

       :app

       :config
       (default +bindings +smartparens))

(defun ++org-tangle-with-include-files ()
  (interactive)
  (save-window-excursion
    (with-current-buffer (find-file-noselect (concat doom-private-dir "config.org"))
      (org-org-export-as-org)
      (let ((tmp (make-temp-file "literate-config-bundle-" nil ".org")))
        (with-temp-file tmp
          (insert (with-current-buffer "*Org ORG Export*" (buffer-string))))
        (org-babel-tangle-file tmp)))))

;; Temporary hack to get Emacs to know where libgccjit-related libs (and other stuff) are
(if IS-MAC
    (setenv "LIBRARY_PATH"
	    (string-join
	     '("/opt/homebrew/opt/gcc/lib/gcc/13"
	       "/opt/homebrew/opt/libgccjit/lib/gcc/13"
	       "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13"
               "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib")
	     ":")))
