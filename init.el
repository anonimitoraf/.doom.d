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
       ;;chinese
       ;;japanese

       :completion
       ;;(company +childframe)
                                        ; the ultimate code completion backend
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;; (ivy +fuzzy
       ;;      +prescient
       ;;      ;; +childframe
       ;;      +icons )
                                        ; a search engine for love and life
       (vertico +icons)

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;; fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       ;; indent-guides     ; highlighted indent columns
       ;; (ligatures +extra)
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ;; nav-flash         ; blink the current line after jumping
       ;; neotree           ; a project drawer, like NERDTree for vim
       ;; ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;; (pretty-code +fira)       ; ligatures or substitute text with pretty symbols
       ;; tabs              ; an tab bar for Emacs
       (treemacs +lsp)
                                        ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       ;;vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       ;; (format +onsave)            ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       ;; snippets          ; my elves. They type so I don't have to
       ;; word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +dirvish
              +icons)             ; making dired pretty [functional]
       ;;electric          ; smarter, keyword-based electric-indent
       ;;ibuffer         ; interactive buffer management
       ;;(undo)
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;; eshell            ; a consistent, cross-platform shell (WIP)
       shell             ; a terminal REPL for Emacs
       ;; term              ; terminals in Emacs
       ;; vterm             ; another terminals in Emacs

       :checkers
       (syntax +childframe) ; tasing you for every semicolon you forget
       ;; (spell +aspell)
                                        ; tasing you for misspelling mispelling
       grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;; (debugger +lsp)          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       ;;docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       gist              ; interacting with github gists
       (lookup +docsets
               +dictionary
               +offline)              ; navigate your code and its documentation
       (lsp)
       ;;macos             ; MacOS-specific commands
       (magit)             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       (terraform +lsp)         ; infrastructure as code
       tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       (tty +osc)               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;assembly          ; assembly for fun or debugging
       ;;cc                ; C/C++/Obj-C madness
       (clojure +lsp)           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       (elixir +lsp)            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       (erlang +lsp)            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp           ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       (go +lsp)                ; the hipster dialect
       ;; (graphql +lsp)      ; Give queries a REST
       ;;(gdscript +lsp)
       (haskell +lsp +ghcide)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       (java +lsp)                ; the poster child for carpal tunnel syndrome
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       (json +lsp)
       ;;julia             ; a better, faster MATLAB
       (kotlin +lsp)            ; a better, slicker Java(Script)
       (latex +lsp)             ; writing papers in Emacs has never been so fun
       ;;lean
       ;;factor
       ;;ledger            ; an accounting system in Emacs
       (lua +lsp)               ; one-based indices? one-based indices
       (markdown +grip)          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       (ocaml +lsp)             ; an objective came
       (org         ; organize your plain life in plain text
         +pretty
         +dragndrop
         +hugo
         +roam2)
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +lsp
               +pyenv
               +pyright)            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       (ruby +rails
             +lsp)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       (sh +lsp)                ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       (swift +lsp)             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web +lsp)               ; the tubes
       (yaml +lsp)

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;; calendar
       ;;irc               ; how neckbeards socialize
       (rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought
       everywhere

       :config
       ;;literate
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

;; Temporary hack to get Emacs to know where libgccjit-related libs are
(if IS-MAC
  (setenv "LIBRARY_PATH"
	  (string-join
	    '("/opt/homebrew/opt/gcc/lib/gcc/13"
	       "/opt/homebrew/opt/libgccjit/lib/gcc/13"
	       "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13")
	    ":")))
