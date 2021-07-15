;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; feature/evil
(package! evil-lisp-state
  :recipe (:host github :repo "syl20bnr/evil-lisp-state"))

;; live preview of markdown files
(package! livedown
  :recipe (:host github :repo "shime/emacs-livedown"))

;; Blogging
(package! org2blog
  :recipe (:host github :repo "org2blog/org2blog"))

(package! ripgrep)
(package! ag)

(package! explain-pause-mode
  :recipe (:host github :repo "lastquestion/explain-pause-mode"))

(package! elcord
  :recipe (:host github :repo "Mstrodl/elcord"))

(package! gherkin-mode
  :recipe (:host github :repo "candera/gherkin-mode"))

(package! org-recur
  :recipe (:host github :repo "m-cat/org-recur"))

(package! exwm)
;; (package! exwm-config)
(package! xelb)

(package! iscroll
  :recipe (:host github :repo "casouri/iscroll"))

;; (package! sayid
;;   :recipe (:host github :repo "clojure-emacs/sayid"))

(package! emacs-anywhere
  :recipe (:host github :repo "zachcurry/emacs-anywhere"))

;; Allow pasting stuff into org-mode
(package! org-download
  :recipe (:host github :repo "abo-abo/org-download"))

(package! gdscript-mode
  :recipe (:host github
           :repo "GDQuest/emacs-gdscript-mode"))

;; I want everything to have jet black bg
(package! solaire-mode :disable t)

(package! kubernetes-el
  :recipe (:host github :repo "chrisbarrett/kubernetes-el"))

(unpin! org-mode)

(package! org-mime
  :recipe (:host github :repo "org-mime/org-mime"))

;; (package! webkit
;;   :recipe (:type git :host github :repo "akirakyle/emacs-webkit"
;;            :branch "main"
;;            :files (:defaults "*.js" "*.css" "*.so")
;;            :pre-build ("make")))

(package! edbi
  :recipe (:host github :repo "kiwanami/emacs-edbi"))

(package! helm
  :recipe (:host github :repo "emacs-helm/helm"))

(package! selectrum
  :recipe (:host github :repo "raxod502/selectrum"))

(package! erefactor
  :recipe (:host github :repo "mhayashi1120/Emacs-erefactor"))

(package! smooth-scrolling)

(unpin! flycheck-posframe)

(package! clojure-rand-ref
  :recipe (:host github :repo "anonimitoraf/clojure-rand-ref.el"))

(package! dotenv-mode
  :recipe (:host github :repo "preetpalS/emacs-dotenv-mode"))

(package! benchmark-init
  :recipe (:host github :repo "kekeimiku/benchmark-init-el"))

(unpin! lsp-mode)

(package! idle-highlight-mode
  :recipe (:host github :repo "nonsequitur/idle-highlight-mode"))
