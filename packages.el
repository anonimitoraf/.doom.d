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

;; --- Disabled packages ---
;; I want everything to have jet black bg
(package! solaire-mode :disable t)
(package! tide :disable t)
(package! evil-snipe :disable t)
(package! writegood-mode :disable t)
;; ---

;; Allow pasting stuff into org-mode
(package! org-download
  :recipe (:host github :repo "abo-abo/org-download"))

(package! dotenv-mode
  :recipe (:host github :repo "preetpalS/emacs-dotenv-mode"))

(package! alert)

(package! keychain-environment
  :recipe (:host github :repo "tarsius/keychain-environment"))

(package! symex
  :recipe (:host github :repo "countvajhula/symex.el"))

(package! tree-sitter)
(package! tree-sitter-langs)

(package! vertico-posframe)

(package! consult-projectile)

(package! exec-path-from-shell)

(package! company)
(package! corfu
  :recipe (:host github :repo "minad/corfu" :files (:defaults "extensions/*.el")))
(package! popon
  :recipe (:type git
           :repo "https://codeberg.org/akib/emacs-popon.git"))
(package! corfu-terminal
  :recipe (:type git
           :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
(package! corfu-doc-terminal
  :recipe (:type git
           :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"))
(package! cape)
(package! kind-icon)

(package! org-alert)

(package! evil-matchit)

(package! topspace
  :recipe (:host github :repo "trevorpogue/topspace"))

(package! yasnippet)

(package!  pulsar
  :recipe (:host gitlab :repo "protesilaos/pulsar"))

(package! apheleia
  :recipe (:host github :repo "raxod502/apheleia"))
(unpin! apheleia)

(package! xterm-color)

(package! org-excalidraw
  :recipe (:host github :repo "wdavew/org-excalidraw"))
(package! exercism
  :recipe (:host github :repo "anonimitoraf/exercism.el"))
(unpin! exercism)

(package! vundo)

(package! undohist)

(package! prescient
  :recipe (:host github :repo "radian-software/prescient.el"
           :files ("*.el")))

(package! olivetti
  :recipe (:host github :repo "rnkn/olivetti"))

(package! org-ros
  :recipe (:host github :repo "LionyxML/ros"))

(package! magit-popup)

(package! ov)

(package! coterm)

(package! buffer-name-relative)

(package! jinx)

(package! consult-dir)

(package! cider-storm
  :recipe (:host github :repo "jpmonettas/cider-storm" :files ("*.el")))

(package! tsx-ts-helper-mode
  :recipe (:type git :repo "https://codeberg.org/ckruse/tsx-ts-helper-mode.git"))

(package! synosaurus)

(package! org-tidy
  :recipe (:host github :repo "jxq0/org-tidy"))

(package! evil-collection)

(package! lsp-mode :pin "57cd9bf153c5ac4ef49d8833aeac77ffa7ec8ea9")

(package! all-the-icons)

(package! ranger :recipe (:host github :repo "anonimitoraf/ranger.el"))

(package! gptel :recipe (:host github :repo "karthink/gptel"))
