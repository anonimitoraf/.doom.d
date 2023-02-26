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

;; live preview of markdown files
;; (package! livedown
;;   :recipe (:host github :repo "shime/emacs-livedown"))

;; (package! ripgrep)
;; (package! ag)

;; (package! explain-pause-mode
;;   :recipe (:host github :repo "lastquestion/explain-pause-mode"))

;; (package! elcord
;;   :recipe (:host github :repo "Mstrodl/elcord"))

(package! company)

(package! gherkin-mode
  :recipe (:host github :repo "candera/gherkin-mode"))

;; (package! emacs-anywhere
;;   :recipe (:host github :repo "zachcurry/emacs-anywhere"))

;; Allow pasting stuff into org-mode
(package! org-download
  :recipe (:host github :repo "abo-abo/org-download"))

;; I want everything to have jet black bg
(package! solaire-mode :disable t)

;; (package! kubernetes-el
;;   :recipe (:host github :repo "chrisbarrett/kubernetes-el"))

;; (unpin! org)

;; (package! org-mime
;;   :recipe (:host github :repo "org-mime/org-mime"))

;; (package! erefactor
;;   :recipe (:host github :repo "mhayashi1120/Emacs-erefactor"))

;; (package! clojure-rand-ref
;;   :recipe (:host github :repo "anonimitoraf/clojure-rand-ref.el"))

(package! dotenv-mode
  :recipe (:host github :repo "preetpalS/emacs-dotenv-mode"))

(package! benchmark-init
  :recipe (:host github :repo "kekeimiku/benchmark-init-el"))

(unpin! lsp-mode)

;; (package! idle-highlight-mode
;;   :recipe (:host github :repo "nonsequitur/idle-highlight-mode"))

(package! alert)

;; (package! ejc-sql)

(package! keychain-environment
  :recipe (:host github :repo "tarsius/keychain-environment"))

;; (package! logview)
(package! itail)

;; (package! chrome
;;   :recipe (:host github :repo "anticomputer/chrome.el"))

(package! i3wm-config-mode
  :recipe (:host github :repo "Alexander-Miller/i3wm-Config-Mode"))

;; (package! nyan-mode
;;   :recipe (:host github :repo "anonimitoraf/nyan-mode"))

;; (package! clomacs)

;; (package! vue-mode)

;; (package! screenshot
;;   :recipe (:host github :repo "tecosaur/screenshot"))

;; (package! slime)

;; (package! projectile-git-autofetch)

;; (package! try
;;   :recipe (:host github :repo "larstvei/Try"))

(package! symex
  :recipe (:host github :repo "countvajhula/symex.el"))

(package! gitconfig-mode
  :recipe (:host github :repo "magit/git-modes"
	   :files ("gitconfig-mode.el")))

(package! gitignore-mode
  :recipe (:host github :repo "magit/git-modes"
	   :files ("gitignore-mode.el")))

(package! thread-dump
  :recipe (:host github :repo "anonimitoraf/thread-dump.el"))

(package! speed-dial
  :recipe (:host github :repo "anonimitoraf/speed-dial.el"))

;; (package! company-quickhelp
;;   :recipe (:host github :repo "anonimitoraf/company-quickhelp"))

(package! xclip)

;; (package! icons-in-terminal
;;   :recipe (:host github :repo "seagle0128/icons-in-terminal.el"))

(package! tree-sitter)
(package! tree-sitter-langs)

(package! speed-type)

(package! tide :disable t)

;; (package! origami
;;   :recipe (:host github :repo "elp-revive/origami.el"))

(package! google-translate)

(package! vertico-posframe)

(package! consult-projectile)

(package! exec-path-from-shell)

(package! company)
(package! corfu)
(package! corfu-doc
  :recipe (:host github :repo "galeo/corfu-doc"))
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

(package! org-modern
  :recipe (:host github :repo "minad/org-modern"))

;; (package! focus)

(package! org-alert)

(package! emacs-async)

(package! svg-tag-mode
  :recipe (:host github :repo "rougier/svg-tag-mode"))

(package! clippo
  :recipe (:host github :repo "anonimitoraf/emacs-clippo"))

(package! hackernews
  :recipe (:host github :repo "clarete/hackernews.el"))

;; (package! prettier)

(package! evil-matchit)

(package! keycast
  :recipe (:host github :repo "tarsius/keycast"))

(package! org-sticky-header)

(package! evil-snipe :disable t)

(package! topspace
  :recipe (:host github :repo "trevorpogue/topspace"))

(package! yasnippet)

(package! writegood-mode :disable t)

(package!  pulsar
  :recipe (:host gitlab :repo "protesilaos/pulsar"))

(package! apheleia
  :recipe (:host github :repo "raxod502/apheleia"))
(unpin! apheleia)

(package! detached)

(package! xterm-color)

(package! org-excalidraw
  :recipe (:host github :repo "wdavew/org-excalidraw"))

(package! persist)
(package! promise)
(package! async-await)

(package! exercism
  :recipe (:host github :repo "anonimitoraf/exercism.el"))
(unpin! exercism)

(package! vundo)

(package! sidecar-locals)

(package! el-easydraw
  :recipe (:host github :repo "misohena/el-easydraw"
           :files ("*.el")))

(package! auto-dim-other-buffers
  :recipe (:host github :repo "mina86/auto-dim-other-buffers.el"))

(package! undohist)

(package! org-remark
  :recipe (:host github :repo "nobiot/org-remark"))

(package! dwim-shell-command
  :recipe (:host github :repo "xenodium/dwim-shell-command"))

(package! prescient
  :recipe (:host github :repo "radian-software/prescient.el"
           :files ("*.el")))

(unpin! magit)
(unpin! forge)

(package! chatgpt
  :recipe (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el")))

(package! olivetti
  :recipe (:host github :repo "rnkn/olivetti"))

(package! sticky-shell
  :recipe (:host github :repo "andyjda/sticky-shell"))

(package! org-ros
  :recipe (:host github :repo "LionyxML/ros"))

(package! magit-filenotify)
