;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
			 ("tromey" . "http://tromey.com/elpa/")))

;;(add-to-list 'package-archives
;;	     '("tromey" . "http://tromey.com/elpa/") t)
;;(add-to-list 'package-archives
;;	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives
;;	     '("gnu" . "http://elpa.gnu.org/packages/") t)

;;(add-to-list 'package-archives
;;             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Setting package-enable-at-startup to nil prevents a second package load
;; and slightly improves startup time.
(setq package-enable-at-startup nil)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; If use-package is not installed, install it.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; This stops emacs adding customised settings to init.el. I try to avoid
;; using customize anyway, preferring programmatic control of variables.
;; Creating it as a temporary file effectively disables it
;; (i.e. any changes are session local).
(setq custom-file (make-temp-file "emacs-custom"))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; aggressive-indent-mode is a minor mode that keeps your code always indented.
    ;; It reindents after every change, making it more reliable than electric-indent-mode.
    aggressive-indent

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; Markdown-mode
    markdown-mode

    ;; Yaml-mode
    yaml-mode

    ;; support to TOML files
    toml-mode

    ;; SCSS mode
    scss-mode

    ;; for editing dockerfiles
    dockerfile-mode

    ;; for managing docker itself
    docker

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-completing-read+

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; show a menu with options for fulfilling key bindings
    guide-key

    ;; auto-complete
    company

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; beautiful monokai theme
    monokai-theme

    ;; cyberpunk theme is a darker mood theme I like
    cyberpunk-theme

    ;; the much loved smooth scrolling
    smooth-scrolling

    ;; wakatime collects usage stats and generates a nice dashboard online
    wakatime-mode

    ;; flycheck allows on-the-fly syntax checking
    flycheck

    ;; for Rust we need cargo (which will also install rust-mode)
    cargo

    ;; flycheck support for Rust
    flycheck-rust

    ;; racer is the gives auto-completion capabilities to Rust
    ;; According to https://github.com/racer-rust/emacs-racer you also need to
    ;; install racer on your system like this:
    ;; $ rustup component add rust-src
    ;; $ cargo install racer
    racer

    ;; restclient-mode is a great tool for debugging APIs
    restclient

    ;; allows setting up search engines onto emacs
    engine-mode

    ;; web-server to server files straight from emacs
    web-server))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Makes sure that wakatime is used for all buffers
(global-wakatime-mode)
;; My personal wakatime API key
(setq wakatime-api-key "e7876e69-1501-4799-93aa-4b6da67d5508")
;; Wakatime's binary needs to be installed seperately
(setq wakatime-cli-path "/run/current-system/sw/bin/wakatime-cli")


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables and other many nice things.
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Mostly bindings for git
(load "setup-git.el")

;; Flycheck for on-the-fly syntax checking
(load "setup-flycheck.el")

;; Serving files as a web server (very useful)
(load "setup-serve-this.el")

;; Langauage-specific settings
(load "setup-elisp.el")
(load "setup-clojure.el")
(load "setup-markdown.el")
(load "setup-js.el")
(load "setup-html.el")
(load "setup-umlaut.el")
(load "setup-rust.el")
(load "setup-extras.el")

(load "~/.finda/integrations/emacs/finda.el")

;; Start Emacs in server mode so that emacsclient can connect to it
(server-start)




;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:hide-gutter t)
 '(git-gutter:modified-sign "*")
 '(git-gutter:update-interval 2)
 '(package-selected-packages
   (quote
    (fill-column-indicator flycheck-rust flycheck cargo company-lua lua-mode transpose-frame git-gutter magit tagedit rainbow-delimiters projectile smex ido-ubiquitous scss-mode yaml-mode markdown-mode cider clojure-mode-extra-font-locking clojure-mode paredit exec-path-from-shell)))
 '(safe-local-variable-values
   (quote
    ((cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
