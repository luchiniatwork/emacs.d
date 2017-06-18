;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; projectile everywhere!
(projectile-global-mode)

;; clean-up whitespace before saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Navigation between window numbers
(require 'window-number)
(window-number-mode 1)

;; Splits into 3 windows horizontally balanced
(defun split-3-windows-horizontally-evenly ()
  (interactive)
  (command-execute 'delete-other-windows)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows))

;; Bind split 3 way
(global-set-key (kbd "C-x 4") 'split-3-windows-horizontally-evenly)

;; Opposite of the traditional C-x o (other-window)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

;; Bind to the traditional C-x o that would normally be on `other-window`
(global-set-key (kbd "C-c 1") (lambda () (interactive) (window-number-select 1)))
(global-set-key (kbd "C-c 2") (lambda () (interactive) (window-number-select 2)))
(global-set-key (kbd "C-c 3") (lambda () (interactive) (window-number-select 3)))
(global-set-key (kbd "C-c 4") (lambda () (interactive) (window-number-select 4)))

;; enables guide-key for easy out completion of key bindings
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(setq guide-key/popup-window-position :bottom)
(setq guide-key/idle-delay 0.5)
(guide-key-mode 1)  ; Enable guide-key-mode

;; Enable engine-mode - it allows search engines to be used from within emacs
;; Default key binding is C-x / followed by the engine (see below)
(require 'engine-mode)
(engine-mode t)

;; Creating a dictionary search engine
(defengine dictionary
  "http://www.dictionary.com/browse/%s?s=t"
  :keybinding "d")

;; Creating a google search engine
(defengine google
  "https://www.google.com/#q=%s"
  :keybinding "?")

;; Creating a google image search engine
(defengine image
  "https://www.google.com/search?q=%s&source=lnms&tbm=isch&sa=X"
  :keybinding "i")

;; Creating an amazon search engine
(defengine amazon
  "https://www.amazon.com/s?field-keywords=%s"
  :keybinding "a")

;; Creating a clojuredocs search engine
(defengine clojuredocs
  "https://clojuredocs.org/search?q=%s"
  :keybinding "c")

;; Creating a clojars search engine
(defengine clojars
  "https://clojars.org/search?q=%s"
  :keybinding "j")

;; Creating a clojars search engine
(defengine wikipedia
  "https://en.wikipedia.org/wiki/%s"
  :keybinding "w")
