;; Customizations relating to editing a buffer.

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)


;; Basic colors - not the distracting, all-over-the-place colors
(setq whitespace-style (quote (spaces tabs newline tab-mark newline-mark empty)))

;; Make space-mark look like middle dot, newline as an arrow and tab as a right arrow
(setq whitespace-display-mappings
      '(
        (space-mark 32 [183])
        (newline-mark 10 [8629 10])
        (tab-mark 9 [8614 9] [92 9])
        ))

;; Enable whitespace
(global-whitespace-mode 1)

;; Comments or uncomments whole regions
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)

;; yay rainbows!
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;; functions for MacOs clipboard integration
(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "C-c c") 'pbcopy)
(global-set-key (kbd "C-c x") 'pbcut)
(global-set-key (kbd "C-c v") 'pbpaste)

(setq electric-indent-mode nil)

;; enabling and setting up git-gutter 
(global-git-gutter-mode 1)
(custom-set-variables
 '(git-gutter:update-interval 2))

(custom-set-variables
 '(git-gutter:hide-gutter t))

(custom-set-variables
 '(git-gutter:modified-sign "*") ;; two space
 '(git-gutter:added-sign "+")    ;; multiple character is OK
 '(git-gutter:deleted-sign "-"))

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
  (command-execute 'balance-windows)
)

(global-set-key (kbd "C-x 4") 'split-3-windows-horizontally-evenly)


;; Bind to the traditional C-x o that would normally be on `other-window`
(global-set-key (kbd "C-x o") 'window-number-switch)
