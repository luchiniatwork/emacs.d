;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;;;;;;;;
;; Customizations
;;;;;;;;
;; mostly inspired (or simply copied) from
;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

;; Instead of running end-of-buffer key-binding, the following setting means any other
;; key will jump back to the prompt. Also it's annoying to remember that `find` and `chmod`
;; behave differently from Emacs than their Unix counterparts, so the last setting will
;; prefer the native implementations.
(setq eshell-scroll-to-bottom-on-input 'all
      eshell-error-if-no-glob t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions nil)

;; Eshell would get somewhat confused if we ran the following commands directly through
;; the normal Elisp library, as these need the better handling of ansiterm:
(add-hook 'eshell-mode-hook (lambda ()
                              (add-to-list 'eshell-visual-commands "ssh")
                              (add-to-list 'eshell-visual-commands "tail")))

;;;;;;;;;;
;; Series of functions to configure the prompt
;;;;;;;;;;

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string
                       (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (if (> (length git-output) 0)
          (concat " :" (substring git-output 0 -1))
        "(no branch)"))))

(defun pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun pwd-shorten-dirs (pwd)
  "Shorten all directory names in PWD except the last two."
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 2)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                    (substring elm 0 1)))
                    (butlast p-lst 2)
                    "/")
         "/"
         (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
      pwd)))  ;; Otherwise, we just return the PWD

(defun split-directory-prompt (directory)
  (if (string-match-p ".*/.*" directory)
      (list (file-name-directory directory) (file-name-base directory))
    (list "" directory)))

;; And here's the function that ties everything up together
(setq eshell-prompt-function
      (lambda ()
        (let* ((directory (split-directory-prompt
                           (pwd-shorten-dirs
                            (pwd-replace-home (eshell/pwd)))))
               (parent (car directory))
               (name (cadr directory))
               (branch (or (curr-dir-git-branch-string (eshell/pwd)) ""))

               (dark-env (eq 'dark (frame-parameter nil 'background-mode)))
               (for-bars                 `(:weight bold))
               (for-parent  (if dark-env `(:foreground "#8888FF") `(:foreground "blue")))
               (for-dir     (if dark-env `(:foreground "#aaaaFF" :weight bold)
                              `(:foreground "blue" :weight bold)))
               (for-git                  `(:foreground "green")))

          (concat
           (propertize "┌─ "    'face for-bars)
           (propertize parent   'face for-parent)
           (propertize name     'face for-dir)
           (propertize " ──"    'face for-bars)
           (propertize branch   'face for-git)
           (propertize "\n"     'face for-bars)
           (propertize "└→"     'face for-bars)
           (propertize (if (= (user-uid) 0) " #" " $") 'face `(:weight ultra-bold))
           (propertize " "    'face `(:weight bold))))))

;; Turn off the default prompt, otherwise, it won’t use ours:
(setq eshell-highlight-prompt nil)

;;;;;;;;;;
;; Miscelaneous
;;;;;;;;;;

;; This function is useful for opening an eshell straight into the path of
;; any file you are in
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name   (car (last (split-string parent "/" t)))))
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

;; This function allows new eshells to be created from a given name (say you want
;; to keep one buffer for logging, one for monitoring a process and one for management
(defun eshell-new-named ()
  "creates a shell with a given name"
  (interactive)
  (let ((shell-name (concat "*eshell*<"
                            (read-string "Shell name: " nil)
                            ">")))
    (eshell "new")
    (rename-buffer shell-name)
    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell-new ()
  (interactive)
  (eshell t)
  (insert (concat "ls"))
  (eshell-send-input))


;; Alias for an easy find-file
(defun eshell/e (file)
  (find-file file))

;; Alias for an easy find-file-other-window
(defun eshell/eo (file)
  (find-file-other-window file))

;; Alias for an easy find-file-other-window
(defun eshell/gs ()
  (magit-status))


;; "Fix" the up and down arrows to work exactly like I'm used to on cider
(add-hook 'eshell-mode-hook (lambda ()
                              (define-key eshell-mode-map [up] 'previous-line)
                              (define-key eshell-mode-map [down] 'next-line)))

;; Global bindings to start eshell easily
(global-set-key (kbd "C-c s n") 'eshell-new-named)
(global-set-key (kbd "C-c s h") 'eshell-here)
(global-set-key (kbd "C-c s e") 'eshell-new)
