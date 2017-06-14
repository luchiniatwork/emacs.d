;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; Uses a nicer shell prompt with git integration
(eshell-git-prompt-use-theme 'git-radar)

;; Making sure that linum is turned off on the REPL
;; Performance is much better when it's off
(add-hook 'eshell-mode-hook (lambda () (linum-mode -1)))
