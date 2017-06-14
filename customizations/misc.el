;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Global keybindings

;; Magit
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-show-refs-popup)
(global-set-key (kbd "C-c g l") 'magit-log-all-branches)
(global-set-key (kbd "C-c g c") 'magit-checkout)
(global-set-key (kbd "C-c g h") 'magit-branch-and-checkout)
(global-set-key (kbd "C-c g p") 'magit-pull-popup)
(global-set-key (kbd "C-c g r") 'magit-rebase-interactive)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-c s s") (lambda () (interactive) (eshell t)))
