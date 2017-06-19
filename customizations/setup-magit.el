;; Global keybindings

;; Magit
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-show-refs-popup)
(global-set-key (kbd "C-c g l") 'magit-log-all-branches)
(global-set-key (kbd "C-c g c") 'magit-checkout)
(global-set-key (kbd "C-c g h") 'magit-branch-and-checkout)
(global-set-key (kbd "C-c g p") 'magit-pull-popup)
(global-set-key (kbd "C-c g r") 'magit-rebase-interactive)
