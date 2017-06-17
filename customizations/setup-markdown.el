;; markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; "Fix" the left and right arrows to work exactly like I'm used to everywhere else
(add-hook 'markdown-mode-hook (lambda ()
                                (define-key markdown-mode-map (kbd "M-<left>") 'left-word)
                                (define-key markdown-mode-map (kbd "M-<right>") 'right-word)
                                (define-key markdown-mode-map (kbd "C-<right>") 'markdown-promote)
                                (define-key markdown-mode-map (kbd "C-<left>") 'markdown-demote)
                                (define-key markdown-mode-map (kbd "M-S-<left>") nil)
                                (define-key markdown-mode-map (kbd "M-S-<right>") nil)))
