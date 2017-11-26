;; make sure it's hooked to flycheck for on-the-fly syntax checking
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; hook to racer for auto-complete
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

;; make sure the "TAB" key is activating auto-completion
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
