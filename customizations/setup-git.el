;; Magit is an awesome interface to git. Summon it with `C-c g s`.
(use-package magit 
  :ensure t
  :bind ("C-c g s" . magit-status))

;; Display line changes in gutter based on git history. Enable it everywhere.
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 't)
  :diminish git-gutter-mode)
