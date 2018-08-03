(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; "Fix" the left and right arrows to work exactly like I'm used to everywhere else
(add-hook 'markdown-mode-hook (lambda ()
                                (define-key markdown-mode-map (kbd "M-<left>") 'left-word)
                                (define-key markdown-mode-map (kbd "M-<right>") 'right-word)
                                (define-key markdown-mode-map (kbd "C-<right>") 'markdown-promote)
                                (define-key markdown-mode-map (kbd "C-<left>") 'markdown-demote)
                                (define-key markdown-mode-map (kbd "M-S-<left>") nil)
                                (define-key markdown-mode-map (kbd "M-S-<right>") nil)))

;; flymd does not work with any other browser but firefox
;; This function makes sure firefox is triggered for it
;; More info here: https://github.com/mola-T/flymd/blob/master/browser.md
(defun my-flymd-browser-function (url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))

;; uses the function defined above
(setq flymd-browser-open-function 'my-flymd-browser-function)
