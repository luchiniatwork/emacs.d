;; Web mode handles html/css/js.
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-engines-alist
        '(("django" . "focus/.*\\.html\\'")
          ("ctemplate" . "realtimecrm/.*\\.html\\'"))))

;; Web beautify prettifies html / css / js using js-beautify
;; install with `npm install -g js-beautify`.
(use-package web-beautify
  :ensure t
  :bind (:map web-mode-map
              ("C-c b" . web-beautify-html)
              :map js2-mode-map
              ("C-c b" . web-beautify-js)))
