;; In JS indent to 2 spaces.
(setq-default js-indent-level 2)

;; JS2 mode improves on the built in JS mode.
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq-default js2-ignored-warnings '("msg.extra.trailing.comma")))

;; js2-refactor supports some useful refactoring options and builds on top of js2-mode.
(use-package js2-refactor
  :ensure t
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'js2-mode-hook 'js2-refactor-mode))

;; RJSX mode makes JSX work well.
(use-package rjsx-mode
  :ensure t)


;; Prettier-js autoformats JS code - much like `gofmt` - and we hook it into JS2 and RJSX modes.
(use-package prettier-js
  :ensure t
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "es5"
                           "--single-quote" "true"
                           "--print-width" "100"
                           ))
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))

;; js-doc makes it easy to add jsdoc comments via Ctrl+c i.
(use-package js-doc
  :ensure t
  :bind (:map js2-mode-map
              ("C-c i" . js-doc-insert-function-doc)
              ("@" . js-doc-insert-tag))
  :config
  (setq js-doc-mail-address "info@tiagoluchini.eu"
        js-doc-author (format "Tiago Luchini <%s>" js-doc-mail-address)
        js-doc-url "luchini.nyc"
        js-doc-license "MIT License"))

;; Sometimes it's useful to use the local eslint provided by a project's
;; node_modules directory. We call this function from a flycheck hook to
;; enable it automatically.
(defun jc/use-eslint-from-node-modules ()
  "Set local eslint if available."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
