;;;; Loosely based on https://www.emacswiki.org/emacs/ModeTutorial

(defvar umlaut-mode-hook nil)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.umlaut\\'" . umlaut-mode))

(defconst umlaut-font-lock-keywords
  (list
   '("//.*$" . font-lock-comment-face)
   '("type +\\|enum +\\|interface +\\|diagram +" . font-lock-function-name-face)
   '("\\(\"\\w*\"\\)" . font-lock-doc-face)
   '("@\\(\\w*\\)" . font-lock-preprocessor-face)
   '("ID\\|String\\|Integer\\|Float\\|Boolean" . font-lock-keyword-face)
   '("\\?\\|\\[\\|\\]\\|\\.\\.\\|{\\|}" . font-lock-negation-char-face)
   '("\\(.*\\) ?:" . font-lock-variable-name-face))
  "Minimal highlighting expressions for Umlaut mode")

(defun umlaut-mode ()
  "Major mode for editing Umlaut files"
  (interactive)
  (kill-all-local-variables)
  
(set (make-local-variable 'font-lock-defaults) '(umlaut-font-lock-keywords))

(setq major-mode 'umlaut-mode)
(setq mode-name "umlaut")
(run-hooks 'umlaut-mode-hook))

(provide 'umlaut-mode)
