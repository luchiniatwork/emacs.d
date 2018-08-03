;; writegood-mode highlights bad word choices and has functions for calculating readability.
(use-package writegood-mode
  :ensure t
  :bind ("C-c w" . writegood-mode)
  :config
  (add-to-list 'writegood-weasel-words "actionable"))

;; restclient-mode is a great tool for debugging APIs
(use-package restclient
  :ensure t)

;; Enable engine-mode - it allows search engines to be used from within emacs
;; Default key binding is C-x / followed by the engine (see below)
(use-package engine-mode
  :ensure t)

;; Creating a dictionary search engine
(defengine dictionary
  "http://www.dictionary.com/browse/%s?s=t"
  :keybinding "d")

;; Creating a google search engine
(defengine google
  "https://www.google.com/#q=%s"
  :keybinding "?")

;; Creating a google image search engine
(defengine image
  "https://www.google.com/search?q=%s&source=lnms&tbm=isch&sa=X"
  :keybinding "i")

;; Creating an amazon search engine
(defengine amazon
  "https://www.amazon.com/s?field-keywords=%s"
  :keybinding "a")

;; Creating a clojuredocs search engine
(defengine clojuredocs
  "https://clojuredocs.org/search?q=%s"
  :keybinding "c")

;; Creating a clojars search engine
(defengine clojars
  "https://clojars.org/search?q=%s"
  :keybinding "j")

;; Creating a clojars search engine
(defengine wikipedia
  "https://en.wikipedia.org/wiki/%s"
  :keybinding "w")
