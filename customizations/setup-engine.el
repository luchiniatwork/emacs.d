(require 'engine-mode)
(engine-mode t)

(defengine dictionary
  "http://www.dictionary.com/browse/%s?s=t"
  :keybinding "d")

(defengine google
  "https://www.google.com/#q=%s"
  :keybinding "")

(defengine clojuredocs
  "https://clojuredocs.org/search?q=%s"
  :keybinding "c")
