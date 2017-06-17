(require 'engine-mode)
(engine-mode t)

(defengine dictionary
  "http://www.dictionary.com/browse/%s?s=t"
  :keybinding "d")

(defengine google
  "https://www.google.com/#q=%s"
  :keybinding "?")

(defengine image
  "https://www.google.com/search?q=%s&source=lnms&tbm=isch&sa=X"
  :keybinding "i")

(defengine amazon
  "https://www.amazon.com/s?field-keywords=%s"
  :keybinding "a")

(defengine clojuredocs
  "https://clojuredocs.org/search?q=%s"
  :keybinding "c")
