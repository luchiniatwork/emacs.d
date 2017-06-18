# My own emacs config #

It's mostly an evolution to my own taste from the Clojure-friendly
emacs config from [Clojure for the Brave and True](http://www.braveclojure.com/basic-emacs/).

If you're new to emacs, check out
[this introductory tutorial](http://www.braveclojure.com/basic-emacs/)!

## Installing

1. Close Emacs.
2. Delete `~/.emacs` or `~/.emacs.d` if they exist. (Windows users, your
   emacs files will probably live in
   `C:\Users\your_user_name\AppData\Roaming\`. So, for example, you
   would delete `C:\Users\jason\AppData\Roaming\.emacs.d`.) This is
   where Emacs looks for configuration files, and deleting these files
   and directories will ensure that you start with a clean slate.
3. Clone the contents of this repo into `~/.emacs.d`
4. Open Emacs. It might ask for permission to add the package archives to
   your system. Approve all.
5. Some compilation warnings may pop up during the very first run as we are
   installing directly from MELPA. If you want MELPA-stable instead, see below.
6. Close and open Emacs again just to get rid of any residue and to feel good
   about your setup.
7. Enjoy Emacs!!!

If you intend to use cider, please also follow these two steps:

1. Create the file `~/.lein/profiles.clj` (Windows users, this is probably
   `C:\Users\your_user_name\.lein\profiles.clj`).
2. Add this line to said file:

```clojure
{:user {:plugins [[cider/cider-nrepl "0.15.0-snapshot"]]}}
```

## Organization of the code

There's an effort to separate everything logically and document the purpose
of every line. `init.el` acts as a kind of table of contents. The variable
`my-packages` contains all the external modes this setup depends on.

The directory `customizations` is where each of these modes (and any other
details) are configured.

It's a good idea to eventually go through `init.el` and the files under the 
`customizations` directory so that you know exactly what's going on.

## Option to use MELPA-stable instead

Since this configuration is very fluid and I'm always reinstalling it from
scratch, I don't find any problem in using MELPA as a package archive.

If, for some reason, you would prefer the stable version, simply add
`http://stable.melpa.org/packages/` to the `package-archives` right at the
top of the `init.el` file. This should work:

```elisp
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
```

Marmalade and GNU have been removed because I tend to find all I need in
MELPA.

## Main highlights of what's supported

First, here's a list of the languages and systems I use on daily basis. This list might
explain why some of the configuration choices have come to be:

* **clojure/cider**: a lot of clojure coding and cider/nrepl debugging has happened with this
  setup
* **markdown**: editing markdown files is a reality in our lives as developers nowadays
* **eshell**: I try hard not to leave Emacs, so eshell has received a lot of love
* **js/html/css**: not as much as I used to edit these files but this setup does a decent job
* **elisp**: I'm not affraid of making my own functions and modes when I need them
* **docker**: you may notice a `dockerfile-mode` and a `docker-mode`. I manage my containers and
  images via Emacs.
* **git**: I guess you didn't see that coming! `magit` and `git-gutter` are two invaluable tools
  on my daily workflow.

These are the Emacs-specific features and modes that I can't live without and are also very
much configured to my taste:

* **guide-key**: if you press `C-x` and wait you'll notice a pop up menu with key bindings
  that can be used in connection with `C-x`. The timer is intentional as I don't like that
  showing up all the time.
* **engine-mode**: there are a few search engines configured in the `navigation.el` file. They
  are super handy.
* **restclient-mode**: great for testing APIs.
* **company-mode**: enabled globally. It works amazingly well for everything I use.
* **window-number/transpose**: two window-specifc packages that make navigating between windows
  a piece of cake.
* **ido/smex**: ido in ubiquitous mode. Leveraging the minibuffer efficiently is a requirement.
* **emacs daemon-mode**: I use `emacsclient` quite a bit, so this is turned on by default.

Things I have in the setup but still need to spend some time learning and fine-tuning:

* **projectile**: should be great but for some reason I didn't get to need it yet.

Things I'll add one day - particularly when the need arises:

* **tramp**: dealing with remote files seems to be very fluid on tramp but the need hasn't been
  there yet.

## Supporting a new language

In general, if you want to add support for a language then you should be able to find
good instructions for it through Google. Most of the time, you'll just need to install the
"x-lang-mode" package for it. If you want anything custom, create a script inside the
`customizations` directory (I tend to call them `setup-<language>.el`) and `load` it from
`init.el`.

## Supporting anything else

As for new modes we get used to on your workflow, make sure to add them to `my-packages`.
I tend to ignore the `custom-set-variables` when patching this code up (I don't like making
those variables a dependency on my setup). This means that any new mode installed manually
may eventually reside there. If the `package-selected-packages` does not get added to the
repo often enough, I may lose these package on a reinstall (and I reinstall often). Therefore,
my recommendation is to keep your `init.el` sane as you evolve it. You want it to survive
several decades.

## Why not Spacemacs?

Spacemacs is a superb project that makes Emacs accessible to so many new users. They are doing
an insanelly good work and the final results are beautiful and polished. I highly recommend
Spacemacs to many developers - particularly those scared of Emacs. Evil-mode on Spacemacs
is so well configured that a vim user can be productive in no time.

However, for me in particular, I kind of like _knowing_ how to hack my most important tool.
Spacemacs is totally hackeable (and I did copy things from them on this setup). But being
comfortable starting a raw Emacs, operating it somewhat efficiently, and making it feel like
home on _any_ environment is a great feeling. If something doesn't work the way I want, I
can always _make_ it work in a few minutes.

Please do use _and_ hack Spacemacs! It's a great initiative that will fast-track you. A
bootstrap like this will teach you a lot but is definitely the slow route.
