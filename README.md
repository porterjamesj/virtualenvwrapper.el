# venvwrapper.el

A featureful virtualenv mode for Emacs. Emulates
much of the functionality of Doug Hellmann's
[virtualenvwrapper](https://bitbucket.org/dhellmann/virtualenvwrapper/)
for Emacs.

## Features

* Works with the new
  [python.el](https://github.com/fgallina/python.el), which is the
  built in on Emacs 24.3 and up. Does not support the older python
  modes.
* Python shells, interactive shells, eshell, and any other subprocesses can
  be made aware of your virtualenvs.
* Implements a large subset of the functionality of virtualenvwrapper.

## Basic Usage

* Make sure you have [dash.el](https://github.com/magnars/dash.el) and
  [s.el](https://github.com/magnars/s.el) installed.
* Obviously make sure you have
  [virtualenv](http://www.virtualenv.org/en/latest/) installed. You
  don't actually need virtualenvwrapper.
* Copy `virtualenvwrapper.el` to somewhere on your `load-path`
* Put
  ```emacs
  (require 'virtualenvwrapper)
  (venv-initialize-shells) ;; if you want interactive shell support
  ```
  in your config somewhere.

* Use `M-x venv-workon` to activate virtualenvs and `M-x
  venv-deactivate` deactivate them.

## What do activating and deactivating actually do?

Many virtual environment support tools describe their functionality as
"it just works" or "it's so simple". This is not descriptive enough to
figure out what's wrong when something inevitably breaks, so here I
will describe *exactly* what happens when you activate a virtualenv:

1. `python-shell-virtualenv-path` is set to the virtualenv's directory
   so that when you open a new python shell, it is aware of the
   virtual environment's installed packages and modules.
2. The virtualenv's `bin` directory is prepended to the `PATH`
   environment variable so that when a process is launched from Emacs
   it is aware of any executables installed in the virtualenv (such as
   `nosetests`, `pep8`, etc.). This comes in handy because you can do
   `M-! nosetests` to run your tests, for example.
3. The `VIRTUAL_ENV` environment variable is set to the virtualenv's
   directory so that any tools that depend on this variable function
   correctly (one such tool is
   [jedi](http://tkf.github.io/emacs-jedi/))
4. The virtualenv's `bin` directory added to the `exec-path`, so that
   Emacs itself can find the environment's installed variables.

When you deactivate, all these things are undone. You can safely
modify your `PATH` and `exec-path` while a virtualenv is active and
expect the changes not to be destroyed.

This covers everything except interactive shells, which are
covered in the next section.

## Shells

This thing supports two types of interactive shells, the
[eshell](https://www.gnu.org/software/emacs/manual/html_mono/eshell.html)
and the [interactive subshell](https://www.gnu.org/software/emacs/manual/html_node/emacs/Interactive-Shell.html) (what you get when you do `M-x shell`).

### Interactive shell

Support for interactive shell is turned on by calling
`venv-initialize-interactive-shell`. After this is done, whenever you call
`shell`, the shell will start in the correct virtualenv. Note that changing
the virtualenv in Emacs will not affect any running shells and vice-versa, they
are independant processes.

#### WARNINGS

This feature is a pretty big hack and works by
[advising](https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html) the `shell` function. This works fine if you haven't otherwise
tricked out or advised it, but if this is the case it may break. Please
file an issue if you encounter any bugs with this functionality, I am
interested to see how robust it is.

### Eshell

Support for eshell is turned on by calling `venv-initialize-eshell`.
After doing this, any new eshells you launch will be in the correct
virtualenv and have access to installed executables, etc. The mode also provides
a variety of virtualenvwrapper funtions that work identically to their bash/zsh
counterparts.
