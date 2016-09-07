# aql-mode

A simple syntax highlighting and indentation mode for [Arango Query Language](https://docs.arangodb.com/AQL/index.html) (AQL), the language used for querying [ArangoDB](http://www.arangodb.com)

Current indentation support is somewhat limited, but should satisfy most simple cases. Everything should be up to date with ArangoDB version 3.0.

Installation
------------

Put the `aql-mode` directory in `~/.emacs.d` and add the following to your `~/.emacs` file:

```
(add-to-list 'load-path "~/.emacs.d/aql-mode")
(require 'aql-mode)
```

Alternatively, you can place `aql-mode.el` somewhere that emacs knows to look (for example directly in `~/.emacs.d`) and omit that first line.

Usage
-----

By default, `aql-mode` will be loaded and applied for files that end with the ".aql" extension. To load the mode and apply it to a buffer that is already open, use the command:

```
M-x aql-mode
```

to automatically use aql mode for some other extension (say ".arango") update `~/.emacs` with:

```
(add-to-list 'auto-mode-alist '("\\.arango$" . aql-mode))
```

Configuration
-------------

The tab width is currently set to `default-tab-width`, but can be configured by adding the following line to `~/.emacs`:

```
(setq aql-tab-width 4) ;; change 4 to whatever you'd like
```