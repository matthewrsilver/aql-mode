# aql-mode

A simple syntax highlighting mode for Arango Query Language (AQL), the language used for querying [ArangoDB](http://www.arangodb.com)

There's currently no support for any reasonable indentation functionality. For now, use spaces and try not to hit the tab key.


Installation
------------

Place aql-mode.el somewhere that emacs knows to look, for example `~/.emacs.d`. Alternatively, put the `aql-mode` directory wherever you'd like and add it to your `load-path`:

```
(add-to-list 'load-path "~/.emacs.d/aql-mode")
```

Then, in your `~/.emacs` file, add the line

```
(require 'aql-mode)
```


Usage
-----

By default, `aql-mode` will be loaded and applied for files that end with the ".aql" extension. To load the mode and apply it to a buffer that is already open, use the command:

```
M-x aql-mode
```
