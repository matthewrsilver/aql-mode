;;; aql-mode.el --- major mode for Arango Query Language (AQL) syntax highlighting

;; Copyright (C) 2016, by Matthew R. Silver

;; Author: Matthew Silver (msilver@vectranetworks.com)
;; Version: 0.1
;; Created: July 26, 2016

;;; License: MIT License -- see LICENSE.txt

;; TODO:
;;   - line indentation (indent after for, "{", and "(" )
;;   - built-in functions?
;;   - namespaces?
;;   - boundary handling in numeric literals is imperfect


;; define keyword categories
(setq aql-keywords
      '("AGGREGATE"     "ALL"           "AND"           "ANY"
	"ASC"           "COLLECT"       "DESC"          "DISTINCT"
        "FILTER"        "for"           "GRAPH"         "IN"
        "INBOUND"       "INSERT"        "INTO"          "LET"
        "LIMIT"         "NOT"           "OR"            "OUTBOUND"
        "REMOVE"        "REPLACE"       "RETURN"        "SHORTEST_PATH"
        "SORT"          "UPDATE"        "UPSERT"        "WITH"
	))

(setq aql-constants
      '("FALSE"         "NONE"          "NULL"          "TRUE"
        ))

;; generate regex strings for each category
(setq aql-keywords-regexp (regexp-opt aql-keywords 'words))
(setq aql-constants-regexp (regexp-opt aql-constants 'words))
(setq aql-numeric-regexp "\\_<[0-9]*\\.?[0-9]*\\_>")
(setq aql-bindvar-regexp "@[a-zA-z_]*")

;; associate categories with faces
(setq aql-font-lock-keywords
      `(
        (,aql-keywords-regexp . font-lock-keyword-face)
        (,aql-constants-regexp . font-lock-constant-face)
        (,aql-numeric-regexp . font-lock-warning-face)
        (,aql-bindvar-regexp . font-lock-variable-name-face)
        ))

(define-derived-mode aql-mode c-mode
  "AQL Mode"
  "Major mode for editing Arango Query Language (AQL)"

  (setq font-lock-defaults '((aql-font-lock-keywords)))
  (set (make-local-variable 'font-lock-defaults)

       ;; user may define aql-font-lock-keywords to override
       '(aql-font-lock-keywords nil t))

  )

;; clear memory
(setq aql-keywords nil)
(setq aql-keywords-regexp nil)
(setq aql-constants nil)
(setq aql-constants-regexp nil)
(setq aql-numeric-regexp nil)
(setq aql-bindvar-regexp nil)

;; add the mode to the features list
(provide 'aql-mode)

;; associate this mode with .aql files
(add-to-list 'auto-mode-alist '("\\.aql$" . aql-mode))