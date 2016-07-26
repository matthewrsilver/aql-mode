;;; aql-mode.el --- major mode for Arango Query Language (AQL) syntax highlighting

;; Copyright (C) 2016, by Matthew Silver

;; Author: Matthew Silver (msilver@vectranetworks.com)
;; Version: 0.1
;; Created: July 26, 2016

;;; License: None? Do anything you want with this.

;; based off of http://www.ergoemacs.org/emacs/elisp_syntax_coloring.html
;; TODO:
;;   - comments
;;   - line indentation


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

;; associate categories with faces
(setq aql-font-lock-keywords
      `(
        (,aql-keywords-regexp . font-lock-keyword-face)
        (,aql-constants-regexp . font-lock-constant-face)
        ))

(define-derived-mode aql-mode fundamental-mode
  "AQL Mode"
  "Major mode for editing Arango Query Language (AQL)"

  (setq font-lock-defaults '((aql-font-lock-keywords)))
  (set (make-local-variable 'font-lock-defaults)
       '(aql-font-lock-keywords nil t))
       ;; user may define aql-font-lock-keywords
  )

;; clear memory
(setq aql-keywords nil)
(setq aql-keywords-regexp nil)
(setq aql-constants nil)
(setq aql-constants-regexp nil)

;; add the mode to the features list
(provide 'aql-mode)
