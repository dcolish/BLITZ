;;
;; Dan Colish
;; kpl-mode provides syntax highlighting and stuff for the kpl lang
;;

(defvar type-regexp
  (regexp-opt
   '("anyType" "array" "bool" "char " "class" "double" "enum" "int" "ptr to"
     "record" "endClass" "endRecord" "typeOfNull") 'words)
  "Regex for matching types in KPL")

(defvar keyword-regexp 
  (regexp-opt 
   '("alloc" "arraySize" "asInteger" "asPtrTo" "behavior""break" "by" "case"
     "catch" "code" "const" "continue" "debug" "default" "do" "else" "elseIf"
     "endBehavior" "endCode" "endFor" "endFunction" "endHeader" "endIf"
     "endInterface" "endMethod" "endSwitch" "endTry" "endWhile" "errors"
     "extends" "external" "false" "fields" "for" "free" "function" "header" "if"
     "implements" "infix" "interface" "isInstanceOf" "isKindOf" "messages"
     "method" "methods" "new" "null" "of" "prefix" "renaming" "return" "returns"
     "self" "sizeOf" "super" "superclass" "switch" "throw" "true" "try" "type"
     "until" "uses" "var" "while") 'words)
  "Regex for matching keywords in KPL")

(defvar kpl-mode-font-lock-defaults
      `(("-.*$" . font-lock-comment-face)
        ("\\(\\<\\S +\\>\\)\\s (" . font-lock-function-name-face)
        ("\\(\\<\\w+\\>:\\)". 'font-lock-variable-name-face)
        (,keyword-regexp . 'font-lock-keyword-face)
        (,type-regexp . 'font-lock-type-face)
        ))

(defvar kpl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "< 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for kpl-mode")

(define-derived-mode kpl-mode fundamental-mode "KPL"
    "Major mode for editing kpl files."
    (kill-all-local-variables)
    (set-syntax-table kpl-mode-syntax-table)
    (make-local-variable 'comment-start)
    (setq comment-start "--")
    (make-local-variable 'comment-start-skip)
    (setq comment-start-skip "--+[ \t]*")
    (make-local-variable 'comment-column)
    (make-local-variable 'parse-sexp-ignore-comments)
    (setq parse-sexp-ignore-comments t)
    (set (make-local-variable 'indent-line-function) 'indent-relative-maybe)
    (set (make-local-variable 'font-lock-defaults) '((kpl-mode-font-lock-defaults))))

(add-to-list 'auto-mode-alist '(".k\\'" . kpl-mode))
