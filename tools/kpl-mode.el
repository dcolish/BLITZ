;;
;; Dan Colish
;; kpl-mode provides syntax highlighting and stuff for the kpl lang
;;

(defvar type-regexp
  (regexp-opt
   '("array" "bool" "char" "double" "enum" "int" "ptr" "record")))

(defvar keyword-regexp
      (regexp-opt
       '("alloc" "anyType" "arraySize" "asInteger" "asPtrTo"
         "behavior""break" "by" "case" "catch" "char" "class"
         "code" "const" "continue" "debug" "default" "do"  "else"
         "elseIf" "endBehavior" "endClass" "endCode" "endFor"
         "endFunction" "endHeader" "endIf" "endInterface" "endMethod"
         "endRecord" "endSwitch" "endTry" "endWhile" "errors"
         "extends" "external" "false" "fields" "for" "free" "function"
         "header" "if" "implements" "infix" "interface"
         "isInstanceOf" "isKindOf" "messages" "method" "methods" "new"
         "null" "of" "prefix"  "renaming" "return"
         "returns" "self" "sizeOf" "super" "superclass" "switch" "throw"
         "to" "true" "try" "type" "typeOfNull" "until" "uses" "var"
         "while")))

(setq myKeywords
      `(("\\(--.*\\)" . 'font-lock-comment-face)
        ("\\(\\<\\S +\\>\\)\\s *(" . font-lock-function-name-face)
        ("\\(\\<\\w+\\>:\\)". 'font-lock-variable-name-face)
        (,type-regexp . 'font-lock-type-face)
        (,keyword-regexp . 'font-lock-keyword-face)))

(define-derived-mode kpl-mode fundamental-mode "KPL"
    "Major mode for editing kpl files."
    (setq font-lock-defaults '((myKeywords))))
(add-to-list 'auto-mode-alist '(".k\\'" . kpl-mode))
