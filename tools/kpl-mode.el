
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

(setq myKeywords
      `(("\\(--.*\\)" . 'font-lock-comment-face)
        ("\\(\\<\\S +\\>\\)\\s *(" . font-lock-function-name-face)
        ("\\(\\<\\w+\\>:\\)". 'font-lock-variable-name-face)
        (,keyword-regexp . 'font-lock-keyword-face)
        (,type-regexp . 'font-lock-type-face)))


(defun kpl-indent-line ()
  "Indent current line as KPL code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)		   ; First line is always non-indented
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*end_") ; If the line we are looking at is the end of a block, then decrease the indentation
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) tab-width)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*end_")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "^[ \t*]\\(function\\|code\\|class\\|while\\|for\\|interface\\|until\\|try\\|behavior\\|class\\|method\\|methods\\|record\\|type\\)")
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width)) ; Do the actual indenting
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation


(define-derived-mode kpl-mode fundamental-mode "KPL"
    "Major mode for editing kpl files."
    (kill-all-local-variables)
    (set (make-local-variable 'indent-line-function) 'kpl-indent-line)
    (set (make-local-variable 'font-lock-defaults) '((myKeywords))))

(add-to-list 'auto-mode-alist '(".k\\'" . kpl-mode))
