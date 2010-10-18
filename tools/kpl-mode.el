;;
;; Dan Colish & Cory Kolbeck
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

;Searchs back through the buffer for the last non-blank, non-comment
(defun last-meaningful () 
  (unless (bobp) (forward-line -1))
	
  (while (and (not (bobp)) (or (looking-at "^[ \t]*\\(--.*\\)?$"))) 
    (forward-line -1))
)

;Searchs back through the buffer for the last non-blank
(defun last-nonblank () 
  (unless (bobp) (forward-line -1))
	
  (while (and (not (bobp)) (or (looking-at "^[ \t]*$"))) 
    (forward-line -1))
)

;indentation for variable declarations
(defun var-indent ()
  (last-meaningful)
  (if (looking-at "^[ \t]*\\(var\\|fields\\)[ \t]*$") 
      (+ (current-indentation) 2) 
    (current-indentation))
)

(defun case-indent () 
  (if (re-search-backward "^[ \t]*switch[ \t]" nil t) 
      (current-indentation)
    (progn 
      (message "No 'switch' found for 'case' statement") 
      (current-indentation))))

;TODO: Fix indentation of endClass in header files
(defun get-indent ()
    (save-excursion
      (beginning-of-line)
      (let ((delta 0)) 
	(cond 
	 
	 ((looking-at "^[ \t]*--.*$") ;Indent comments to the level of their parent  
	  (progn (last-nonblank)
		 (current-indentation)))

;I found this annoying, YMMV
;	 ((looking-at "^[ \t]*$") 0) ;Get rid of any whitespace on blank lines

	 ((looking-at "^[ \t]*[a-zA-Z0-9_]+:.*") (var-indent)) ;Indent variable decls specially,
	 
	 ((looking-at "^[ \t]*case .*:") (case-indent))
	 
	 (t (progn ;Indent everything else relative to the last meaningful line
	      (if (looking-at "^[ \t]*\\(end\\|else\\).*") (setq delta (- 0 tab-width)))	      
	      (last-meaningful)
	      (if (looking-at "^[ \t]*[a-zA-Z_]+:.*") (setq delta (- delta 2)))
	      
	      (+ delta
	       (if (looking-at "^[ \t]*\\(?:function\\|fields\\|class\\|while\\|for\\|interface\\|until\\|try\\|behavior\\|method\\|methods\\|record\\|type\\|if\\|else\\|case\\).*") 
		   (+ (current-indentation) tab-width)
		 (current-indentation)))))))))


(defun kpl-indent-line ()
  "Indent current line as KPL code."
  (interactive)
  (let ((ind (get-indent)))
;    (message "ind = %d, tabwidth = %d" ind tab-width)
    (if (< ind 0) (setq ind 0))
    (indent-line-to ind)))


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
    (set (make-local-variable 'indent-line-function) 'kpl-indent-line)
    (set (make-local-variable 'font-lock-defaults) '((kpl-mode-font-lock-defaults))))

(add-to-list 'auto-mode-alist '(".k\\'" . kpl-mode))
