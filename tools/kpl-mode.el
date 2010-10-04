;; ;; Dan Colish
;; ;; kpl-mode provides syntax highlighting and stuff for the kpl lang
;; ;;


(setq myKeywords
  '(("\\(--.*\\)" . 'font-lock-comment-face)
    ("\\(\\<\\S +\\>\\)\\s *(" . font-lock-function-name-face)
    ("\\(\\<\\w+\\>:\\)". 'font-lock-variable-name-face)
    ("\\(?:array\\|bool\\|char\\|double\\|enum\\|int\\|ptr\\|record\\)"
      . 'font-lock-type-face)
    ("\\(a\\(?:lloc\\|nyType\\|rraySize\\|s\\(?:Integer\\|PtrTo\\)\\)\\|b\\(?:ehavior\\|reak\\|y\\)\\|c\\(?:a\\(?:se\\|tch\\)\\|har\\|lass\\|o\\(?:de\\|n\\(?:st\\|tinue\\)\\)\\)\\|d\\(?:e\\(?:bug\\|fault\\)\\|o\\)\\|e\\(?:lse\\(?:If\\)?\\|nd\\(?:Behavior\\|C\\(?:lass\\|ode\\)\\|F\\(?:or\\|unction\\)\\|Header\\|I\\(?:f\\|nterface\\)\\|Method\\|Record\\|Switch\\|Try\\|While\\)\\|rrors\\|xte\\(?:nds\\|rnal\\)\\)\\|f\\(?:alse\\|ields\\|or\\|ree\\|unction\\)\\|header\\|i\\(?:f\\|mplements\\|n\\(?:fix\\|terface\\)\\|s\\(?:\\(?:Instance\\|Kind\\)Of\\)\\)\\|me\\(?:ssages\\|thods?\\)\\|n\\(?:ew\\|ull\\)\\|of\\|prefix\\|re\\(?:naming\\|turns?\\)\\|s\\(?:elf\\|izeOf\\|uper\\(?:class\\)?\\|witch\\)\\|t\\(?:hrow\\|o\\|r\\(?:ue\\|y\\)\\|ype\\(?:OfNull\\)?\\)\\|u\\(?:ntil\\|ses\\)\\|var\\|while\\)" 
     . 'font-lock-keyword-face)))

(define-derived-mode kpl-mode fundamental-mode "KPL"
    "Major mode for editing kpl files."
    (setq font-lock-defaults '((myKeywords))))
(add-to-list 'auto-mode-alist '(".k\\'" . kpl-mode))
