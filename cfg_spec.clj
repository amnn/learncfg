;; Balanced Parentheses
(tok 'LPAR "\\(" 'RPAR "\\)")
(cfg (:S => LPAR :S RPAR | :S :S | LPAR RPAR))

;; (a^n)(b^n)
(tok 'A "a" 'B "b")
(cfg (:S => A :S B | ))

;; Addition
{'A "a" 'B "b" 'C "c"}

(cfg (:S => A :S C | :T)
     (:T => B :T C | ))

;; If-then-else
(tok 'IF   "if"
     'THEN "then"
     'ELSE "else"
     'END  "end"
     'VAR  "[A-z][A-z0-9_]*"
     'EQ   "\\="
     'SEMI ";"
     'BOOL "(0|1)")

(cfg (:S => :stmt | :S SEMI :stmt)
     (:stmt => IF :expr THEN :S ELSE :S END
            |  :expr
            |  VAR EQ :expr)
     (:expr => VAR | BOOL))

;; Mathematical Expressions
(tok 'PLUSOP "(\\+|-)"
     'MULOP  "(\\*|/)"
     'LPAR   "\\("
     'RPAR   "\\)"
     'VAR    "[A-z][A-z0-9_]*"
     'NUM    "[0-9]+(\\.[0-9]+)?")

(cfg (:S => :expr)
     (:expr => :term | :expr PLUSOP :term)
     (:term => :val | :term MULOP :val)
     (:val => VAR | NUM | LPAR :expr RPAR))
