;; Balanced Parentheses
{'LPAR "\\(" 'RPAR "\\)"}

(cfg (:S => LPAR :S RPAR | :S :S | LPAR RPAR))

;; (a^n)(b^n)
{'A "a" 'B "b"}

(cfg (:S => A :S B | A B))

;; Addition
{'A "a" 'B "b" 'C "c"}

(cfg (:S => A :S C | :T)
     (:T => B :T C | B C))

;; If-then-else
{'IF   "if"
 'THEN "then"
 'ELSE "else"
 'END  "end"
 'VAR  "[A-z][A-z0-9_]*"
 'BOOL "0|1"}

(cfg (:Stmt  => IF :Expr THEN :Stmts ELSE :Stmts END)
     (:Stmts => :Stmt | :Stmt SEMI :Stmts)
     (:Stmt  => :Expr | VAR EQ :Expr)
     (:Expr  => VAR | BOOL))

;; Mathematical Expressions
{'PLUSOP "\\+|-"
 'MULOP  "\\*|/"
 'LPAR   "\\("
 'RPAR   "\\)"
 'VAR    "[A-z][A-z0-9_]*"
 'NUM    "[0-9]+(\.[0-9]+)?"}

(cfg (:Expr => :Term | :Expr PLUSOP :Expr )
     (:Term => :Val | LPAR :Expr RPAR | :Term MULOP :Term)
     (:Val => VAR | NUM))
