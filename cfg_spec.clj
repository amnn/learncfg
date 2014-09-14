;; Balanced Parentheses
{'LPAR "\\(" 'RPAR "\\)"}

(cfg (:S => LPAR :S RPAR | :S :S | LPAR RPAR))

;; (a^n)(b^n)
{'A "a" 'B "b"}

(cfg (:S => A :S B | ))

;; Addition
{'A "a" 'B "b" 'C "c"}

(cfg (:S => A :S C | :T)
     (:T => B :T C | ))

;; If-then-else
{'IF   "if"
 'THEN "then"
 'ELSE "else"
 'END  "end"
 'VAR  "[A-z][A-z0-9_]*"
 'SEMI ";"
 'BOOL "0|1"}

(cfg (:Stmt  => IF :Expr THEN :Stmts ELSE :Stmts END)
     (:Stmts => :Stmt | :Stmts SEMI :Stmt)
     (:Stmt  => :Expr | VAR EQ :Expr)
     (:Expr  => VAR | BOOL))

;; Mathematical Expressions
{'PLUSOP "\\+|-"
 'MULOP  "\\*|/"
 'LPAR   "\\("
 'RPAR   "\\)"
 'VAR    "[A-z][A-z0-9_]*"
 'NUM    "[0-9]+(\\.[0-9]+)?"}

(cfg (:Expr => :Term | :Expr PLUSOP :Term )
     (:Term => :Val | :Term MULOP :Val)
     (:Val => VAR | NUM | LPAR :Expr RPAR))
