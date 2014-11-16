;; Balanced Parentheses
(tok 'LPAR "\\(" 'RPAR "\\)")
(cfg (:S => LPAR :S RPAR | :S :S | LPAR RPAR))
(cfg (:S => LPAR :S RPAR :S | ))

;; CNF
(cfg (:S  => :L :R | :L* :R*)
     (:L* => :L :S) (:R* => :R :S)
     (:L  => LPAR)  (:R  => RPAR))

;; (a^n)(b^n)
(tok 'A "a" 'B "b")
(cfg (:S => A :S B | ))

;; CNF
(cfg (:S  => :A :S*)
     (:S* => B | :S :B)
     (:A  => A) (:B => B))

;; Addition
{'A "a" 'B "b" 'C "c"}

(cfg (:S => A :S C | :T)
     (:T => B :T C | ))

(cfg (:S  => :A :S* | :B :T*)
     (:S* => :S :C)
     (:T  => :B :T*)
     (:T* => :T :C | C)
     (:A => A) (:B => B) (:C => C))

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
            |  VAR EQ :expr)
     (:expr => VAR | BOOL))

;; CNF

     ;; Sequencing
(cfg (:S       => :Cond :Act | :V :Set | :S :Next)
     (:Next    => :Semi :S*)

     ;; Statements
     (:S*      => :Cond :Act | :V :Set)

     ;; Assignment
     (:Set     => :Eq :E)

     ;; Conditional
     (:Cond    => :If :E)
     (:Act     => :ThenPt :ElsePt)
     (:ThenPt  => :Then :S)
     (:ElsePt  => :ElsePt* :End)
     (:ElsePt* => :Else :E)

     ;; Expressions
     (:E       => VAR | BOOL)

     ;; Regular Tokens
     (:V       => VAR)
     (:Semi    => SEMI)
     (:Eq      => EQ)
     (:If      => IF)
     (:Then    => THEN)
     (:Else    => ELSE)
     (:End     => END))

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

;; CNF
(cfg (:S  => :S :R1 | :T :R2 | :L :R3 | NUM | VAR)
     (:T  => :T :R2 | :L :R3 | NUM | VAR)
     (:V  => :L :R3 | NUM | VAR)
     (:R1 => :P :T) (:P => +)
     (:R2 => :M :V) (:M => *)
     (:R3 => :S :R)
     (:L  => L)
     (:R  => R))
