#lang eopl

;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variables

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {<identifier> = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp(proc-names idss bodies bodyletrec)>
;;                  ::= begin <expression> {; <expression>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identifier> = <expression>
;;                     <set-exp (id rhsexp)>
;;  <primitive>     ::= + | - | * | add1 | sub1 

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression ("true") true-exp)
    (expression ("false") false-exp)
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expression ( "(" expression (arbno expression) ")")
                app-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) 
                letrec-exp)
    
    ; características adicionales
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("set" identifier "=" expression)
                set-exp)
    ;;;;;;
    ; implementacin diccionario
    ;<e x p r e s s i o n> : : = "{" ( i d e n t i f i e r ":" e x p r e s s i o n ) ∗ ( , ) "}"
    (expression
     ( "{" (separated-list identifier ":" expression ",")"}")
     diccionario-exp)
    ; index
    (expression
     ("index" "(" expression "," identifier ")")
     diccionario-index-exp)
    ; update
    (expression
     ("update" "(" expression "," identifier "," expression ")")
     diccionario-update-exp)
    ; suma
    (expression
     ("sumDic" "(" expression ")")
     diccionario-suma-exp)
    ; multiplicacion
    (expression
     ("mulDic" "(" expression ")")
     diccionario-mult-exp)
    ;;;;;;
    

    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive (">") mayor-prim)
    ;; primitiva display-dick
    (primitive ("display-dict") display-dict)

    ;; proyecto_inicio 
    (expression ("while" expression "{" expression "}") while-exp)
    ))


;Tipos de datos para la sintaxis abstracta de la gramática

;Construidos manualmente:

;(define-datatype program program?
;  (a-program
;   (exp expression?)))
;
;(define-datatype expression expression?
;  (lit-exp
;   (datum number?))
;  (var-exp
;   (id symbol?))
;  (primapp-exp
;   (prim primitive?)
;   (rands (list-of expression?)))
;  (if-exp
;   (test-exp expression?)
;   (true-exp expression?)
;   (false-exp expression?))
;  (let-exp
;   (ids (list-of symbol?))
;   (rans (list-of expression?))
;   (body expression?))
;  (proc-exp
;   (ids (list-of symbol?))
;   (body expression?))
;  (app-exp
;   (proc expression?)
;   (args (list-of expression?)))
;  (letrec-exp
;   (proc-names (list-of symbol?))
;   (idss (list-of (list-of symbol?)))
;   (bodies (list-of expression?))
;   (body-letrec expression?))
;  (begin-exp
;   (exp expression?)
;   (exps (list-of expression?)))
;  (set-exp
;   (id symbol?)
;   (rhs expression?)))
;
;(define-datatype primitive primitive?
;  (add-prim)
;  (substract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim))

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))

(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))

;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z f)
;     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (decr-prim) (cons (var-exp 'y) ())) ())))
;                      (empty-env)))
;     (empty-env))))
; definicion de dicionario
(define-datatype diccionario diccionario?
  (dict-empty)
  (dict-no-empty (llave symbol?) (valor number?) (dict-ext diccionario?))
)

(define ordenar_dicionario
  (lambda (diccio)
    (letrec
      (
        (ordenar_merge
          (lambda (dic1 dic2)
            (cond
              [(null? dic1) dic2]
              [(null? dic2) dic1]
              [(string>? (symbol->string (caar dic1)) (symbol->string (caar dic2))) 
                (cons (car dic2) (ordenar_merge dic1 (cdr dic2)))
              ]
              [else (cons (car dic1) (ordenar_merge (cdr dic1) dic2))]
            )
          )
        )
        (take
          (lambda (array numero)
            (cond
              [(= numero 0) '()]
              [else (cons (car array) (take (cdr array) (- numero 1)))]
            )
          )
        ) 
        (drop
          (lambda (array numero)
            (cond
              [(= numero 0) array]
              [else (drop (cdr array) (- numero 1))]
            )
          )
        )
        (merge_sort
          (lambda (arr-type-dic)
            (if (<= (length arr-type-dic) 1) arr-type-dic
              (let
                (
                  [mitad (/ (length arr-type-dic) 2)]
                ) 
                (ordenar_merge
                  (merge_sort (take arr-type-dic (round mitad)))
                  (merge_sort (drop arr-type-dic (round mitad)))
                )
                
              )
            )
          )
        ) 
      )
      (merge_sort  diccio)
      
    )
  )
)
;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (true-exp () #T)
      (false-exp () #F)
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (eval-expression test-exp env)
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (while-exp (test-exp cuerpo-exp)
        (if (eval-expression test-exp env) 
          (
            (eval-expression  cuerpo-exp env) 
            (eval-expression  (while-exp test-exp cuerpo-exp) env) 
          )
          '()  ))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                 1))
      (begin-exp (exp exps) 
                 (let loop ((acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (eval-expression (car exps) 
                                               env)
                              (cdr exps)))))
      (diccionario-exp (llaves valores)
        (letrec (
            (inser_no_rep_dict
              (lambda (dic llave valor)
                (cond
                  [(null? dic) (list (list llave valor))]
                  [(equal? (caar dic) llave) (cons (list llave valor) (cdr dic))]
                  [else (cons (car dic) (inser_no_rep_dict (cdr dic) llave valor))]
                )
              )
            )
            (crear_dic
              (lambda (llaves valores dic)
                (if (null? llaves) dic 
                  (crear_dic (cdr llaves) (cdr valores) (inser_no_rep_dict dic (car llaves) (car valores)))
                )
              )
            )
          )
          (ordenar_dicionario (crear_dic llaves (eval-rands valores env) '()))
        ) 
      )
      (diccionario-index-exp (dicciona indice)
        (letrec
          (
            [dic (eval-expression dicciona env)]
            [retornar_valor
              (lambda (dicc indice)
                (cond
                  [(null? dicc) (eopl:error "el indice no existe")]
                  [(equal? (caar dicc) indice) (cadar dicc)]
                  [else (retornar_valor (cdr dicc) indice)]
                )
              )
            ]
          )
          (retornar_valor dic indice)
        )
      )
      (diccionario-update-exp (expt1 indice exp2)
        (letrec
          (
            [dic (eval-expression expt1 env)]
            [valor (eval-expression exp2 env)]
            [actualizar_diccionario
              (lambda (dicc indice valor)
                (cond
                  [(null? dicc) (eopl:error "el indice no existe")]
                  [(equal? (caar dicc) indice) (cons (list indice valor) (cdr dicc))]
                  [else (cons (car dicc) (actualizar_diccionario (cdr dicc) indice valor))]
                )
              )
            ]
          )
          (actualizar_diccionario dic indice valor)
        )
      )
      (diccionario-suma-exp (exp)
        (letrec
          (
            [dic (eval-expression exp env)]
            [suma_diccionario
              (lambda (dicc acc)
                (cond
                  [(null? dicc) acc]
                  [else (suma_diccionario (cdr dicc) (+ acc (cadar dicc)))]
                )
              )
            ]
          )
          (suma_diccionario dic 0)
        )
      )
      (diccionario-mult-exp (exp)
        (letrec
          (
            [dic (eval-expression exp env)]
            [mult_diccionario
              (lambda (dicc acc)
                (cond
                  [(null? dicc) acc]
                  [else (mult_diccionario (cdr dicc) (* acc (cadar dicc)))]
                )
              )
            ]
          )
          (mult_diccionario dic 1)
        )
      )
    )
  )
)

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;operation: Aplicar una operación a la lista

(define operation
  (lambda (lst f acc)
    (cond
      [(null? lst) acc]
      [else
        (operation (cdr lst) f (f acc (car lst) ))])))


;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (operation args + 0))
      (substract-prim () (operation (cdr args) - (car args)))
      (mult-prim () (operation args * 1))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (mayor-prim () (> (car args) (cadr args)))
      (display-dict () 
        (letrec (
            (recorrer_diccionario
              (lambda (diccio)
                (cond
                  [(null? diccio) (string "}")]
                  [(null? (cdr diccio)) (string-append (symbol->string (caar diccio)) ":" (number->string (cadar diccio)) "}")]
                  [else (string-append (symbol->string (caar diccio)) ":" (number->string (cadar diccio)) ", " (recorrer_diccionario (cdr diccio)))]
                )
              )
            )
          )
          (string-append "{" (recorrer_diccionario (car args)))
        )
      
      )
    )
  )
)

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;(define iota
;  (lambda (end)
;    (iota-aux 0 end)))
;
;(define iota-aux
;  (lambda (ini fin)
;    (if (>= ini fin)
;        ()
;        (cons ini (iota-aux (+ 1 ini) fin)))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))
     ;(apply-env-ref env sym)))
    ;env))
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))


;*******************************************************************************************
;Referencias

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;******************************************************************************************
;Pruebas

(show-the-datatypes)
just-scan
scan&parse
(just-scan "add1(x)")
(just-scan "add1(   x   )%cccc")
(just-scan "add1(  +(5, x)   )%cccc")
(just-scan "add1(  +(5, %ccccc x) ")
(scan&parse "add1(x)")
(scan&parse "add1(   x   )%cccc")
(scan&parse "add1(  +(5, x)   )%cccc")
(scan&parse "add1(  +(5, %cccc
x)) ")
(scan&parse "if -(x,4) then +(y,11) else *(y,10)")
(scan&parse "let
x = -(y,1)
in
let
x = +(x,2)
in
add1(x)")

(define caso1 (primapp-exp (incr-prim) (list (lit-exp 5))))
(define exp-numero (lit-exp 8))
(define exp-ident (var-exp 'c))
(define exp-app (primapp-exp (add-prim) (list exp-numero exp-ident)))
(define programa (a-program exp-app))
(define una-expresion-dificil (primapp-exp (mult-prim)
                                           (list (primapp-exp (incr-prim)
                                                              (list (var-exp 'v)
                                                                    (var-exp 'y)))
                                                 (var-exp 'x)
                                                 (lit-exp 200))))
(define un-programa-dificil
    (a-program una-expresion-dificil))

;; Descomentar para pruebas, vuelva a comentar para envitar
(interpretador)

#|
No tocar
Exporar funciones
|#
(provide (all-defined-out)) 



