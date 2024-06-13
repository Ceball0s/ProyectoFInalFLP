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
  (digitoDecimal
   (digit (arbno digit)) number)
  (digitoDecimal
   ("-" digit (arbno digit)) number)
  (digitoBinario
   ("b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoBinario
   ("-" "b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoOctal
   ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")(arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoOctal
   ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoHexadecimal
   ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (digitoHexadecimal
   ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (flotante
   (digit (arbno digit) "." digit (arbno digit)) number)
  (flotante
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program ((arbno struct-decl) expression) a-program)
    ;(expression (number) lit-exp)
    (expression (numero-exp) num-exp)  

    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoHexadecimal) hex-num)

    (expression (identifier) var-exp)
    (expression ("\"" identifier (arbno identifier) "\"") cadena-exp)
    (expression ("true") true-exp)
    (expression ("false") false-exp)

    (expression ("list" "(" (separated-list expression ",") ")") lista-exp)
    (expression ("cons" "(" expression expression ")") cons-exp)
    (expression ("empty") empty-list-exp)
    ;;Primitiva listas
    (expression (primitivaListas "(" expression ")") prim-list-exp)
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    (expression ("if" expression "{" expression "else" expression "}") if-exp)

    (expression ("func" "(" (separated-list identifier ",") ")" expression) func-exp)

    ;;(expression ( "(" expression (arbno expression) ")") app-exp)
    (expression ("call" expression "(" (separated-list expression ",") ")") app-exp)
    
    ;;;;;;
    (expression ("(" expression primitive expression ")") prim-num-exp)
    ;;primitivas numéricas
    (primitive ("+") sum-prim)
    (primitive ("-") minus-prim)
    (primitive ("*") mult-prim)
    (primitive ("mod") mod-prim)
    (primitive ("pow") elevar-prim)
    (primitive ("<") menor-prim)
    (primitive (">") mayor-prim)
    (primitive ("<=") menorigual-prim)
    (primitive (">=") mayorigual-prim)
    (primitive ("!=") diferente-prim)
    (primitive ("==") igual-prim)
    ;;expression para ligaduras modificables
    (expression ("var" (arbno identifier "=" expression) "in" expression) lvar-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    ; características adicionales
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("set" identifier "=" expression)
                set-exp)
    ;; struccturas
    (struct-decl ("struct" identifier "{" (arbno identifier) "}") struct-exp)
    ;;Instanciación y uso de estructuras
    (expression ("new" identifier "(" (separated-list expression ",") ")") new-struct-exp)
    (expression ("get" expression "." identifier) get-struct-exp)
    (expression ("set-struct" expression "." identifier "=" expression) set-struct-exp)

    (expression (primitivaCadena "(" (separated-list expression ",") ")") prim-cad-exp)
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)

    
    ;;Iteradores
    (expression ("for" identifier "from" expression "until" expression "by" expression "do" expression) for-exp)
    (expression ("while" expression "{" expression "}") while-exp)
    
    ;;Switch
    (expression ("switch" "(" expression ")" "{" (arbno "case" expression ":" expression) "default" ":" expression "}") switch-exp)


    (expression ("array" "(" (separated-list expression ",") ")") array-exp)
    ;;Primitiva arrays
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)
    (expression (primitivaArray "(" (separated-list expression ",") ")") prim-array-exp)

    ;;Reconocimiento de patrones
    (expression ("match" expression "{" (arbno regular-exp "=>" expression) "}") match-exp)
    ;;Expresiones regulares
    (regular-exp (identifier "::" identifier) list-match-exp)
    (regular-exp ("numero" "(" identifier ")") num-match-exp)
    (regular-exp ("cadena" "(" identifier ")") cad-match-exp)
    (regular-exp ("boolean" "(" identifier ")") bool-match-exp)
    (regular-exp ("array" "(" (separated-list identifier ",") ")") array-match-exp)
    (regular-exp ("empty") empty-match-exp)
    (regular-exp ("default") default-match-exp)
    ))


(define struct-decl->name
  (lambda (struct)
    (cases struct-decl struct
      (struct-exp (struct-name field-list)
        struct-name)
      (else (eopl:error "struct no valida"))
    )
  )
)

(define struct-decl->atributos
  (lambda (struct)
    (cases struct-decl struct
      (struct-exp (struct-name field-list)
        field-list)
      (else (eopl:error "struct no valida"))
    )
  )
)

(define struct-env '())

(define elaborate-struct-decls!
  (lambda (s-decls)
    (set! struct-env s-decls)))

(define lookup-struct
  (lambda (name)
    (let loop ((env struct-env))
      (cond
        [(null? env) (eopl:error "lookup-struct 'Unknown struct '" name)]
        [(eqv? (struct-decl->name (car env)) name) (car env)]
        [else (loop (cdr env))]
      )
    )
  )
)

(define encontrar_atributo
  (lambda (Latrib atributo acc)
    (cond
      [(null? Latrib) (eopl:error "No se encontro el atributo: " atributo)]
      [(equal? (car Latrib) atributo) acc]
      [else (encontrar_atributo (cdr Latrib) atributo (+ acc 1))]
    )
  )
)

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
      (a-program (struc-dec body)
                 (elaborate-struct-decls! struc-dec)
                 (eval-expression body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))

;(define init-env
;  (lambda ()
;    (extend-env
;     '(i v x)
;     '(1 5 10)
;     (empty-env))))

(define init-env
  (lambda ()
    (extend-env
     '(x y z f)
     (list 4 2 5 2)
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      ;(lit-exp (datum) datum)
      (num-exp (tipo_numero)
        (cases numero-exp tipo_numero
          (decimal-num (dato)  dato)
          (bin-num (dato)  dato)
          (octal-num (dato) dato)
          (hex-num (dato) dato)
        )
      )
      (var-exp (id) (apply-env env id))
      (true-exp () #T)
      (false-exp () #F)
      (prim-num-exp (exp1 prim exp2)
                   (let ((eexp1 (eval-expression exp1 env))
                        (eexp2 (eval-expression exp2 env))
                      )
                     (apply-primitive prim eexp1 eexp2)))
      (prim-cad-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive_string prim args)
        )
      )
      (if-exp (test-exp true-exp false-exp)
              (if (eval-expression test-exp env)
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (func-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args env)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (set-exp (id rhs-exp)
        (let ((argu (eval-expression rhs-exp env)))
          (modificar-env env id argu)
          'void-exp)
        )
        
      (begin-exp (exp exps) 
                 (let loop ((acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (eval-expression (car exps) 
                                               env)
                              (cdr exps)))))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (lvar-exp (ids rands body)
        (let ((args (eval-rands rands (extend-mod-env ids (list->vector rands) env))))
                 (eval-expression body
                                  (extend-mod-env ids (list->vector args) env))))
      (cadena-exp (identificador Lidentifica)
        (letrec
          [
            (crear_string
              (lambda (lids)
                (cond
                  [(null? lids) ""]
                  [else (string-append " " (symbol->string (car lids)) (crear_string (cdr lids)))]
                )
              )
            )
          ]
          (string-append (symbol->string identificador) (crear_string Lidentifica))
        )
      ) 
      (lista-exp (Lexp)
        (eval-rands Lexp env))
      (cons-exp (exp1 exp2)
        (cons (eval-rand exp1 env) (eval-rand exp2 env)))
      (prim-list-exp (prim exp)
        (let ((arg (eval-rand exp env)))
          (apply-list prim arg)))
      (while-exp (boolean_exp body_exp)
          (cond 
              [(eval-expression boolean_exp env)
                (eval-expression body_exp env)
                (eval-expression exp env)
              ]
              [else 'void-exp]
          )
      )
      (for-exp (var start-exp end-exp sum-exp body-exp)
        (let ((start (eval-expression start-exp env))
              (end (eval-expression end-exp env))
              (sum (eval-expression sum-exp env))
            )
          (let loop ((i start))
            (when (< i end)
              (eval-expression body-exp (extend-env (list var) (list i) env))
              (loop (+ i sum)))))
      )
      (switch-exp (var_exp list_caso list_exp default_exp)
        (letrec ((valor (eval-expression var_exp env))
            (coinciden
              (lambda (caso list_e valor)
                (cond
                  [(null? caso) (eval-expression default_exp env)]
                  [(equal? valor (eval-expression (car caso) env)) (eval-expression (car list_e) env)]
                  [else (coinciden (cdr caso) (cdr list_e) valor)]
                )
              )
            )
          )
          (coinciden list_caso list_exp valor)
        )
      )
      (array-exp (lista) 
        (list->vector (eval-rands lista env))
      )
      (prim-array-exp (primitiva lista_argumentos)
        (primitiva-array primitiva (eval-rands lista_argumentos env))
      )
      (empty-list-exp () '())
      (match-exp (exp_var list_casos lista_exp)
        (let ((valor (eval-expression exp_var env)))
          (detector_patron valor list_casos lista_exp env)
        )
      )
      (new-struct-exp (identi lista_atributos)
        (let 
          [
            (evalu_list (eval-rands lista_atributos env))          
            (struct (lookup-struct identi))
          ]
          (if (= (length (struct-decl->atributos struct)) (length evalu_list))
            (cons identi (list (struct-decl->atributos struct) (list->vector evalu_list)))
            (eopl:error "Error el elemento no contiene la lista de atributos requerida, requerido:" (struct-decl->atributos struct))
          )
        )
      )
      (get-struct-exp (struc atributo)
        (let
          [
            (struct (eval-rand struc env))
          ]
          (vector-ref (car (cddr struct)) (encontrar_atributo (cadr struct) atributo 0) )
        )
      )
      (set-struct-exp (strucVar atributo nuevo_valor)
        (letrec
          [
            (struct (eval-rand strucVar env))
            (eNuevo_val (eval-rand nuevo_valor env))
          ]
          (vector-set! (car (cddr struct)) (encontrar_atributo (cadr struct) atributo 0 ) eNuevo_val)
          'void-exp
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

;(define operation
;  (lambda (lst f acc)
;    (cond
;      [(null? lst) acc]
;      [else
;        (operation (cdr lst) f (f acc (car lst) ))])))


;calcula diferentes bases
(define remove-char 
  (lambda (str ch)
    (cond
      [(null? str) '()]
      [else (if (not (char=? ch (car str)))
        (cons (car str) (remove-char (cdr str) ch))
        (remove-char (cdr str) ch))
      ]
    )
  )
)

(define eliminar_caracter
  (lambda (stri ch)
    (list->string (remove-char (string->list stri) ch))
  )
)

(define cambiar-char 
  (lambda (str ch re_ch)
    (cond
      [(null? str) '()]
      [else (if (not (char=? ch (car str)))
        (cons (car str) (remove-char (cdr str) ch))
        (cons re_ch (remove-char (cdr str) ch)))
      ]
    )
  )
)

(define remplazar_caracter
  (lambda (stri ch re_ch)
    (list->string (cambiar-char (string->list stri) ch re_ch))
  )
)

(define decimal-num->numero
  (lambda (decimal)
    (cases numero-exp decimal
      (decimal-num (numero) numero)
      (else (eopl:error "operacion con tipos diferentes es rechazada"))
    )
  )
)

(define bin-num->numero
  (lambda (decimal)
    (cases numero-exp decimal
      (bin-num (numero) numero)
      (else (eopl:error "operacion con tipos diferentes es rechazada"))
    )
  )
)

(define aplicar_segun_base
  (lambda (operation arg1 arg2 tratamiento)
    (cond
      [(string? arg1)
        (cond 
          [(or (equal? (string-ref arg1 0) #\b) (and (equal? (string-ref arg1 1) #\b) (equal? (string-ref arg1 0) #\-)))
            (tratamiento 
              (operation 
                (string->number (eliminar_caracter arg1 #\b ) 2) 
                (string->number (eliminar_caracter arg2 #\b ) 2)
              ) 2)
          ]
          [(or (equal? (string-ref arg1 0) #\h) (and (equal? (string-ref arg1 1) #\h) (equal? (string-ref arg1 0) #\-)))
            (tratamiento 
              (operation 
                (string->number  (remplazar_caracter arg1 #\h #\# )  16) 
                (string->number  (remplazar_caracter arg2 #\h #\# )  16)
              ) 16)
          ]
          [(or (equal? (string-ref arg1 1) #\x) (and (equal? (string-ref arg1 2) #\x) (equal? (string-ref arg1 0) #\-)))
            (tratamiento 
              (operation 
                (string->number (eliminar_caracter arg1 #\x) 8) 
                (string->number (eliminar_caracter arg2 #\x) 8)
              ) 8)
          ]
          [else (display "mentira")]
        )
      ]
      [else  (operation  arg1 arg2)]
    )
  )
)

(define parse-number
  (lambda (num base)
    (let* ((negative? (< num 0))
           (abs-num (abs num)))
      (case base
        [(2) (if negative? (string-append "-"  (string-append "b" (number->string abs-num 2)))  (string-append "b" (number->string abs-num 2)))]
        [(8) (if negative? (string-append "-"  (string-append "0x" (number->string abs-num 8)))  (string-append "0x" (number->string abs-num 8)))]
        [(16) (if negative? (string-append "-"  (string-append "hx" (number->string abs-num 16)))  (string-append "hx" (number->string abs-num 16)))]
        [else (eopl:error "Base no soportada")]
      )
    )
  )
)





(define retorno_tal_cual
  (lambda (out base) out)
)

(define concat
  (lambda (list_stri)
    (cond
      [(null? list_stri) ""]
      [else (string-append (car list_stri ) (concat (cdr list_stri)))]
    )
  )
)

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive_string
  (lambda (prim args)
    (cases primitivaCadena prim
      (concat-primCad () (concat args))
      (length-primCad () (string-length (car args)))
      (index-primCad ()  (string  (string-ref (car args) (cadr args))))
      )))

(define apply-primitive
  (lambda (prim arg1 arg2)
    (cases primitive prim
      (sum-prim () (aplicar_segun_base + arg1 arg2 parse-number ))
      (minus-prim () (aplicar_segun_base - arg1 arg2 parse-number))
      (mult-prim () (aplicar_segun_base * arg1 arg2 parse-number))
      (mayor-prim () (aplicar_segun_base > arg1 arg2 retorno_tal_cual))
      (menor-prim () (aplicar_segun_base < arg1 arg2 retorno_tal_cual))
      (menorigual-prim () (aplicar_segun_base <= arg1 arg2 retorno_tal_cual))
      (mayorigual-prim () (aplicar_segun_base >= arg1 arg2 retorno_tal_cual))
      (diferente-prim ()( not (equal? arg1 arg2)))
      (igual-prim () (equal? arg1 arg2))
      (mod-prim () (aplicar_segun_base remainder arg1 arg2 parse-number))
      (elevar-prim () (aplicar_segun_base expt arg1 arg2 parse-number))
      )))

; aplicar-primitiva lista
(define apply-list
  (lambda (prim arg)
    (cases primitivaListas prim
      (first-primList () (car arg))
      (rest-primList () (cdr arg))
      (empty-primList () (null? arg))
      )))

(define subvector
  (lambda (vect inicio final)
    (cond
      [(= inicio final) (cons (vector-ref vect inicio) '())]
      [else (cons (vector-ref vect inicio) (subvector vect (+ inicio 1) final))]
    )
  )
)

(define primitiva-array
  (lambda (prim arg)
    (cases primitivaArray prim
      (length-primArr () (vector-length (car arg)))
      (index-primArr () (vector-ref (car arg) (cadr arg)))
      (slice-primArr () (list->vector (subvector  (car arg) (cadr arg) (caddr arg))))
      (setlist-primArr () 
        (vector-set! (car arg) (cadr arg) (caddr arg))
        (car arg)
      )   
    )
  )
)

;
(define detector_patron 
  (lambda (valor primt_match expresi_match env)
    (cases regular-exp (car primt_match)
      (empty-match-exp () 
        (if (and (list? valor ) (equal? (car valor) '()))
          (eval-expression (car expresi_match) env)
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
        )
      )
      (list-match-exp (cabeza cola) 
        (if (list? valor)
          (eval-expression (car expresi_match) (extend-env (cons cabeza (cons cola '())) (list (car valor) (cdr valor)) env))
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
        )
      )
      (num-match-exp (ids) 
        (if (number? valor)
          (eval-expression (car expresi_match) (extend-env (list ids) (list valor) env))
          (cond
            [(or (equal? (string-ref valor 0) #\b) (and (equal? (string-ref valor 1) #\b) (equal? (string-ref valor 0) #\-)))
              (eval-expression (car expresi_match) (extend-env (list ids) (list valor) env))
            ]
            [(or (equal? (string-ref valor 0) #\h) (and (equal? (string-ref valor 1) #\h) (equal? (string-ref valor 0) #\-)))
              (eval-expression (car expresi_match) (extend-env (list ids) (list valor) env))
            ]
            [(or (equal? (string-ref valor 1) #\x) (and (equal? (string-ref valor 2) #\x) (equal? (string-ref valor 0) #\-)))
              (eval-expression (car expresi_match) (extend-env (list ids) (list valor) env))
            ]
            [else (detector_patron valor (cdr primt_match) (cdr expresi_match) env)]
          )

          
        )
      )
      (cad-match-exp (ids) 
        (if (string? valor)
          (eval-expression (car expresi_match) (extend-env (list ids) (list valor) env))
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
        )
      )
      (bool-match-exp (ids) 
        (if (boolean? valor)
          (eval-expression (car expresi_match) (extend-env (list ids) (list valor) env))
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
        )
      )
      (array-match-exp (ids) 
        (if (vector? valor)
          (eval-expression (car expresi_match) (extend-env ids (asignar_array ids valor 0) env))
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
        )
      )
      (default-match-exp () 
        (eval-expression (car expresi_match) env)
      )
    )
  )
)

(define asignar_array
  (lambda (lis_ids vect acc)
    (cond
      [(null? (cdr lis_ids)) (cons (subvector vect acc (- (vector-length vect) 1)) '())]
      [else (cons (vector-ref vect acc) (asignar_array (cdr lis_ids) vect (+ acc 1)))]
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

(define mod_list_ids
  (lambda (Lids Lval env)
    (cond
      [(null? Lids) 0]
      [else ((modificar-env env (car Lids) (car Lval)) 
      (mod_list_ids (cdr Lids) (cdr Lval) env))]
    )
  )
)

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args envI)
    (cases procval proc
      (closure (ids body env)
        (cases environment env
          (extend-mod-env (lid lval next-env) 
            (eval-expression body (extend-env ids args envI))
          )
          (else (eval-expression body (extend-env ids args envI)))
        )
      )
    )
  )
)

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (syms (list-of symbol?))
   (valores (list-of scheme-value?))
   (env environment?))
  (extend-mod-env
   (syms (list-of symbol?))
   (valores vector?)
   (env environment?)))
  ;(extended-env-record
  ;  (nombre-procedimientos (list-of symbol?))
  ;  (argumentos-proc (list-of (list-of symbol?)))
  ;  (cuerpos-proc (list-of expression?))
  ;  (old-env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
;(define empty-env  
;  (lambda ()
;    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
;(define extend-env
;  (lambda (syms vals env)
;    (extended-env-record syms (list->vector vals) env)))


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

;funcion para buscar una variable en un ambiente
(define buscador 
  (lambda (lid lval valBus next-amb)
    (cond
      [(null? lid) (apply-env next-amb valBus)]
      [(equal? (car lid) valBus) (car lval)]
      [else (buscador (cdr lid) (cdr lval) valBus next-amb)]
    )  
  ) 
)
(define buscador_mod
  (lambda (lid lval valBus next-amb acc env )
    (cond
      [(null? lid) (apply-env next-amb valBus)]
      [(equal? (car lid) valBus) 
        (if (expression? (vector-ref lval acc)) 
          (eval-expression (vector-ref lval acc) env)
          (vector-ref lval acc)
        )
      ]
      [else (buscador_mod (cdr lid) lval valBus next-amb (+ acc 1) env)]
    )  
  ) 
)

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env () (eopl:error "variable no encontrada " sym))
      (extend-env (lid lval next-env)
        (buscador lid lval sym next-env)
      )
      (extend-mod-env (lid lval next-env)
        (buscador_mod lid lval sym next-env 0 env)
      )
    ) 
  )
)


(define buscador!
  (lambda (lid lval valBus next-amb)
    (cond
      [(null? lid) (extend-env (apply-env next-amb valBus))]
      [(equal? (car lid) valBus) (car lval)]
      [else (buscador (cdr lid) (cdr lval) valBus next-amb)]
    )  
  ) 
)

;función que busca un símbolo en un ambiente
(define set-env!
  (lambda (env sym)
    (cases environment env
      (empty-env () (eopl:error "variable no encontrada"))
      (extend-env (lid lval next-env)
        (buscador lid lval sym next-env)
      )
      (else (eopl:error "variable no encontrada"))
    ) 
  )
)

    ;(deref (apply-env-ref env sym))))
     ;(apply-env-ref env sym)))
    ;env))
;(define apply-env-ref
;  (lambda (env sym)
;    (cases environment env
;      (empty-env-record ()
;                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
;      (extended-env-record (syms vals env)
;                           (let ((pos (rib-find-position sym syms)))
;                             (if (number? pos)
;                                 (a-ref pos vals)
;                                 (apply-env-ref env sym)))))))


;*******************************************************************************************
;modificar ligaduras

(define modificar-env
  (lambda (env sym val)
    (cases environment env
      (extend-mod-env (symB Vect envNext)
        (letrec
          [
            (encontrar_indice
              (lambda (Lsym acc)
                (cond
                  [(null? Lsym) (eopl:error "indice no encontrado " sym)]
                  [(equal? (car Lsym) sym) acc]
                  [else (encontrar_indice (cdr Lsym) (+ acc 1))]
                )
              )
            )
          ]
          (vector-set! Vect (encontrar_indice symB 0) val)
        )
      )
      (extend-env (symB list next-env)
        (modificar-env next-env sym val)
      )
      (else (eopl:error "variable no encontrada en un ambiente valido"))
    )
  )
)

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

;(define un-programa-dificil (a-program una-expresion-dificil))

;; Descomentar para pruebas, vuelva a comentar para envitar
(interpretador)

#|
No tocar
Exporar funciones
|#
(provide (all-defined-out)) 

