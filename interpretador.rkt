#lang eopl

;******************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variables

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expresion>
;;                      <a-program (exp)>
;;  <expresion>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expresion>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expresion>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {<identificador> = <expresion>}* in <expresion>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expresion>
;;                      <proc-exp (ids body)>
;;                  ::= (<expresion> {<expresion>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identificador ({identificador}(,)) = <expresion>} in <expresion>
;;                     <letrec-exp(proc-names idss bodies bodyletrec)>
;;                  ::= begin <expresion> {; <expresion>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identificador> = <expresion>
;;                     <set-exp (id newval)>
;;  <primitive>     ::= + | - | * | add1 | sub1 

;******************************

;******************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identificador
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
  '((program ((arbno struct-decl) expresion) a-program)
    ;(expresion (number) lit-exp)
    (expresion (numero-exp) num-exp)  

    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) flotante-num)

    ;numeritos
    (expresion ("(" expresion primitive expresion ")") prim-num-exp)
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


    (expresion (identificador) var-exp)
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    ;cadenas
    (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)

    (expresion ("true") true-exp)
    (expresion ("false") false-exp)

    
    (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    (expresion ("cons" "(" expresion expresion ")") cons-exp)
    (expresion ("empty") empty-list-exp)
    ;;Primitiva listas
    (expresion (primitivaListas "(" expresion ")") prim-list-exp)
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    (expresion ("if" expresion "{" expresion "else" expresion "}") if-exp)

    (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)

    (expresion ("call" expresion "(" (separated-list expresion ",") ")") app-exp)
    
    
    ;;expresion para ligaduras modificables
    (expresion ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (expresion ("let" (arbno identificador "=" expresion) "in" expresion)
                let-exp)
    ; características adicionales
    (expresion ("begin" expresion (arbno ";" expresion) "end")
                begin-exp)
    (expresion ("set" identificador "=" expresion)
                set-exp)
    ;; struccturas
    (struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)
    ;;Instanciación y uso de estructuras
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    (expresion ("get" expresion "." identificador) get-struct-exp)
    (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    
    ;;Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)
    (expresion ("while" expresion "{" expresion "}") while-exp)
    
    ;;Switch
    (expresion ("switch" "(" expresion ")" "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}") switch-exp)

    ;;Reconocimiento de patrones
    (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)
    ;;Expresiones regulares
    (regular-exp (identificador "::" identificador) list-match-exp)
    (regular-exp ("numero" "(" identificador ")") num-match-exp)
    (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    (regular-exp ("empty") empty-match-exp)
    (regular-exp ("default") default-match-exp)

    ;;primitiva booleana
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)
    (expresion (primitivaBooleana "(" (separated-list expresion ",") ")") prim-bool-exp)

    (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)
    ;;Primitiva arrays
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)
    (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)
    ))

; funciones structuras
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

; fin funciones structuras

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************
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

;*******************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (struc-dec body)
                 (elaborate-struct-decls! struc-dec)
                 (eval-expresion body (empty-env))))))


;eval-expresion: <expresion> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      ;(lit-exp (datum) datum)
      (num-exp (tipo_numero)
        (cases numero-exp tipo_numero
          (decimal-num (dato)  dato)
          (bin-num (dato)  dato)
          (octal-num (dato) dato)
          (hex-num (dato) dato)
          (flotante-num (dato) dato)
        )
      )
      (prim-bool-exp (prim args)
        (evaluar_booleano prim (eval-rands args env))
      )
      (var-exp (id) (apply-env env id))
      (true-exp () #T)
      (false-exp () #F)
      (prim-num-exp (exp1 prim exp2)
        (let ((Evaluated_exp1 (eval-expresion exp1 env))
            (Evaluated_exp2 (eval-expresion exp2 env))
          )
          (apply-primitive prim Evaluated_exp1 Evaluated_exp2)))
      (prim-cad-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive_string prim args)
        )
      )
      (if-exp (test-exp true-exp false-exp)
              (if (eval-expresion test-exp env)
                  (eval-expresion true-exp env)
                  (eval-expresion false-exp env)))
      (func-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expresion rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args env)
                     (eopl:error 'eval-expresion
                                 "Attempt to apply non-procedure ~s" proc)))) 
      (begin-exp (exp exps) 
        (let loop ((acc (eval-expresion exp env))
                    (exps exps))
          (if (null? exps) 
              acc
              (loop (eval-expresion (car exps) 
                                      env)
                    (cdr exps)))))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expresion body
                                  (extend-env ids args env))))
      (lvar-exp (ids rands body)
        (let ((args (eval-rands rands (extend-mod-env ids (list->vector rands) env))))
                 (eval-expresion body
                                  (extend-mod-env ids (list->vector args) env))))
      (set-exp (id newval-exp)
        (let ((argu (eval-expresion newval-exp env)))
          (modificar-env env id argu)
          'void-exp)
        )
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
        (let ((arg (eval-expresion exp env)))
          (apply-list prim arg)))
      (while-exp (boolean_exp body_exp)
          (cond 
              [(eval-expresion boolean_exp env)
                (eval-expresion body_exp env)
                (eval-expresion exp env)
              ]
              [else 'void-exp]
          )
      )
      (for-exp (var start-exp end-exp sum-exp body-exp)
        (let ((start (eval-expresion start-exp env))
              (end (eval-expresion end-exp env))
              (sum (eval-expresion sum-exp env))
            )
          (let loop ((i start))
            (when (< i end)
              (eval-expresion body-exp (extend-mod-env (list var) (vector i) env))
              (loop (+ i sum)))))
      )
      (switch-exp (var_exp list_caso list_exp default_exp)
        (letrec ((valor (eval-expresion var_exp env))
            (coinciden
              (lambda (caso list_e valor)
                (cond
                  [(null? caso) (eval-expresion default_exp env)]
                  [(equal? valor (eval-expresion (car caso) env)) (eval-expresion (car list_e) env)]
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
        (let ((valor (eval-expresion exp_var env)))
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

; funciones auxiliares para aplicar eval-expresion a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expresion rand env)))

;funciones para boleanos

(define evaluar_lista 
  (lambda (operador lista)
    (if (null? (cdr lista))
      (car lista)
      (operador (car lista) (evaluar_lista operador (cdr lista)))
    )
  )
)

(define and-func 
  (lambda (a b)
  (and a b)))

(define or-func 
  (lambda (a b)
  (or a b)))

(define xor-func
  (lambda (a b)
  (or (and a (not b)) (and (not a) b))))

(define evaluar_booleano
  (lambda (prim args)
    (cases primitivaBooleana prim 
      (and-prim () (evaluar_lista and-func args))
      (or-prim () (evaluar_lista or-func args))
      (xor-prim () (evaluar_lista xor-func args))
      (not-prim () (not (car args)))
    )
  )
)

;; metodos para numeros
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
                (string->number (eliminar_caracter arg2 #\b ) 2)) 
              2)
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
          [else (eopl:error "no ingreso un numero valido")]
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
        [(2) (if negative? (string-append "-b" (number->string abs-num 2))  (string-append "b" (number->string abs-num 2)))]
        [(8) (if negative? (string-append "-0x" (number->string abs-num 8))  (string-append "0x" (number->string abs-num 8)))]
        [(16) (if negative? (string-append "-hx"(number->string abs-num 16))  (string-append "hx" (number->string abs-num 16)))]
        [else (eopl:error "Base no soportada")]
      )
    )
  )
)

(define retorno_tal_cual
  (lambda (out base) out)
)

; metodos string
(define concat
  (lambda (list_stri)
    (cond
      [(null? list_stri) ""]
      [else (string-append (car list_stri ) (concat (cdr list_stri)))]
    )
  )
)

;apply-primitive: <primitiva> <list-of-expresion> -> numero
(define apply-primitive_string
  (lambda (prim args)
    (cases primitivaCadena prim
      (concat-primCad () (concat args))
      (length-primCad () (string-length (car args)))
      (index-primCad ()  (string (string-ref (car args) (cadr args))))
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

;metodos de listas
; aplicar-primitiva lista
(define apply-list
  (lambda (prim arg)
    (cases primitivaListas prim
      (first-primList () (car arg))
      (rest-primList () (cdr arg))
      (empty-primList () (null? arg))
      )))

;metodos de vectores
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

;match
(define detector_patron 
  (lambda (valor primt_match expresi_match env)
    (cases regular-exp (car primt_match)
      (empty-match-exp () 
        (if (null? valor)
          (eval-expresion (car expresi_match) env)
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
        )
      )
      (list-match-exp (cabeza cola) 
        (if (and (list? valor) (not (null? valor)))
          (eval-expresion (car expresi_match) (extend-env (cons cabeza (cons cola '())) (list (car valor) (cdr valor)) env))
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
        )
      )
      (num-match-exp (ids) 
        (if (number? valor)
          (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))
          (if (string? valor)
            (cond
              [(or (equal? (string-ref valor 0) #\b) (and (equal? (string-ref valor 1) #\b) (equal? (string-ref valor 0) #\-)))
                (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))
              ]
              [(or (equal? (string-ref valor 0) #\h) (and (equal? (string-ref valor 1) #\h) (equal? (string-ref valor 0) #\-)))
                (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))
              ]
              [(or (equal? (string-ref valor 1) #\x) (and (equal? (string-ref valor 2) #\x) (equal? (string-ref valor 0) #\-)))
                (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))
              ]
              [else (detector_patron valor (cdr primt_match) (cdr expresi_match) env)]
            )
            (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
          )
        )
      )
      (cad-match-exp (ids)
        (if (string? valor) 
          (cond
            [(or (and (equal? (string-ref valor 0) #\b)
                      (or (equal? (string-ref valor 1) #\0) 
                          (equal? (string-ref valor 1) #\1)))
                (and (equal? (string-ref valor 0) #\-)
                      (equal? (string-ref valor 1) #\b)
                      (or (equal? (string-ref valor 2) #\0) 
                          (equal? (string-ref valor 2) #\1))))
            (detector_patron valor (cdr primt_match) (cdr expresi_match) env)]
            [(or (and (equal? (string-ref valor 0) #\h)
                      (equal? (string-ref valor 1) #\x))
                (and (equal? (string-ref valor 0) #\-)
                      (equal? (string-ref valor 1) #\h)
                      (equal? (string-ref valor 2) #\x)))
            (detector_patron valor (cdr primt_match) (cdr expresi_match) env)]
            [(or (and (equal? (string-ref valor 0) #\0)
                      (equal? (string-ref valor 1) #\x))
                (and (equal? (string-ref valor 0) #\-)
                      (equal? (string-ref valor 1) #\0)
                      (equal? (string-ref valor 2) #\x)))
            (detector_patron valor (cdr primt_match) (cdr expresi_match) env)]
            [else (eval-expresion (car expresi_match) 
                            (extend-env (list ids) (list valor) env))]
          )
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
        )
      )

        ;(if (string? valor)
        ;  (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))
        ;  (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
        ;)
      ;)
      (bool-match-exp (ids) 
        (if (boolean? valor)
          (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
        )
      )
      (array-match-exp (ids) 
        (if (vector? valor)
          (eval-expresion (car expresi_match) (extend-env ids (asignar_array ids valor 0) env))
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)
        )
      )
      (default-match-exp () 
        (eval-expresion (car expresi_match) env)
      )
    )
  )
)

(define asignar_array
  (lambda (lis_ids vect acc)
    (cond
      [(null? (cdr lis_ids)) (cons (list->vector (subvector vect acc (- (vector-length vect) 1))) '())]
      [else (cons (vector-ref vect acc) (asignar_array (cdr lis_ids) vect (+ acc 1)))]
    )
  )
)


;*******************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)))


;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args envI)
    (cases procval proc
      (closure (ids body env)
        (cases environment env
          (extend-mod-env (lid lval next-env) 
            (eval-expresion body (extend-env ids args envI))
          )
          (else (eval-expresion body (extend-env ids args envI)))
        )
      )
    )
  )
)

;*******************************
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

(define scheme-value? (lambda (v) #t))



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
        (if (expresion? (vector-ref lval acc)) 
          (eval-expresion (vector-ref lval acc) env)
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




;*******************************
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
                  [(null? Lsym) -1]
                  [(equal? (car Lsym) sym) acc]
                  [else (encontrar_indice (cdr Lsym) (+ acc 1))]
                )
              )
            )
            (indice (encontrar_indice symB 0))
          ]
          (if (= indice -1 )
            (modificar-env envNext sym val)
            (vector-set! Vect indice val)
          )
        )
      )
      (extend-env (symB list next-env)
        (modificar-env next-env sym val)
      )
      (else (eopl:error "variable no encontrada en un ambiente valido"))
    )
  )
)

(define ejemplos-scan
  (list
    "(scan&parse ''b10101010'') \n"
    (scan&parse "b10101010")
    "\n(scan&parse ''-b01010101'') \n"
    (scan&parse "-b01010101")
    "\n(scan&parse ''23213'') \n"
    (scan&parse "232123")
    "\n(scan&parse ''-12312'')\n"
    (scan&parse "-12312")
    "\n(scan&parse ''0x213345'') \n"
    (scan&parse "0x213345")
    "\n(scan&parse ''-0x23123'') \n"
    (scan&parse "-0x23123")
    "\n(scan&parse ''hxFAB123'') \n"
    (scan&parse "hxFAB123")
    "\n(scan&parse ''-hx99EA'') \n"
    (scan&parse "hx99EA")
    "\n(scan&parse ''412312.2312'')\n"
    (scan&parse "412312.2312")
    "\n(scan&parse ''-23123.2312'')\n"
    (scan&parse "-23123.2312")
    "\n(scan&parse ''true'') \n"
    (scan&parse "true")
    "\n(scan&parse ''false'') \n"
    (scan&parse "false")
    "\n(scan&parse ''\''hola mundo\'''') \n"
    (scan&parse "\"hola mundo\"")
    "\n(scan&parse ''\''hola que tal\'''')\n"
    (scan&parse "\"hola que tal\"")
    "\n"
    ))

;******************************
;Pruebas

(show-the-datatypes)

;(define un-programa-dificil (a-program una-expresion-dificil))

;; Descomentar para mostrar los resultados:
;(for-each display ejemplos-scan)

;; Descomentar para pruebas, vuelva a comentar para envitar
(interpretador)

#|
No tocar
Exporar funciones
|#
(provide (all-defined-out))
