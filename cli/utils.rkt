#lang racket

(provide parse-arg defparam)

;; 'separator
;; (cons 'regular arg)
;; (cons 'short flags)
;; (cons 'long name)
;; (list 'kv name value)
(define (parse-arg arg)
  (set! arg (string-trim arg))
  (define len (string-length arg))
  (cond [(not (string-prefix? arg "-"))  (cons 'regular arg)]
        [(not (string-prefix? arg "--")) (cons 'short (substring arg 1))]
        [(= len 2)                       'separator]
        [else
         (match (string-split arg "=")
           [(list x)   (cons 'long (substring arg 2))]
           [(list x y) (list 'kv   (substring x 2) y)])]))

(define registered-params '())

(define (register-param name param)
  (set! registered-params
        (cons (cons (symbol->string name) param)
              registered-params)))

;; defines a parameter and registers it with the global registry
(define-syntax (defparam args)
  (syntax-case args ()
    [(_ name default)
     #'(begin
         (define name (make-parameter default))
         (register-param name 'name))]))

;; (define param
;;   (class object%
;;     (init name doc default)
;;     (define param (make-parameter default))
;;     (define/public (parameter) param)
;;     (super-new)))

;; (define-syntax local=
;;   (syntax-rules ()
;;     [(_ param value exprs ...+)
;;      (parameterize ([(send param parameter) value])
;;        exprs ...+)]))

;; (define-syntax local+
;;   (syntax-rules ()
;;     [(_ param value exprs ...+)
;;      (parameterize ([(send param parameter) ((send param combiner) (param) value)])
;;        exprs ...+)]))

;; (define toggle-opt
;;   (class object%

;; (define param-option
;;   (class object%
;;     (init param
;;           names
;;           handler)
;;     (super-new)))

;; (define (converting-handler validator)
;;   (λ args
;;     (match args
;;       [(list) (error "")]
;;       [(cons v rest) (cons (convertor v) rest)
;;        (if (validator v)
           
;; (define (string-handler args)
;;   (match args
;;     [(list) (error "")]
;;     [(cons v rest) (cons v rest)]))

;; append git/


;; ;; defines a task and registers it with the global registry
;; (define-syntax (deftask args)
;;   (syntax-case args ()
;;     [(_ name params body ...)
;;      #'(begin
;;          (define name (make-parameter default))
;;          (register-param name 'name))]))




;; how do we define arguments?
;; -f file ; short flag that takes a parameter
;; -f file -f file ; short flag that can take many parameters
;; -q ;

;; (defparam git/parallelism 100)

;; (deftask git/push
;;  [(param parallelism 
;;
;;

;; (define (parse-options args)
;;   (let po ([args args] [acc '()_
;;   (if (null? args)
;;       (cons '() '())
