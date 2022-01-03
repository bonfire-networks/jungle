#lang racket

(require srfi/1)
(require rackjure/threading)

(provide id <<< any? none? all? map-truthy
         to-string str
         which run-command
         string-input string-output trim-output trim-output-or-false
         with-silent-stderr
         basename
         replace-empty-with
         concurrent/each)

(define id identity)
(define <<< compose)

;;; returns #t if any list item passes the predicate
(define (any? fn list)
  (and (find-tail fn list) #t))

;;; returns #t if no list item passes the predicate
(define (none? fn list)
  (not (any? fn list)))

;;; returns #t if all list items pass the predicate (i.e. if none do not)
(define (all? fn list)
  (not (any? (negate fn) list)))

(define (map-truthy fn val)
  (if val (fn val) val))

;;; if it is not a string already, calls print on it.
(define (to-string v)
  (cond [(string? v) v]
        [(symbol? v) (symbol->string v)]
        [(path? v)   (path->string v)]
        [else (~v v)]))

;;; concatenates to-string of the provided
(define (str . pieces)
  (match pieces
    [(list) ""]
    [(list p) (to-string p)]
    [else (~>> (map to-string pieces)
               (apply string-append-immutable))]))
 
;; sugar around with-input-from-string to avoid the lambda
(define-syntax string-input
  (syntax-rules ()
    [(_ a b ...)
     (with-input-from-string a (lambda () b ...))]))

;; sugar around with-output-to-string to avoid the lambda
(define-syntax string-output
  (syntax-rules ()
    [(_ a ...)
     (with-output-to-string (lambda () a ...))]))

;; sugar around with-output-to-string to avoid the lambda
(define-syntax trim-output
  (syntax-rules ()
    [(_ a ...)
     (string-trim (with-output-to-string (lambda () a ...)))]))

;; sugar around with-output-to-string to avoid the lambda
(define-syntax trim-output-or-false
  (syntax-rules ()
    [(_ a ...)
     (replace-empty-with #f (trim-output a ...))]))


(define (which val)
  (some~>
    (cond [(string? val) val]
          [(symbol? val) (to-string val)]
          [else (error (str "not sure what to do with " val))])
    find-executable-path
    path->string))

(define (run-command command . args)
  (define command2 (which command))
  (if command2
      (apply system* command2 (map to-string args))
      (error (str "not found on PATH: " command))))

(define (replace-empty-with else val)
  (cond [(string? val)  (if (non-empty-string? val) val else)]
        [(list? val)    (if (null? val) else val)]))

(define dev/null
  (make-output-port
   "dev/null"
   always-evt
   (lambda (_ start end _1 _2) (- end start))
   (lambda () (void))))

(define-syntax with-silent-stderr
  (syntax-rules ()
    [(_ a ...)
     (parameterize ([current-error-port dev/null])
       a ...)]))       

(define (basename path)
  (define-values (dir base _) (split-path path))
  base)

(define (concurrent/each fn coll [concurrency-limit 10000])
  (define coordinator (current-thread))
  (define (spawn arg)
    (thread
     (λ ()
       (fn arg)
       (thread-send coordinator (current-thread)))))
  (define (wait-for-finish running)
    (when (> running 0)
      (thread-receive)
      (wait-for-finish (- running 1))))
  (define (wait-for-spawn running coll)
    (if (null? coll)
        (wait-for-finish)
        (begin
          (thread-receive)   ; 1 out
          (spawn (car coll)) ; 1 in
          (wait-for-spawn running (cdr coll)))))
  (let spawning ([running 0] [coll coll])
    (cond [(null? coll) (wait-for-finish running)]
          [(< running concurrency-limit)
           (spawn (car coll))
           (spawning (+ running 1) (cdr coll))]
          [else (wait-for-spawn running coll)])))

;; (define-syntax 
;;   (syntax-rules ()
;;     [(_ a ...)
;;      (with-output-to-string (lambda () a ...))]))

;; boo! this doesn't work inside `class`, we will have to patch it :/
;; (define-syntax lazy
;;   (syntax-rules ()
;;     [(_ a b ..1)
;;      (begin
;;        (when (void? a) (set! a (begin b ..1)))
;;        a)]))
