#lang racket

(require (prefix-in git: jungle/git))
(require jungle/repos)
(require jungle/utils)
(require rackjure/threading)

(provide jungle-cli statuses fetch-all)
;; > bonfire new classic foo
;;
;; > bonfire hack on feature/coffee
;; Hacking on feature/coffee.
;; Warning: changes in 2 forks:
;;  * bonfire_boundaries
;;  * bonfire_things
;; > bonfire hack status
;; Changes found in 4 repositories
;; 
;; 
;; 
;; stdout/stderr
;;   * ignored
;;   * individually or combined
;;     * buffered in memory
;;     * buffered in memory, overflowing to disk beyond a certain size
;;     * redirected to a file
;;     * printed as it happens
;; stdin
;;   * close
;;   * run headless
;;   * attach new pty
;;   * attach current terminal

(define (is-fork? dir)
;  "Given a path to the root of a repository, checks if it sits within a forks directory."
  (eqv? "extensions" (basename (simplify-path (build-path dir 'up)))))

(define (find-forks-dir)
  (some~>
   (find-project-root)
   (send get-path-string)
   (build-path "extensions")))

(define (find-project-root)
;  "Assuming a standard bonfire repository layout, resolves to the root of the project repo."
  (when (not (git:repo? (current-directory)))
    (error "Not in a git repository!"))
  (define root (git:root (current-directory)))
  (if (is-fork? root)
       (repo (simplify-path (build-path root 'up 'up)))
       (repo root)))

(define (status repo)
  (let* ([name      (send repo get-name)]
         [is-git?   (send repo is-git?)]
         [head      (send repo get-head)]
         [branch    (send repo get-branch)]
         [tracking  (send repo get-tracking)]
         [staged    (or (send repo count-staged-changes) 0)]
         [unstaged  (or (send repo count-unstaged-changes) 0)]
         [untracked (or (send repo count-untracked) 0)]
         [behind    (or (send repo count-behind) 0)]
         [ahead     (or (send repo count-ahead) 0)]
         [note
          (cond
           [(not is-git?)  "not git"]
           [(not head)     "headless"]
           [(not branch)   "detached head"]
           [(not tracking) "not tracking"]
           [else
            (~>>
             (list
              (str "branch: " branch)
              (when (send repo in-rebase?) "rebasing")
              (when (send repo in-merge?)  "merging")
              (when (> ahead 0)     (str "ahead: "     ahead))
              (when (> behind 0)    (str "behind: "    behind))
              (when (> staged 0)    (str "staged: "    staged))
              (when (> unstaged 0)  (str "unstaged: "  unstaged))
              (when (> untracked 0) (str "untracked: " untracked)))
             (filter string?)
             ((lambda (notes) (string-join notes ", "))))])])
    (displayln (str name " (" note ")"))
    ))

(define (statuses)
  (define project (find-project-root))
  (status project)
  (for ([fork (directory-list (find-forks-dir) #:build? #t)]
        #:when (directory-exists? fork))
    (status (repo fork))))

(define (fetch-fork repo)
  (when (send repo get-tracking)
    (let ([output (git:fetch (send repo get-path-string))])
      (when (non-empty-string? output)
        (display (str (send repo get-name) ":" #\newline output))))))

(define (fetch-all)
  (define project (find-project-root))
  (concurrent/each fetch-fork
    (for/list ([fork (directory-list (find-forks-dir) #:build? #t)]
               #:when (directory-exists? fork))
      (repo fork))))

;; (deftask dev ()
;;  (all-for-one
;;   
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;

(define (help)
  (map displayln
  '(""
    "jungle: how to build a Bonfire (pre-alpha)"
    ""
    "usage: jungle [<global opts>] <task> [<args>] [<-- <task> [<args>]>]"
    ""
    "Global Options:"
    ""
    ""
    "Basic Tasks:"
    "  help   Help using jungle"
    ""
    "Maintainer tasks:"
    "  git    Repository management"
    ))
  (exit 1))

(define (jungle-cli)
  (match (vector->list (current-command-line-arguments))
    [(list) (help)]
    [(cons "git" rest)
     (match rest
       [(list) (help)]
       [(cons "status" rest) (statuses)]
       [(cons "fetch" rest)  (fetch-all)])]))

;; (define commands (hash-map))

;; (define (register-subcommand command handler)
;;   (when (not (string? command))
;;     (error "expected command string"))
;;   (when (not (or (procedure? handler)
;;                  (procedure-arity-includes? handler 1)))
;;     (error "expected handler procedure accepting 1 argument"))
;;   (hash-set commands command handler))



;; (define (parse-cli-args . args)
;;   (match args
;;     [(list) (parse-cli-args (vector->list args))]
;;     [(list (list)) (help)]
;;     [(list (cons h t))
;;      (match (hash-ref commands h #f)
;;        [#f (help)]
;;        [command (command t)])]))
     
; the purp
;
;
;
;
;
;
;
;




;; (define


;; (define (hash-subcommands
;; (define (overall)
  

;; (define (run . args)
;;   (match args
;;     [(list) 

;; (for/list ([f (in-directory)])
;;      f)  

;; (define (run-shell-command
;;          command
;;          #:stdin  [stdin  'close]  ; 'headless 'pty 'interactive
;;          #:stdout [stdout 'ignore] ; 'memory-buffer 'memory-or-file-buffer (and/c input-port? file-stream-port?)
;;          #:stderr [stderr 'ignore] ; 
         


;; scenarios for "just" running a command:
;; "shell out" - just return the stdout and exit code
;; "shell out quietly" - just return the return code
;; "shell out loudly" - print everything to the screen as it happens
;; "shell out interactively" - connect the user to the process

;; other scenarios:
;; * if the process exits with error, show merged stdout and stderr
;; * multiplex output: show merged output with a name prefix
;; * tabbed shells: like a tmux-style gui (one day maybe)

;; (struct proc (subprocess stdin-port stdout-port stderr-port) #:transparent)

;; (define (close-proc proc)

;; (define (SHELL)
;;   (environment-variables-ref
;;    (current-environment-variables)
;;    "SHELL"))

;; (define (shell-stdout command cwd)
;;   "run a shell command just for the output"
;;   (define-values (proc stdout stdin stderr)
;;     (parameterize ([current-directory cwd])
;;       (subprocess #f #f #f (SHELL) command)))
;;   (close-output-port stdin)
;;   (subprocess-wait proc)
;;   (let ((output (port->string stdout)))
;;     (close-input-port stdout)
;;     (close-input-port stderr)
;;     output))

;; (define summarise


;; (define (shell-output command)
;;   (let ((shell (environment-variables-ref (current-environment-variables) "SHELL")))
;;     (let-values (((sub stdout stdin stderr)
;;                   (apply subprocess #f #f #f shell command)))
;;      (close-output-port stdin)
;;      (subprocess-wait sub)
;;      (let ((stdout-str (port->string stdout))
;;            (stderr-str (port->string stderr)))
;;      (close-input-port stdout)
;;      (close-input-port stderr)
;; 



;; (define (proc-wait-status)
;;   (let ((


;; (define (shell-piped command)
;;   (let ((shell (environment-variables-ref (current-environment-variables) "SHELL")))
;;     (let-values (((sub stdout stdin stderr)
;;                   (apply subprocess #f #f #f (find-executable-path command) command)))
;;       (proc sub stdin stdout stderr))))

;; do we want this, actually?
;; (define (spawn-shell-interactive command)
;;   (let ((shell (environment-variables-ref (current-environment-variables) "SHELL")))
;;     (let-values (((sub stdout stdin stderr)
;;                   (apply subprocess #f #f #f (find-executable-path command) command)))
;;       (proc subprocess stdin stdout stderr))))

;;
;;
;;
;;
;;
;;
;;

;; (require syntax)

;;; (make-file-or-directory-link to path) → void?
;;; (current-directory)
;;; (current-directory path)
;;; (directory-exists? path)
;;; (delete-directory path) 


;; (define (pre-run)
;;   (for ([d '["forks" "extensions" "data/uploads" "priv/static/data"]])
;;     (make-directory* d))
;;   ;; (make-directory "extensions")
;;   ;; (make-direct
;;   )

;; (create-pointable "A.B.C" "a-ulid")
;; (



;; (require 'syntax)

;; (define-syntax-parser deftask
;;   [(deftask name:id)
;;    #'()])

;; (define-syntax (deftask stx)
;;   (syntax-parse stx
;;    [(deftask:id name:id body ...+)
;;     '()]))
;; (define (new-app &optional path)
;;   '())

;; (define-config

;; (define (start-shell command

;; (define interactive-shell

;; (define (start-process command . args)
;;   (let-values (((sub stdout stdin stderr)
;;                 (apply subprocess #f #f #f
;;                        (find-executable-path command)
;;                        args)))
;;     (let ((output (port->string stdout)))
;;       (subprocess-wait sub)
;;       (if (eqv? 0 (subprocess-status sub))
;;           output
;;           (error "Command failed:" (cons command args))))))  
