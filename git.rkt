#lang racket

(require rackjure/threading)
(require jungle/utils)

;; git statuses:
;; ' ' = unmodified
;; M = modified
;; T = file type changed (regular file, symbolic link or submodule)
;; A = added
;; D = deleted
;; R = renamed
;; C = copied (if config option status.renames is set to "copies")
;; U = updated but unmerged

(provide git
         repo? in-rebase? in-merge?
         fetch
         change% changes
         root current-branch head tracking behind)

(define (git . args)
  (apply run-command 'git args))

(define (fetch path . args)
  (string-output
   (apply git '-C path 'fetch '--prune args)))

(define change%
  (class object%
    (init-field path)
    (init-field stage)
    (init-field work)
    (define/public (changed-stage?)
      (not (member stage '(#\space #\?)))) 
    (define/public (changed-work?)
      (not (member work '(#\space #\?)))) 
    (define/public (untracked?)
      (eqv? stage #\?))
    (super-new)))
  
(define (repo? path)
  (git '-C path 'rev-parse))

;;; true if the repository is currently in a rebase.
(define (in-rebase? path)
  (some~>>
   (trim-output-or-false
    (git '-C path 'rev-parse '--git-path 'rebase-merge))
   directory-exists?))
          
;;; true if the repository is currently in a merge.
(define (in-merge? path)
  (git '-C path 'rev-parse '-q '--verify 'MERGE_HEAD))

(define (root path)
  (trim-output-or-false
   (git '-C path 'rev-parse '--show-toplevel)))

(define (parse-change line)
  (new change%
       [path (substring line 3)]
       [stage (string-ref line 0)]
       [work (string-ref line 1)]))

;;; Returns a list of `change`
(define (changes path)
  (string-input (string-output (git '-C path 'status '--porcelain))
    (for/list ([line (in-lines)] #:when (> (string-length line) 3))
      (parse-change line))))

;;; For a detached head, this will be empty. requires git 2.22+ (2015)
(define (current-branch path)
  (trim-output-or-false
   (git '-C path 'branch '--show-current)))

(define (head path)
  (with-silent-stderr
   (trim-output-or-false
    (git '-C path 'rev-parse '--abbrev-ref "@"))))

(define (tracking path [branch ""])
  (with-silent-stderr
   (trim-output-or-false
    (git '-C path 'rev-parse '--abbrev-ref (str branch "@{u}")))))

(define (behind path from to)
  (with-silent-stderr
   (some~>>
    (trim-output-or-false
     (git '-C path 'rev-list (str from ".." to) '--count))
    string->number)))
