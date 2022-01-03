#lang racket

;; So I'd say maybe starting with:
;; - fetch and list remote branch names
;; - switch to a remote branch (across app and all forks that have a branch by that name, main for the rest). This may also edit deps.git to append the branch name where appropriate? 
;; - create a new branch across all forks with uncommitted changes (or prompt to select/deselect among them)

;;     fetch and list remote branch names (on all known remotes)

;;     switch to a remote branch (across app and all forks that have a branch by that name, main for the rest). If app does have a branch by that name, this also edits deps.git to append the branch name where appropriate

;;     create a new branch with the same name across all forks with uncommitted changes (or prompt to select/deselect among them)

;;     push newly created branch, with a prompt to enter either create a fork on your gh account or enter another remote (which should the be remembered for future) in case you don't have write access on bonfire-networks

(require rackjure/threading)
(require jungle/utils)
(require (prefix-in git: jungle/git))

(provide repo repo%)

(define (repo path)
  (new repo% [path path]))

(define repo%
  (class object%
    (init path)
    (define-values
      (abs-path git? branch head merge? rebase? changes tracking behind ahead)
      (values
       (simplify-path (path->complete-path path))
       (void)
       (void)
       (void)
       (void)
       (void)
       (void)
       (void)
       (void)
       (void)
       ))
    (define/public (get-path) abs-path)
    (define/public (get-path-string) (path->string abs-path))
    (define/public (get-name)
      (define-values (base ret _) (split-path (get-path-string)))
      (path->string ret))
    (define/public (is-git?)
      (when (void? git?)
        (set! git? (git:repo? (get-path-string))))
      git?)
    (define/public (get-head)
      (and (is-git?)
           (begin
             (when (void? head)
               (set! head (git:head (get-path-string))))
             head)))
    (define/public (in-merge?)
      (and (is-git?)
           (begin
             (when (void? merge?)
               (set! merge? (git:in-merge? (get-path-string))))
             merge?)))
    (define/public (in-rebase?)
      (and (is-git?)
           (begin
             (when (void? rebase?)
               (set! rebase? (git:in-rebase? (get-path-string))))
             rebase?)))
    (define/public (get-branch)
      (and (is-git?)
           (begin
             (when (void? branch)
               (set! branch (git:current-branch (get-path-string))))
             branch)))
    (define/public (get-changes)
      (and (get-head)
           (begin
             (when (void? changes)
               (set! changes (git:changes (get-path-string))))
             changes)))
    (define/public (get-staged-changes)
      (some~>>
       (get-changes)
       (filter (λ (c) (send c changed-stage?)))))
    (define/public (count-staged-changes)
      (some~>> (get-staged-changes) length))
    (define/public (get-unstaged-changes)
      (some~>>
       (get-changes)
       (filter (λ (c) (send c changed-work?)))))
    (define/public (count-unstaged-changes)
      (some~>> (get-unstaged-changes) length))
    (define/public (get-untracked)
      (some~>>
       (get-changes)
       (filter (λ (c) (send c untracked?)))))
    (define/public (count-untracked)
      (some~>> (get-untracked) length))
    (define/public (get-tracking)
      (and (get-branch)
           (begin
             (when (void? tracking)
               (set! tracking (git:tracking (get-path-string)))))
           tracking))
    (define/public (count-behind)
      (match (get-tracking)
        [#f (void)]
        [tracking
         (when (void? behind)
           (set! behind (git:behind (get-path-string) 'HEAD tracking)))
         behind]))
    (define/public (count-ahead)
      (match (get-tracking)
        [#f (void)]
        [tracking
         (when (void? ahead)
           (set! ahead (git:behind (get-path-string) tracking 'HEAD)))
         ahead]))
    (define/public (fetch)
      (git:fetch (get-path-string)))
    (super-new)))
