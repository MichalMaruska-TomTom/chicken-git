;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git.scm - libgit2 bindings for Chicken Scheme
;;
;; Copyright (c) 2011, Evan Hanson
;; See LICENSE for details
;;
;; Nowhere near complete. Don't use this.

(module git *
(import scheme chicken foreign)
(use lolevel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git2.h

(foreign-declare "#include <git2.h>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types.h

(define-foreign-type commit (c-pointer "git_commit"))
(define-foreign-type config (c-pointer "git_config"))
(define-foreign-type index (c-pointer "git_index"))
(define-foreign-type odb (c-pointer "git_odb"))
(define-foreign-type oid (c-pointer "git_oid"))
(define-foreign-type repository (c-pointer "git_repository"))
(define-foreign-type revwalk (c-pointer "git_revwalk"))
(define-foreign-type signature (c-pointer "git_signature"))
(define-foreign-type time (c-pointer "git_time_t"))

(define-foreign-type repository-pathid int)
(define path/path    (foreign-value GIT_REPO_PATH int))
(define path/index   (foreign-value GIT_REPO_PATH_INDEX int))
(define path/odb     (foreign-value GIT_REPO_PATH_ODB int))
(define path/workdir (foreign-value GIT_REPO_PATH_WORKDIR int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commit.h

(define (commit-lookup repo id)
  (let-location ((cmt commit))
    ((foreign-lambda int git_commit_lookup (c-pointer commit) repository oid)
       (location cmt) repo id)
    cmt))

(define commit-id
  (foreign-lambda oid git_commit_id commit))

(define commit-message-short
  (foreign-lambda c-string* git_commit_message_short commit))

(define commit-message
  (foreign-lambda c-string* git_commit_message commit))

(define commit-time
  (foreign-lambda time git_commit_time commit))

(define commit-committer
  (foreign-lambda signature git_commit_committer commit))

(define commit-author
  (foreign-lambda signature git_commit_author commit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; oid.h

(define (oid-fromstr str)
  (let ((id (allocate (foreign-value "sizeof(git_oid)" int))))
    ((foreign-lambda int git_oid_fromstr oid c-string) id str)
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; repository.h

(define (repository-open path)
  (let-location ((repo repository))
    ((foreign-lambda int git_repository_open (c-pointer repository) c-string)
       (location repo) path)
    repo))

(define repository-database
  (foreign-lambda odb git_repository_database repository))

(define (repository-index repo)
  (let-location ((ix index))
    ((foreign-lambda int git_repository_index (c-pointer index) repository)
       (location ix) repo)
    ix))

(define repository-free
  (foreign-lambda void git_repository_free repository))

(define repository-is-empty
  (foreign-lambda bool git_repository_is_empty repository))

(define repository-is-bare
  (foreign-lambda bool git_repository_is_bare repository))

(define (repository-path repo #!optional (pathid path/path))
  ((foreign-lambda c-string* git_repository_path repository repository-pathid) repo pathid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; revwalk.h

(define (revwalk-new repo)
  (let-location ((walker revwalk))
    ((foreign-lambda int git_revwalk_new (c-pointer revwalk) repository) (location walker) repo)
    walker))

(define revwalk-push
  (foreign-lambda int git_revwalk_push revwalk oid))

(define (revwalk-next walker)
  (let ((id (allocate (foreign-value "sizeof(git_oid)" int))))
    ((foreign-lambda int git_revwalk_next oid revwalk) id walker)
    id))

(define revwalk-free
  (foreign-lambda int git_revwalk_free revwalk)))
