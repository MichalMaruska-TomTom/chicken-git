;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git.scm - libgit2 bindings for Chicken Scheme
;;
;; Copyright (c) 2011, Evan Hanson
;; See LICENSE for details
;;
;; Nowhere near complete. Don't use this.

(module git-lolevel *
(import scheme chicken foreign)
(use lolevel)

;; Errors are of condition properties (exn git)
(define (git-error loc msg . args)
  (signal (make-composite-condition
            (make-property-condition 'git)
            (make-property-condition 'exn 'location  loc
                                          'message   msg))))

;; Check retval, signal an error when nonzero.
(define-syntax guard-errors
  (syntax-rules ()
    ((_ <loc> <exp1> <expn> ...)
     (begin (when (< <exp1> 0) (git-error <loc> (lasterror)))
            (when (< <expn> 0) (git-error <loc> (lasterror))) ...))))

;; This could be compacted a bit more later, but for
;; right now we'll keep syntax-rules for readability.
(define-syntax define/allocate
  (syntax-rules ()
    ((_  <rtype> <name> (<cfun> (<atype> <arg>) ...))
     (define (<name> <arg> ...)
       (let-location ((object <rtype>))
         (guard-errors '<name>
           ((foreign-lambda int <cfun> (c-pointer <rtype>) <atype> ...)
              (location object) <arg> ...))
         object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git2.h

(foreign-declare "#include <git2.h>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types.h

(define-foreign-type commit     (c-pointer "git_commit"))
(define-foreign-type config     (c-pointer "git_config"))
(define-foreign-type index      (c-pointer "git_index"))
(define-foreign-type object     (c-pointer "git_object"))
(define-foreign-type odb        (c-pointer "git_odb"))
(define-foreign-type oid        (c-pointer "git_oid"))
(define-foreign-type otype      (c-pointer "git_otype"))
(define-foreign-type repository (c-pointer "git_repository"))
(define-foreign-type revwalk    (c-pointer "git_revwalk"))
(define-foreign-type signature  (c-pointer "git_signature"))
(define-foreign-type time       (c-pointer "git_time_t"))
(define-foreign-type tree       (c-pointer "git_tree"))

(define-foreign-type otype int)
(define obj/any       (foreign-value GIT_OBJ_ANY int))
(define obj/bad       (foreign-value GIT_OBJ_BAD int))
(define obj/ext1      (foreign-value GIT_OBJ__EXT1 int))
(define obj/commit    (foreign-value GIT_OBJ_COMMIT int))
(define obj/tree      (foreign-value GIT_OBJ_TREE int))
(define obj/blob      (foreign-value GIT_OBJ_BLOB int))
(define obj/tag       (foreign-value GIT_OBJ_TAG int))
(define obj/ext2      (foreign-value GIT_OBJ__EXT2 int))
(define obj/ofs-delta (foreign-value GIT_OBJ_OFS_DELTA int))
(define obj/ref-delta (foreign-value GIT_OBJ_REF_DELTA int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commit.h

(define/allocate commit commit-lookup
  (git_commit_lookup (repository repo) (oid id)))

(define commit-close         (foreign-lambda void git_commit_close commit))
(define commit-id            (foreign-lambda oid git_commit_id commit))
(define commit-message-short (foreign-lambda c-string git_commit_message_short commit))
(define commit-message       (foreign-lambda c-string git_commit_message commit))
(define commit-time          (foreign-lambda time git_commit_time commit))
(define commit-time-offset   (foreign-lambda int git_commit_time_offset commit))
(define commit-committer     (foreign-lambda signature git_commit_committer commit))
(define commit-author        (foreign-lambda signature git_commit_author commit))

(define/allocate tree commit-tree
  (git_commit_tree (commit cmt)))

(define commit-tree-oid      (foreign-lambda oid git_commit_tree_oid commit))
(define commit-parentcount   (foreign-lambda unsigned-int git_commit_parentcount commit))

(define/allocate commit commit-parent
  (git_commit_parent (commit cmt) (unsigned-int n)))

(define commit-parent-oid    (foreign-lambda oid git_commit_parent_oid commit unsigned-int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common.h

(define (libgit2-version)
  (let-location ((major int) (minor int) (rev int))
    ((foreign-lambda void git_libgit2_version (c-pointer int) (c-pointer int) (c-pointer int))
       (location major)
       (location minor)
       (location rev))
    (values major minor rev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; errors.h

(define lasterror (foreign-lambda c-string git_lasterror))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; object.h

(define/allocate object object-lookup 
  (git_object_lookup (repository repo) (oid id) (otype type)))

(define object-id (foreign-lambda oid git_object_id object))
(define object-close (foreign-lambda void git_object_close object))
(define object-owner (foreign-lambda repository git_object_owner object))
(define object-type (foreign-lambda otype git_object_type object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; oid.h

(define (oid-fromstr str)
  (let ((id (allocate (foreign-value "sizeof(git_oid)" int))))
    ((foreign-lambda int git_oid_fromstr oid c-string) id str)
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; repository.h

(define-foreign-type repository-pathid int)
(define path/path    (foreign-value GIT_REPO_PATH int))
(define path/index   (foreign-value GIT_REPO_PATH_INDEX int))
(define path/odb     (foreign-value GIT_REPO_PATH_ODB int))
(define path/workdir (foreign-value GIT_REPO_PATH_WORKDIR int))

(define/allocate repository repository-open
  (git_repository_open (c-string path)))

(define repository-database
  (foreign-lambda odb git_repository_database repository))

(define/allocate index repository-index
  (git_repository_index (repository repo)))

(define repository-free
  (foreign-lambda void git_repository_free repository))

(define repository-is-empty
  (foreign-lambda bool git_repository_is_empty repository))

(define repository-is-bare
  (foreign-lambda bool git_repository_is_bare repository))

(define (repository-path repo #!optional (pathid path/path))
  ((foreign-lambda c-string git_repository_path repository repository-pathid) repo pathid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; revwalk.h

(define/allocate revwalk revwalk-new
  (git_revwalk_new (repository repo)))

(define revwalk-push
  (foreign-lambda int git_revwalk_push revwalk oid))

(define (revwalk-next walker)
  (let ((id (allocate (foreign-value "sizeof(git_oid)" int))))
    ((foreign-lambda int git_revwalk_next oid revwalk) id walker)
    id))

(define revwalk-free
  (foreign-lambda void git_revwalk_free revwalk)))
