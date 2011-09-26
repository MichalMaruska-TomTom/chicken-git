;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git.scm - libgit2 bindings for Chicken Scheme
;;
;; Copyright (c) 2011, Evan Hanson
;; See LICENSE for details
;;
;; Just a stub. Might be home to a nice API someday.

(module git
  (object-id object-type object-sha
   string->oid oid->string
   repository-open repository-path
   repository-empty? repository-bare?
   repository-reference repository-references reference-resolve
   reference-id reference-name reference-target reference-type
   repository-ref
   make-signature signature-name signature-email
   signature-time signature-time-offset
   commit-message commit-author)
  (import scheme files
    (only lolevel record->vector record-instance-type)
    (except chicken repository-path)
    (prefix git-lolevel git-))
  (require-library files lolevel git-lolevel)

(import-for-syntax srfi-13)
(define-for-syntax (+ . args)
  (string->symbol (apply string-append (map symbol->string args))))

(define-syntax define-git-record-type
  (er-macro-transformer
    (lambda (e r c)
      (let* ((name (caadr e))
             (attr (cdadr e))
             (free (cdddr e))
             (printer (caddr e))
             (make (+ 'make- name))
             (%make (+ '%make- name))
             (pointer (+ name '->pointer)))
        `(begin
           (define-record ,name >pointer) ; XXX this is lazy
           (define ,%make ,(+ 'make- name))
           (define-record-printer (,name ,name out)
             (display ,printer out))
           (define (,(+ 'pointer-> name) ptr)
             (let ((obj (,%make ptr)))
               ,(if (null? free)
                 'obj
                 `(set-finalizer! obj
                   (lambda (o) (,(caar free) (,pointer o)))))))
           ,@(map (lambda (attr)
                    (let ((getter (+ name '- attr)))
                      `(define (,getter obj)
                        ,(case attr
                          ((id) `(pointer->oid (,pointer obj)))
                          (else `(,(+ 'git- getter) (,pointer obj)))))))
                  attr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generics & OIDs

(define-git-record-type
  (oid)
  (format "#<oid ~S>" (oid->string oid 7)))

;; Internal.
(define (object->pointer obj)
  (vector-ref (record->vector obj) 1))

;; The type symbol of the given object
;; as reported by Git, or #f. Only for
;; Commit, Tree, Blob & Tag types.
(define (object-type obj)
  (let ((type (git-object-type (object->pointer obj))))
    (and (symbol? type) type)))

(define (object-id obj)
  (pointer->oid (git-object-id (object->pointer obj))))

(define (object-sha obj #!optional (len 40))
  (oid->string (object-id obj) len))

(define (oid->string id #!optional (len 40))
  (git-oid-to-string (min len 40) (oid->pointer id)))

(define (string->oid str)
  (pointer->oid (git-oid-fromstr str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repositories

(define-git-record-type
  (repository is-empty is-bare)
  (format "#<repository ~S>" (repository-path repository))
  (git-repository-free))

(define repository-empty? repository-is-empty)
(define repository-bare? repository-is-bare)

(define (repository-open path)
  ;; Try opening path as a "normal" repo first
  ;; (i.e. a workdir with a '.git' directory),
  ;; and if that doesn't work try as a "bare" repo.
  (let ((path (normalize-pathname path)))
    (pointer->repository
      (call-with-current-continuation
        (lambda (return)
          (condition-case
            (return (git-repository-open (string-append path "/.git")))
            (exn () (git-repository-open path))))))))

;; This one is kind of a catch-all.
;; Takes a string sha, an OID,
;; or a reference (direct or indirect),
;; returns a commit.
(define (repository-ref repo ref)
  (cond ((string? ref)
         (repository-ref repo (string->oid ref)))
        ((reference? ref)
         (repository-ref repo (reference-id ref)))
        ((oid? ref)
         (pointer->commit
           (git-commit-lookup
             (repository->pointer repo)
             (oid->pointer ref))))))

(define (repository-path repo #!optional (which 'path))
  (git-repository-path (repository->pointer repo) which))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; References

(define-git-record-type
  (reference oid target type name)
  (format "#<reference ~S>" (reference-name reference)))

(define (reference-id ref)
  (pointer->oid (reference-oid (reference-resolve ref))))

(define (reference-resolve ref)
  (pointer->reference (git-reference-resolve (reference->pointer ref))))

(define (repository-reference repo name)
  (pointer->reference
    (git-reference-lookup
      (repository->pointer repo)
      name)))

(define (repository-references repo #!optional (which 'listall))
  (map (lambda (ref) (repository-reference repo ref))
       (git-reference-listall (repository->pointer repo) which)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commits

(define-git-record-type
  (commit id message message-short time-offset tree-oid parentcount)
  (format "#<commit ~S>" (oid->string (commit-id commit) 7))
  (git-commit-close))

(define (commit-author cmt)    (pointer->signature (git-commit-author (commit->pointer cmt))))
(define (commit-committer cmt) (pointer->signature (git-commit-committer (commit->pointer cmt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signatures

(define-git-record-type
  (signature name email)
  (format "#<signature \"~A <~A>\">" (signature-name signature) (signature-email signature))
  (git-signature-free))

(define (signature-time sig) (git-time-time (git-signature-time (signature->pointer sig))))
(define (signature-time-offset sig) (git-time-offset (git-signature-time (signature->pointer sig))))

(define (make-signature name email #!optional time (offset 0))
  (pointer->signature
    (if time (git-signature-new name email time offset)
             (git-signature-now name email)))))
