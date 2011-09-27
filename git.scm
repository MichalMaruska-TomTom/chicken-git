;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git.scm - libgit2 bindings for Chicken Scheme
;;
;; Copyright (c) 2011, Evan Hanson
;; See LICENSE for details
;;
;; Just a stub. Might be home to a nice API someday.

(module git
  (object-id object-type object-sha
   string->oid oid->string oid->path
   repository-open repository-path
   repository-empty? repository-bare?
   reference references reference-resolve reference-owner
   reference-id reference-name reference-target reference-type
   commit commit-id commit-message commit-message-short
   commit-time commit-time-offset commit-parentcount
   commit-author commit-committer commit-parent commit-tree
   blob blob-content blob-size
   index-open index-clear index-add index-read index-write
   index-entrycount index-entrycount-unmerged index-contains?
   index-ref index->list
   index-entry-dev index-entry-oid index-entry-ino index-entry-mode
   index-entry-uid index-entry-gid index-entry-size index-entry-stage
   index-entry-flags index-entry-extended index-entry-path
   index-entry-id index-entry-ctime index-entry-mtime
   odb-new odb-open odb-id odb-read odb-has-object?
   odb-object-id odb-object-data odb-object-size odb-object-type
   make-signature signature-name signature-email
   signature-time signature-time-offset
   tree tree-id tree-entrycount tree-ref tree->list
   tree-entry-id tree-entry-name tree-entry-attributes tree-entry-type
   tree-entry->object)
  (import scheme
    (only srfi-1 iota)
    (only files normalize-pathname make-pathname)
    (only lolevel record->vector record-instance-type)
    (except chicken repository-path)
    (prefix git-lolevel git-))
  (require-library srfi-1 files lolevel git-lolevel)

(define-for-syntax (s+ . args)
  (string->symbol (apply string-append (map symbol->string args))))

(define-syntax define-git-record-type
  (er-macro-transformer
    (lambda (e r c)
      (let* ((name (caadr e))
             (attr (cdadr e))
             (free (cdddr e))
             (printer (caddr e))
             (make (s+ 'make- name))
             (%make (s+ '%make- name))
             (->pointer (s+ name '->pointer))
             (pointer-> (s+ 'pointer-> name)))
        `(begin
           (define-record ,name >pointer) ; XXX this is lazy
           (define ,%make ,(s+ 'make- name))
           (define-record-printer (,name ,name out)
             (display ,printer out))
           (define (,pointer-> ptr)
             (and-let* ((ptr)
                        (obj (,%make ptr)))
               ,(if (null? free)
                 'obj
                 `(set-finalizer! obj
                   (lambda (o) (,(caar free) (,->pointer o)))))))
           ,@(map (lambda (attr)
                    (let ((getter (s+ name '- attr)))
                      `(define (,getter obj)
                        ,(case attr
                          ((id) `(pointer->oid (,->pointer obj)))
                          (else `(,(s+ 'git- getter) (,->pointer obj)))))))
                  attr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generics & OIDs

;; All git record types consist of a
;; single field, the object pointer.
(define (->pointer obj)
  (vector-ref (record->vector obj) 1))

(define-git-record-type
  (oid)
  (format "#<oid ~S>" (oid->string oid 7)))

;; The type symbol of the given object
;; as reported by Git, or #f. Only for
;; Commit, Tree, Blob & Tag types.
(define (object-type obj)
  (let ((type (git-object-type (->pointer obj))))
    (and (symbol? type) type)))

(define (object-id obj)
  (pointer->oid (git-object-id (->pointer obj))))

(define (object-sha obj #!optional (len 40))
  (oid->string (object-id obj) len))

(define (oid->string id #!optional (len 40))
  (git-oid-to-string (min len 40) (oid->pointer id)))

(define (string->oid str) (pointer->oid (git-oid-fromstr str)))
(define (oid->path oid)   (git-oid-pathfmt (oid->pointer oid)))

;; More needs to be done here to get
;; meaningful ids from e.g. tree/index
;; entries or odb-objects.
(define (->oid obj)
  (cond ((oid? obj) obj)
        ((string? obj) (string->oid obj))
        ((reference? obj) (reference-id obj))
        (else (object-id obj))))

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
            (return (git-repository-open (make-pathname path ".git")))
            (exn () (git-repository-open path))))))))

(define (repository-path repo #!optional (type 'path))
  (git-repository-path (repository->pointer repo) type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; References

(define-git-record-type
  (reference oid target type name)
  (format "#<reference ~S>" (reference-name reference)))

;; Follow symbolic references to get an OID.
;; Not sure if this is kosher, but it doesn't
;; look like symbolic references actually have
;; valid OIDs themselves, so we'll keep this
;; behavior for now...
(define (reference-id ref)      (pointer->oid (reference-oid (reference-resolve ref))))
(define (reference-owner ref)   (pointer->repository (git-reference-owner (reference->pointer ref))))
(define (reference-resolve ref) (pointer->reference (git-reference-resolve (reference->pointer ref))))

(define (reference repo name)
  (pointer->reference
    (git-reference-lookup
      (repository->pointer repo)
      name)))

(define (references repo #!optional (type 'listall))
  (map (lambda (ref) (reference repo ref))
       (git-reference-listall (repository->pointer repo) type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commits

(define-git-record-type
  (commit id message message-short time time-offset parentcount)
  (format "#<commit ~S>" (oid->string (->oid commit) 7))
  (git-commit-close))

(define (commit-tree cmt)      (pointer->tree (git-commit-tree (commit->pointer cmt))))
(define (commit-author cmt)    (pointer->signature (git-commit-author (commit->pointer cmt))))
(define (commit-committer cmt) (pointer->signature (git-commit-committer (commit->pointer cmt))))
(define (commit-parent cmt #!optional (n 0))
  (pointer->commit (git-commit-parent (commit->pointer cmt) n)))

(define (commit repo ref)
  (pointer->commit
    (git-commit-lookup
      (repository->pointer repo)
      (oid->pointer (->oid ref)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blobs

(define-git-record-type
  (blob rawsize rawcontent)
  (format "#<commit ~S>" (oid->string (->oid blob) 7))
  (git-blob-close))

(define blob-content blob-rawcontent)
(define blob-size blob-rawsize)

(define (blob repo ref)
  (pointer->blob
    (git-blob-lookup
      (repository->pointer repo)
      (oid->pointer (->oid ref)))))

;; TODO git_blob_create_fromfile git_blob_create_frombuffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Index

(define-git-record-type
  (index entrycount entrycount-unmerged read write clear)
  (format "#<index>")
  (git-index-free))

(define-git-record-type
  (index-entry dev oid ino mode uid gid size stage flags extended path)
  (format "#<index-entry ~S>" (index-entry-path index-entry))
  (git-odb-object-close))

(define (index-open path) (pointer->index (git-index-open path)))
(define (index-contains? ix path) (git-index-find (index->pointer ix) path))

(define (index-add ix path #!optional (stage 0))
  (git-index-add (index->pointer ix) stage))

(define (index-append ix path #!optional (stage 0))
  (git-index-append (index->pointer ix) stage))

(define (index-ref ix key #!optional (type 'merged))
  (case type
    ((merged)
     (pointer->index-entry (git-index-get (index->pointer ix) key)))
    ((unmerged)
     (pointer->index-entry
       (let ((ix* (index->pointer ix)))
         (cond ((string? key) (git-index-get-unmerged-bypath ix* key))
               ((number? key) (git-index-get-unmerged-byindex ix* key))
               (else (git-git-error 'index-ref "Invalid key" key))))))
    (else (git-git-error 'index-ref
                         "Invalid index type specifier"
                         type))))

(define (index->list ix #!optional (type 'merged))
  (map (lambda (i) (index-ref ix i type))
       (iota (index-entrycount ix))))

(define (index-entry-id entry) (pointer->oid (index-entry-oid entry)))

(define (index-entry-ctime entry)
  (git-index-time-seconds (git-index-entry-mtime (index-entry->pointer entry))))

(define (index-entry-mtime entry)
  (git-index-time-seconds (git-index-entry-mtime (index-entry->pointer entry))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ODB

(define-git-record-type
  (odb id)
  (format "#<odb>")
  (git-odb-close))

(define-git-record-type
  (odb-object id data size type)
  (format "#<odb-object>")
  (git-odb-object-close))

(define (odb-new) (pointer->odb (git-odb-new)))
(define (odb-open path) (pointer->odb (git-odb-open path)))
(define (odb-has-object? odb obj)
  (git-odb-exists (odb->pointer odb)
                  (oid->pointer (->oid obj))))

;; Not sure how to wrap this one
;; so it returns something meaningful.
(define (odb-read odb obj)
  (pointer->odb-object
    (git-odb-read (odb->pointer odb)
                  (oid->pointer (->oid obj)))))

;; TODO odb-write, odb-hash

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
             (git-signature-now name email))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags
;;
;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trees

(define-git-record-type
  (tree id entrycount)
  (format "#<tree ~S>" (oid->string (->oid tree) 7))
  (git-tree-close))

(define-git-record-type
  (tree-entry id name attributes type)
  (format "#<tree-entry ~S>" (tree-entry-name tree-entry)))

(define (tree repo ref)
  (pointer->tree
    (git-tree-lookup
      (repository->pointer repo)
      (oid->pointer (->oid ref)))))

(define (tree-ref tree key)
  (pointer->tree-entry
    (let ((tree* (tree->pointer tree)))
      (cond ((string? key) (git-tree-entry-byname tree* key))
            ((number? key) (git-tree-entry-byindex tree* key))
            (else (git-git-error 'tree-ref "Invalid key" key))))))

(define (tree-entry->object repo entry)
  (let* ((obj (git-tree-entry-2object
                (repository->pointer repo)
                (tree-entry->pointer entry))))
    (case (git-object-type obj)
      ((blob)   (pointer->blob obj))
      ((commit) (pointer->commit obj))
      ; ((tag) (pointer->tag obj)) TODO
      ((tree)   (pointer->tree obj)))))

(define (tree->list tree)
  (map (lambda (i) (tree-ref tree i))
       (iota (tree-entrycount tree)))))
