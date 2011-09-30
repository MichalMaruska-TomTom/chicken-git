;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git.scm - libgit2 bindings for Chicken Scheme
;;
;; Copyright (c) 2011, Evan Hanson
;; See LICENSE for details
;;
;; Approaching usability.
;; Needs more testing.

(module git
  (object-id object-type object-sha
   string->oid oid->string oid->path
   repository-open repository-path repository-ref repository-empty? repository-bare?
   reference references create-reference reference-resolve reference-owner
   reference-id reference-name reference-target reference-type
   commit commits create-commit commit-id commit-message commit-message-short
   commit-time commit-time-offset commit-parentcount
   commit-author commit-committer commit-parent commit-tree
   blob* blob*-content blob*-size
   index-open index-find index-ref index->list
   index-clear index-add index-remove index-read index-write
   index-entrycount index-entrycount-unmerged
   index-entry-dev index-entry-oid index-entry-ino index-entry-mode
   index-entry-uid index-entry-gid index-entry-size index-entry-stage
   index-entry-flags index-entry-extended index-entry-path
   index-entry-id index-entry-ctime index-entry-mtime
   odb-new odb-open odb-has-object? odb-read odb-write odb-hash
   odb-object-id odb-object-data odb-object-size odb-object-type
   make-signature signature-name signature-email signature-time signature-time-offset
   tag tags create-tag tag-id tag-type tag-name tag-message tag-delete tag-tagger tag-target
   tree create-tree tree-id tree-entrycount tree-ref tree->list
   tree-entry-id tree-entry-name tree-entry-attributes tree-entry-type
   tree-entry->object)
  (import scheme
    (only srfi-1 iota)
    (only files normalize-pathname make-pathname)
    (except chicken repository-path)
    (prefix git-lolevel git-)
    (only lolevel record->vector record-instance-type number-of-bytes move-memory!))
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
(define (object->pointer obj)
  (vector-ref (record->vector obj) 1))

(define-git-record-type
  (oid)
  (format "#<oid ~S>" (oid->string oid 7)))

;; The type symbol of the given object
;; as reported by Git, or #f. Only for
;; Commit, Tree, Blob & Tag types.
(define (object-type obj)
  (let ((type (git-object-type (object->pointer obj))))
    (and (symbol? type) type)))

(define (object-id obj)
  (pointer->oid (git-object-id (object->pointer obj))))

(define (object-sha obj #!optional (len 40))
  (oid->string (->oid obj) len))

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

;; Try to take obj as a reference.
(define (->reference obj)
  (cond ((string? obj) obj)
        ((reference? obj) (reference-name obj))
        (else (git-git-error '->reference
                             "Not a valid reference"
                             obj))))

(define (pointer->object ptr)
  (case (git-object-type ptr)
    ((blob)   (pointer->blob* ptr))
    ((commit) (pointer->commit ptr))
    ((tag)    (pointer->tag ptr))
    ((tree)   (pointer->tree ptr))))

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

(define (repository-ref repo ref #!optional (type 'any))
  (pointer->object
    (git-object-lookup
      (repository->pointer repo)
      (oid->pointer (->oid ref))
      type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; References

(define-git-record-type
  (reference oid target type name delete)
  (format "#<reference ~S>" (reference-name reference)))

;; Follow symbolic references to get an OID.
;; Not sure if this is kosher, but it doesn't
;; look like symbolic references actually have
;; valid OIDs themselves, so we'll keep this
;; behavior for now...
(define (reference-id ref)      (pointer->oid (reference-oid (reference-resolve ref))))
(define (reference-owner ref)   (pointer->repository (git-reference-owner (reference->pointer ref))))
(define (reference-resolve ref) (pointer->reference (git-reference-resolve (reference->pointer ref))))

(define (pack-references repo) (git-reference-packall (repository->pointer repo)))

(define (reference repo name)
  (pointer->reference
    (git-reference-lookup
      (repository->pointer repo)
      name)))

(define (references repo #!optional (type 'listall))
  (map (lambda (ref) (reference repo ref))
       (git-reference-listall (repository->pointer repo) type)))

;; This will overwrite existing references.
;; There should probably be a flag or something
;; to disable this. TODO, maybe.
(define (create-reference repo name target #!optional symbolic?)
  (let ((repo* (repository->pointer repo)))
    (pointer->reference
      (if (not symbolic?)
        ;; Direct references are created by OID.
        (git-reference-create-oid-f repo* name (oid->pointer (->oid target)))
        ;; Symbolic references require the
        ;; target to be given by a string.
        (if (string? target)
          (git-reference-create-symbolic-f repo* name target)
          (git-git-error 'create-reference
                         "Symbolic reference target must be a string"
                         target))))))

(define (reference-target-set ref target)
  (git-reference-set-target (reference->pointer ref) target))

(define (reference-id-set ref id)
  (git-reference-set-oid (reference->pointer ref)
                         (oid->pointer (->oid id))))

(define (reference-rename ref name)
  (git-reference-rename-f (reference->pointer ref) name))

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

(define (commits repo #!key initial (hide '()) (sort 'none))
  (map pointer->commit
       (let ((walker (git-revwalk-new (repository->pointer repo))))
         ;; Sort mode, one of '(none topo time rev)
         (git-revwalk-sorting walker sort)
         ;; Set hidden commits. These exclude
         ;; full branches from the traversal,
         ;; rather than just the commits.
         (for-each (lambda (ptr) (git-revwalk-hide walker ptr))
                   (map oid->pointer (map ->oid hide)))
         ;; Set initial revision.
         ;; Use HEAD if none is given (allowed? safe?).
         ;; HEAD should always exist if there's at least one commit, so...
         (git-revwalk-push walker (oid->pointer (->oid (or initial (reference repo "HEAD")))))
         (let lp ((acc '()))
           (condition-case
             (lp (cons (git-revwalk-next walker) acc))
             ((git) acc))))))

(define (create-commit repo #!key tree parents message author (committer author) (reference #f))
  (commit repo
    (pointer->oid
      (apply git-commit-create
        (repository->pointer repo)
        (and reference (->reference reference))
        (signature->pointer author)
        (signature->pointer committer)
        message
        (tree->pointer tree)
        (map commit->pointer parents)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blobs

(define-git-record-type
  (blob* rawsize rawcontent)
  (format "#<blob* ~S>" (oid->string (->oid blob*) 7))
  (git-blob*-close))

(define blob*-size blob*-rawsize)
(define (blob*-content blob*)
  (let* ((size (blob*-size blob*))
         (dest (make-blob size)))
    (move-memory! (blob*-rawcontent blob*) dest size)
    dest))

(define (blob* repo ref)
  (pointer->blob*
    (git-blob*-lookup
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

(define (index-open loc)
  (pointer->index
    (cond ((string? loc) (git-index-open loc))
          ((repository? loc) (git-repository-index (repository->pointer loc)))
          (else (git-git-error 'index-open
                               "Invalid index location"
                               loc)))))

(define (index-find ix path)
  (and-let* ((pos (git-index-find (index->pointer ix) path))
             ((<= 0 pos)))
    pos))

(define (index-add ix path #!optional (stage 0))
  (git-index-add (index->pointer ix) path stage))

(define (index-remove ix ref)
  (let ((ix* (index->pointer ix)))
    (cond ((string? ref)
           (and-let* ((pos (index-find ix ref)))
             (git-index-remove ix* pos)))
          ((number? ref)
           (git-index-remove ix* ref))
          (else #f))))

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
       (iota (case type
               ((merged) (index-entrycount ix))
               ((unmerged) (index-entrycount-unmerged ix))
               (else (git-git-error 'index-ref
                         "Invalid index type specifier"
                         type))))))

(define (index-entry-id entry) (pointer->oid (index-entry-oid entry)))

(define (index-entry-ctime entry)
  (git-index-time-seconds (git-index-entry-mtime (index-entry->pointer entry))))

(define (index-entry-mtime entry)
  (git-index-time-seconds (git-index-entry-mtime (index-entry->pointer entry))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ODB

(define-git-record-type
  (odb)
  (format "#<odb>")
  (git-odb-close))

(define-git-record-type
  (odb-object id size type)
  (format "#<odb-object>")
  (git-odb-object-close))

(define (odb-new) (pointer->odb (git-odb-new)))

(define (odb-has-object? odb obj)
  (git-odb-exists (odb->pointer odb)
                  (oid->pointer (->oid obj))))

(define (odb-open loc)
  (pointer->odb
    (cond ((string? loc) (git-odb-open loc))
          ((repository? loc) (git-odb-open (repository-path loc 'odb)))
          (else (git-git-error 'odb-open
                               "Invalid odb location"
                               loc)))))

(define (odb-read odb obj)
  (pointer->odb-object
    (git-odb-read (odb->pointer odb)
                  (oid->pointer (->oid obj)))))

(define (odb-write odb data #!optional (type 'blob))
  (pointer->oid (git-odb-write (odb->pointer odb) data (number-of-bytes data) type)))

(define (odb-hash data #!optional (type 'blob))
  (pointer->oid (git-odb-hash data (number-of-bytes data) type)))

(define (odb-object-data obj)
  (let* ((obj* (odb-object->pointer obj))
         (data (git-odb-object-data obj*))
         (size (odb-object-size obj))
         (dest (make-blob size)))
    (move-memory! data dest size)
    dest))

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

(define-git-record-type
  (tag id type name message)
  (format "#<tag ~S>" (tag-name tag))
  (git-tag-close))

(define (tag repo name)
  (pointer->tag
    (git-tag-lookup
      (repository->pointer repo)
      (oid->pointer
        (reference-id
          (reference repo name))))))

(define (tags repo)
  (map (lambda (t) (tag repo t))
       (git-tag-list (repository->pointer repo))))

(define (tag-tagger tag) (pointer->signature (git-tag-tagger (tag->pointer tag))))
(define (tag-target tag) (pointer->object (git-tag-target (tag->pointer tag))))
(define (tag-delete tag)
  (git-tag-delete
    (git-object-owner (tag->pointer tag))
    (tag-name tag)))

(define (create-tag repo #!key target name message tagger)
  (repository-ref repo
    (pointer->oid
      (git-tag-create
        (repository->pointer repo)
        name
        (object->pointer target)
        (signature->pointer tagger)
        message))))

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

;; I'd like to do away with the repo argument
;; here and use git_object_owner to figure it
;; out automatically, but when a repository
;; created that way is freed it invalidates
;; any objects it has spawned (which would
;; include the object returned here). We could
;; do our own refcounting to delay freeing the
;; repo but that sounds like hell on earth.
;; We'll just leave the argument for now.
(define (tree-entry->object repo entry)
  (pointer->object
    (git-tree-entry-2object
      (repository->pointer repo)
      (tree-entry->pointer entry))))

(define (create-tree repo ix)
  (tree repo (pointer->oid (git-tree-create-fromindex (index->pointer ix)))))

(define (tree->list tree)
  (map (lambda (i) (tree-ref tree i))
       (iota (tree-entrycount tree)))))
