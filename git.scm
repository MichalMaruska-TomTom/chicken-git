;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git.scm - libgit2 bindings for Chicken Scheme
;;
;; Copyright (c) 2011, Evan Hanson
;; See LICENSE for details
;;
;; Approaching usability.

(module git
  (object-id object-type object-sha
   string->oid oid->string oid->path oid?
   repository? create-repository repository-open
   repository-path repository-ref repository-empty? repository-bare?
   reference? reference references create-reference reference-resolve
   reference-id reference-name reference-target reference-type
   reference-target-set reference-rename reference-delete
   commit? commit commits commits-fold create-commit commit-id
   commit-message commit-message-encoding
   commit-time commit-time-offset commit-parentcount
   commit-author commit-committer commit-parent commit-tree commit-tree-id
   blob*? blob* blob*-content blob*-size
   index? index-open index-find index-ref index->list
   index-clear index-add index-remove index-read index-write
   index-entrycount index-entrycount-unmerged
   index-entry? index-entry-dev index-entry-ino index-entry-mode
   index-entry-uid index-entry-gid index-entry-size index-entry-stage
   index-entry-flags index-entry-extended index-entry-path
   index-entry-id index-entry-ctime index-entry-mtime
   odb? odb-open odb-has-object? odb-read odb-write odb-hash
   odb-object? odb-object-id odb-object-data odb-object-size odb-object-type
   signature? make-signature signature-name signature-email signature-time signature-time-offset
   tag? tag tags create-tag tag-id tag-type tag-name tag-message tag-delete tag-tagger tag-target
   tree? tree create-tree tree-id tree-entrycount tree-ref tree->list tree-subtree
   tree-entry? tree-entry-id tree-entry-name tree-entry-attributes tree-entry-type tree-entry->object
   make-tree-builder tree-builder-ref tree-builder-insert tree-builder-remove tree-builder-clear tree-builder-write
   tree-diff tree-diff-old-attr tree-diff-new-attr tree-diff-old-id tree-diff-new-id tree-diff-path tree-diff-status
   config? config-open config-path config-get config-set config-unset
   file-status file-ignored?)
  (import scheme
    (only srfi-1 iota)
    (only extras format)
    (only posix current-directory)
    (only files normalize-pathname make-pathname pathname-strip-directory)
    (except chicken repository-path)
    (prefix git-lolevel git-)
    (only lolevel record->vector number-of-bytes move-memory! tag-pointer pointer-tag))
  (require-library srfi-1 extras posix files lolevel git-lolevel)

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
                          ((id) `(pointer->oid (,(s+ 'git- getter) (,->pointer obj))))
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
  (git-oid-tostr (min len 40) (oid->pointer id)))

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

;; Try to convert obj to a reference name.
(define (->reference-name obj)
  (cond ((string? obj) obj)
        ((reference? obj) (reference-name obj))
        (else (git-git-error '->reference-name
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
  (repository is-empty is-bare workdir)
  (format "#<repository ~S>" (repository-path repository))
  (git-repository-free))

(define repository-empty? repository-is-empty)
(define repository-bare? repository-is-bare)

(define (repository-open #!optional (path (current-directory)))
  ;; Try opening path as a "normal" repo first
  ;; (i.e. a workdir with a '.git' directory),
  ;; and if that doesn't work try as a "bare" repo.
  (let ((path (normalize-pathname path)))
    (pointer->repository
      (condition-case
        (git-repository-open (make-pathname path ".git"))
        ((git) (git-repository-open path))))))

(define (repository-path repo #!optional (type 'path))
  (case type
    ((path) (git-repository-path (repository->pointer repo)))
    ((workdir) (git-repository-workdir (repository->pointer repo)))
    (else (git-git-error 'repository-path "Invalid path type specifier" type))))

(define (repository-ref repo ref)
  (call-with-current-continuation
    (lambda (return)
      (for-each
        (lambda (try)
          (condition-case
            (return (try repo ref))
            ((git) 'continue)))
        (list commit blob* tag tree (lambda _ #f))))))

  ;; git-object-lookup fails on 32-bit bindings.
  ;; TODO figure out why so we can use libgit2's
  ;; polymorphic lookup (which will be faster than
  ;; the hand-rolled hack above).
  ; (condition-case
  ;   (pointer->object
  ;     (git-object-lookup
  ;       (repository->pointer repo)
  ;       (oid->pointer (->oid ref))
  ;       type))
  ;   ((git) #f)))

(define (create-repository #!optional (path (current-directory)) bare?)
  (pointer->repository (git-repository-init path bare?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; References

(define-git-record-type
  (reference oid type name reload is-packed)
  (format "#<reference ~S>" (reference-name reference)))

;; When a reference is deleted with git_reference_delete
;; its pointer is invalidated, so when a reference record
;; is GC'd we have to make sure it wasn't already freed by
;; a deletion during its lifetime. Here, we set a GC tag
;; on the pointer, which reference-delete will remove.
(define pointer->reference
  (let ((pointer->reference* pointer->reference))
    (lambda (ptr)
      (set-finalizer! (pointer->reference* (tag-pointer ptr 'gc))
        (lambda (ref)
          (let ((ref* (reference->pointer ref)))
            (if (pointer-tag ref*)
              (git-reference-free ref*))))))))

(define (reference-delete ref)
  (let ((ref* (reference->pointer ref)))
    (reference->pointer-set! ref (tag-pointer ref* #f))
    (git-reference-delete ref*)))

;; Follow symbolic references to get an OID.
;; Not sure if this is kosher, but it doesn't
;; look like symbolic references actually have
;; valid OIDs themselves, so we'll keep this
;; behavior for now...
(define (reference-id ref)      (pointer->oid (reference-oid (reference-resolve ref))))
(define (reference-resolve ref) (pointer->reference (git-reference-resolve (reference->pointer ref))))

(define reference-is-packed? reference-is-packed)

(define (pack-references repo) (git-reference-packall (repository->pointer repo)))

(define (reference-target ref)
  (if (not (eq? (reference-type ref) 'symbolic))
    (git-git-error 'reference-target "Reference must be symbolic" ref)
    (git-reference-target (reference->pointer ref))))

(define (reference repo name)
  (pointer->reference
    (git-reference-lookup
      (repository->pointer repo)
      name)))

(define (references repo #!optional (type 'listall))
  (map (lambda (ref) (reference repo ref))
       (git-reference-listall (repository->pointer repo) type)))

(define (create-reference repo #!key name target symbolic force)
  (let ((repo* (repository->pointer repo)))
    (pointer->reference
      (if (not symbolic)
        ;; Direct references are created by OID.
        (git-reference-create-oid repo* name (oid->pointer (->oid target)) force)
        ;; Symbolic references require the
        ;; target to be given by a string.
        (git-reference-create-symbolic repo* name (->reference-name target) force)))))

(define (reference-target-set ref target)
  (let ((ref* (reference->pointer ref)))
    (cond ((reference? target)
           (git-reference-set-target ref* (->reference-name target)))
          ((commit? target)
           (git-reference-set-oid ref* (oid->pointer (commit-id target))))
          ((oid? target)
           (git-reference-set-oid ref* (oid->pointer target)))
          ((string? target)
           (condition-case
             (git-reference-set-oid ref* (oid->pointer (string->oid target)))
             ((git) (git-reference-set-target ref* target))))
          (else (git-git-error
                  'reference-target-set
                  "Invalid reference target"
                  target)))))

(define (reference-rename ref name #!optional force)
  (git-reference-rename (reference->pointer ref) name force))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commits

(define-git-record-type
  (commit id message message-encoding time time-offset parentcount)
  (format "#<commit ~S>" (oid->string (->oid commit) 7))
  (git-commit-free))

(define (commit-tree cmt)      (pointer->tree (git-commit-tree (commit->pointer cmt))))
(define (commit-tree-id cmt)   (pointer->oid  (git-commit-tree-oid (commit->pointer cmt))))
(define (commit-author cmt)    (pointer->signature (git-commit-author (commit->pointer cmt))))
(define (commit-committer cmt) (pointer->signature (git-commit-committer (commit->pointer cmt))))
(define (commit-parent cmt #!optional (n 0))
  (condition-case
    (pointer->commit (git-commit-parent (commit->pointer cmt) n))
    ((git) #f)))

(define (commit repo ref)
  (pointer->commit
    (git-commit-lookup
      (repository->pointer repo)
      (oid->pointer (->oid ref)))))

(define (commits-fold kons knil repo #!key initial (hide '()) (sort 'none))
  (call-with-current-continuation
    (lambda (return)
      (let ((walker #f))
        (dynamic-wind
          (lambda ()
            (set! walker
              (git-revwalk-new (repository->pointer repo))))
          (lambda ()
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
            (git-revwalk-push walker
              (condition-case
                (oid->pointer
                  (->oid (or initial (reference repo "HEAD"))))
                ((git) (return '()))))
            (let lp ((state knil))
              (condition-case
                (lp (kons (commit repo (pointer->oid (git-revwalk-next walker))) state))
                ((git) state))))
          (lambda ()
            (git-revwalk-free walker)))))))

(define (commits repo . rest)
  (apply commits-fold cons '() repo rest))

(define (create-commit repo #!key tree message (parents '()) author (committer author) (reference #f))
  (commit repo
    (pointer->oid
      (apply git-commit-create
        (repository->pointer repo)
        (and reference (->reference-name reference))
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
  (git-blob*-free))

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
  "#<index>"
  (git-index-free))

(define-git-record-type
  (index-entry dev oid ino mode uid gid size stage flags extended path)
  (format "#<index-entry ~S>" (index-entry-path index-entry)))

(define (index-open loc)
  (pointer->index
    (cond ((string? loc) (git-index-open loc))
          ((repository? loc) (git-repository-index (repository->pointer loc)))
          (else (git-git-error 'index-open
                               "Invalid index location"
                               loc)))))

(define (index-find ix path)
  (if (not (string? path))
    (git-git-error 'index-find "String required" path)
    (and-let* ((pos (git-index-find (index->pointer ix) path))
               ((<= 0 pos)))
      pos)))

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
  (let ((ix* (index->pointer ix)))
    (case type
      ((merged)
       (pointer->index-entry
         (cond ((number? key) (git-index-get ix* key))
               ((string? key)
                (and-let* ((i (index-find ix key)))
                  (git-index-get ix* i)))
               (else (git-git-error 'index-ref "Invalid key" key)))))
      ((unmerged)
       (pointer->index-entry
         (cond ((string? key) (git-index-get-unmerged-bypath ix* key))
               ((number? key) (git-index-get-unmerged-byindex ix* key))
               (else (git-git-error 'index-ref "Invalid key" key)))))
      (else (git-git-error 'index-ref
                           "Invalid index type specifier"
                           type)))))

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
;; Status

(define (file-status repo path)
  (git-status-file (repository->pointer repo) path))

(define (file-ignored? repo path)
  (git-status-should-ignore (repository->pointer repo) path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ODB

(define-git-record-type
  (odb)
  "#<odb>"
  (git-odb-free))

(define-git-record-type
  (odb-object id size type)
  "#<odb-object>"
  (git-odb-object-free))

(define (odb-has-object? odb obj)
  (git-odb-exists (odb->pointer odb)
                  (oid->pointer (->oid obj))))

(define (odb-open loc)
  (pointer->odb
    (cond ((string? loc) (git-odb-open loc))
          ((repository? loc) (git-repository-odb (repository->pointer loc)))
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
  (format "#<signature \"~A <~A>\">" (signature-name signature) (signature-email signature)))

(define (signature-time sig) (git-time-time (git-signature-time (signature->pointer sig))))
(define (signature-time-offset sig) (git-time-offset (git-signature-time (signature->pointer sig))))

(define (make-signature name email #!optional time (offset 0))
  (set-finalizer!
    (pointer->signature
      (if time
        (git-signature-new name email time offset)
        (git-signature-now name email)))
    (lambda (sig)
      (git-signature-free
        (signature->pointer sig)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags

(define-git-record-type
  (tag id type name message)
  (format "#<tag ~S>" (tag-name tag))
  (git-tag-free))

(define (tag repo ref)
  (pointer->tag
    (git-tag-lookup
      (repository->pointer repo)
      (oid->pointer (->oid ref)))))

(define (tags repo)
  (let lp ((tags (references repo))
           (acc '()))
    (if (null? tags)
      (reverse acc)
      (lp (cdr tags)
          (condition-case
            (cons (tag repo (reference-id (car tags))) acc)
            ((git) acc))))))

(define (tag-tagger tag) (pointer->signature (git-tag-tagger (tag->pointer tag))))
(define (tag-target tag) (pointer->object (git-tag-target (tag->pointer tag))))
(define (tag-delete tag)
  (git-tag-delete
    (git-object-owner (tag->pointer tag))
    (tag-name tag)))

(define (create-tag repo #!key target name message tagger force)
  (tag repo
    (pointer->oid
      (git-tag-create
        (repository->pointer repo)
        name
        (object->pointer target)
        (signature->pointer tagger)
        message
        force))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trees

(define-git-record-type
  (tree id entrycount)
  (format "#<tree ~S>" (oid->string (->oid tree) 7))
  (git-tree-free))

(define-git-record-type
  (tree-entry id name attributes type)
  (format "#<tree-entry ~S>" (tree-entry-name tree-entry)))

(define (tree repo ref)
  (pointer->tree
    (git-tree-lookup
      (repository->pointer repo)
      (oid->pointer (->oid ref)))))

(define (tree-subtree tree path)
  (pointer->tree (git-tree-get-subtree (tree->pointer tree) path)))

(define (tree-ref tree key)
  (pointer->tree-entry
    (let ((tree* (tree->pointer tree)))
      (cond ((number? key)
             (git-tree-entry-byindex tree* key))
            ((string? key)
             (or (git-tree-entry-byname tree* key)
                 (condition-case
                   (git-tree-entry-byname
                     (git-tree-get-subtree tree* key)
                     (pathname-strip-directory key))
                   ((git) #f))))
            (else
             (git-git-error 'tree-ref "Invalid key" key))))))

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

;; Returns a list of tree entries.
;; A repository can optionally be
;; given into which to recurse.
(define (tree->list tree #!optional recurse)
  (map (if (not recurse)
         (lambda (i) (tree-ref tree i))
         (lambda (i)
           (let ((entry (tree-ref tree i)))
             (case (tree-entry-type entry)
               ((tree) (tree->list (tree-entry->object recurse entry) recurse))
               (else entry)))))
       (iota (tree-entrycount tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Builders

(define-git-record-type
  (tree-builder clear)
  "#<tree-builder>"
  (git-tree-builder-free))

(define (make-tree-builder #!optional tree)
  (pointer->tree-builder
    (git-tree-builder-create
      (or (and tree (tree->pointer tree))))))

(define (tree-builder-ref tb path)    (git-tree-builder-get (tree-builder->pointer tb) path))
(define (tree-builder-remove tb path) (git-tree-builder-remove (tree-builder->pointer tb) path))

(define (tree-builder-insert builder obj path attributes)
  (pointer->tree-entry
    (git-tree-builder-insert
      (tree-builder->pointer builder)
      path
      (oid->pointer (->oid obj))
      attributes)))

(define (tree-builder-write repo tb)
  (tree repo
    (pointer->oid
      (git-tree-builder-write
        (tree-builder->pointer tb)
        (repository->pointer repo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Diffs

(define-git-record-type
  (tree-diff old-oid new-oid old-attr new-attr status path)
  (format "#<tree-diff ~S>" (tree-diff-path tree-diff)))

(define (tree-diff-old-id diff) (pointer->oid (tree-diff-old-oid diff)))
(define (tree-diff-new-id diff) (pointer->oid (tree-diff-new-oid diff)))

(define (tree-diff tree1 tree2)
  (map pointer->tree-diff
       (git-tree-diff
         (and tree1 (tree->pointer tree1))
         (and tree2 (tree->pointer tree2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configs

(define-git-record-type
  (config)
  "#<config>"
  (git-config-free))

(define (config-path #!optional (type 'user))
  (case type
    ((user)   (git-config-find-global))
    ((system) (git-config-find-system))
    (else (git-git-error 'config-open
                         "Invalid configuration path type"
                         type))))

(define (config-open #!optional (target 'user))
  (pointer->config
    (cond ((string? target)
           (git-config-open-ondisk target))
          ((symbol? target)
           (git-config-open-ondisk (config-path target)))
          ((repository? target)
           (git-repository-config
             (repository->pointer target)))
          (else (git-git-error 'config-open
                               "Invalid configuration source"
                               target)))))

(define (config-get config name #!optional (type 'string))
  ((case type
     ((boolean) git-config-get-bool)
     ((string)  git-config-get-string)
     ((number)  git-config-get-int64)
     (else (git-git-error 'config-get "Invalid value type specifier" type)))
   (config->pointer config)
   name))

(define (config-set config name value)
  ((cond ((boolean? value) git-config-set-bool)
         ((string? value)  git-config-set-string)
         ((number? value)  git-config-set-int64)
         (else (git-git-error 'config-set "Invalid value type" value)))
   (config->pointer config)
   name
   value))

(define (config-unset cfg name)
  (git-config-delete (config->pointer cfg) name))

)
