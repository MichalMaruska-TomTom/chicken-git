;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git-lolevel.scm - libgit2 bindings for Chicken Scheme
;;
;; Copyright (c) 2012, Evan Hanson
;; See LICENSE for details
;;
;; See git.scm for a cleaner, high-level API.

(module git-lolevel *
  (import scheme lolevel foreign foreigners srfi-69
    (except chicken repository-path)
    (only srfi-13 string-index))
  (require-library srfi-13 lolevel)

;; Errors are composite conditions
;; of properties (exn git).
(define (git-error loc msg . args)
  (signal (make-composite-condition
            (make-property-condition 'git)
            (make-property-condition 'exn 'location  loc
                                          'message   msg))))

;; Check the return value of an expression,
;; signaling an error when nonzero.
(define-syntax guard-errors
  (syntax-rules ()
    ((_ <loc> <exp>)
     (begin
       (error-clear)
       (let ((res <exp>))
         (if (< res 0)
           (git-error <loc> (error-last))))))))

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

;; Same.
(define-syntax define/retval
  (syntax-rules ()
    ((_  <name> (<cfun> (<atype> <arg>) ...))
     (define (<name> <arg> ...)
       (if (not (guard-errors '<name>
                  ((foreign-lambda int <cfun> <atype> ...) <arg> ...)))
         #f))))) ;; Ignored, the if just forces a void return.

;;;
;;; Callback management.
;;;
;;; We have to make sure procedures passed to C as callbacks aren't moved by
;;; the GC while in use, so we store them in a lookup table and pass integer
;;; keys to the libgit2 functions that need them.
;;;

(define-values (callback-lookup callback-unregister! callback-register!)
  (let ((callback-index 0)
        (callback-table (make-hash-table)))
    (values
     (lambda (i) (hash-table-ref callback-table i))
     (lambda (i) (hash-table-delete! callback-table i))
     (lambda (c)
       (let ((index callback-index))
         (hash-table-set! callback-table index c)
         (set! callback-index (+ index 1))
         index)))))

(define (with-callback c proc)
  (let ((callback #f))
    (dynamic-wind
     (lambda () (set! callback (callback-register! c)))
     (lambda () (proc callback))
     (lambda () (callback-unregister! callback)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git2.h

(foreign-declare "#include <git2.h>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types.h

(define-foreign-type time-t integer64)
(define-foreign-type off-t integer64)

(define-foreign-record-type (time git_time)
  (time-t time time-time)
  (int offset time-offset))

(define-foreign-record-type (signature git_signature)
  (c-string name signature-name)
  (c-string email signature-email)
  ((struct time) when signature-time))

(define-foreign-record-type (oid git_oid)
  (constructor: %make-oid)
  (destructor: oid-free)
  (unsigned-char (id (foreign-value GIT_OID_RAWSZ int)) oid-id))

(define (make-oid)
  (set-finalizer! (%make-oid) oid-free))

(define-foreign-record-type (index-time git_index_time)
  (time-t seconds index-time-seconds)
  (unsigned-int nanoseconds index-time-nanoseconds))

(define-foreign-record-type (index-entry git_index_entry)
  ((struct index-time) ctime index-entry-ctime)
  ((struct index-time) mtime index-entry-mtime)
  (unsigned-int dev index-entry-dev)
  (unsigned-int ino index-entry-ino)
  (unsigned-int mode index-entry-mode)
  (unsigned-int uid index-entry-uid)
  (unsigned-int gid index-entry-gid)
  (off-t file_size index-entry-size)
  ((struct oid) oid index-entry-oid)
  (unsigned-int flags index-entry-flags)
  (unsigned-int flags_extended index-entry-extended)
  (c-string path index-entry-path))

;; I cannot see this unmerge in header files, anymore.
#;(define-foreign-record-type (index-entry-unmerged git_index_entry_unmerged)
  (unsigned-int (mode 3) index-entry-unmerged-mode)
  ((struct oid) (oid 3) index-entry-unmerged-oid)
  (c-string path index-entry-unmerged-path))

(define-foreign-type commit         (c-pointer "git_commit"))
(define-foreign-type config         (c-pointer "git_config"))
(define-foreign-type blob*          (c-pointer "git_blob")) ; clash w/ built-in
;; (define-foreign-type entry-unmerged (c-pointer "git_index_entry_unmerged"))
(define-foreign-type index          (c-pointer "git_index"))
(define-foreign-type object         (c-pointer "git_object"))
(define-foreign-type odb            (c-pointer "git_odb"))
(define-foreign-type odb-object     (c-pointer "git_odb_object"))
(define-foreign-type oid-shorten    (c-pointer "git_oid_shorten"))
(define-foreign-type reference      (c-pointer "git_reference"))
(define-foreign-type repository     (c-pointer "git_repository"))
(define-foreign-type revwalk        (c-pointer "git_revwalk"))
(define-foreign-type tag            (c-pointer "git_tag"))
(define-foreign-type tree           (c-pointer "git_tree"))
(define-foreign-type tree-entry     (c-pointer "git_tree_entry"))
(define-foreign-type tree-builder   (c-pointer "git_treebuilder"))

(define-foreign-enum-type (otype int)
  (otype->int int->otype)
  ((any       otype/any)       GIT_OBJ_ANY)
  ((bad       otype/bad)       GIT_OBJ_BAD)
  ((ext1      otype/ext1)      GIT_OBJ__EXT1)
  ((commit    otype/commit)    GIT_OBJ_COMMIT)
  ((tree      otype/tree)      GIT_OBJ_TREE)
  ((blob      otype/blob)      GIT_OBJ_BLOB)
  ((tag       otype/tag)       GIT_OBJ_TAG)
  ((ext2      otype/ext2)      GIT_OBJ__EXT2)
  ((ofs-delta otype/ofs-delta) GIT_OBJ_OFS_DELTA)
  ((ref-delta otype/ref-delta) GIT_OBJ_REF_DELTA))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; blob.h

(define/allocate blob* blob*-lookup
  (git_blob_lookup (repository repo) (oid id)))

(define/allocate blob* blob*-lookup-prefix
  (git_blob_lookup_prefix (repository repo) (oid id) (unsigned-int len)))

(define blob*-free              (foreign-lambda void git_blob_free blob*))
(define blob*-rawcontent        (foreign-lambda c-pointer git_blob_rawcontent blob*))
(define blob*-rawsize           (foreign-lambda size_t git_blob_rawsize blob*))
(define blob*-create-fromfile   (foreign-lambda int git_blob_create_fromfile oid repository c-string))
(define blob*-create-frombuffer (foreign-lambda int git_blob_create_frombuffer oid repository c-string unsigned-int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; branch.h

(define-foreign-enum-type (btype int)
  (btype->int int->btype)
  ((local  btype/local)  GIT_BRANCH_LOCAL)
  ((remote btype/remote) GIT_BRANCH_REMOTE))

(define (branch-create repo name target force)
  (let ((id (make-oid)))
    (guard-errors branch-create
      ((foreign-lambda int git_branch_create
         oid repository c-string object bool)
         id  repo       name     target force))
    id))

(define (branch-list repo flags)
  (let ((sa (make-strarray)))
    (guard-errors branch-list
      ((foreign-lambda int git_branch_list strarray repository btype) sa repo flags))
    (strarray-strings sa)))

(define/retval branch-delete (git_branch_delete (repository repo) (c-string name) (btype type)))
(define/retval branch-move   (git_branch_move   (repository repo) (c-string old) (c-string new) (bool force)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commit.h

(define/allocate commit commit-lookup
  (git_commit_lookup (repository repo) (oid id)))

(define/allocate commit commit-lookup-prefix
  (git_commit_lookup_prefix (repository repo) (oid id) (unsigned-int len)))

(define commit-free             (foreign-lambda void git_commit_free commit))
(define commit-id               (foreign-lambda oid git_commit_id commit))
(define commit-message          (foreign-lambda c-string git_commit_message commit))
(define commit-message-encoding (foreign-lambda c-string git_commit_message_encoding commit))
(define commit-time             (foreign-lambda time-t git_commit_time commit))
(define commit-time-offset      (foreign-lambda int git_commit_time_offset commit))
(define commit-committer        (foreign-lambda signature git_commit_committer commit))
(define commit-author           (foreign-lambda signature git_commit_author commit))
(define commit-tree-oid         (foreign-lambda oid git_commit_tree_oid commit))
(define commit-parentcount      (foreign-lambda unsigned-int git_commit_parentcount commit))
(define commit-parent-oid       (foreign-lambda oid git_commit_parent_oid commit unsigned-int))

(define/allocate tree commit-tree (git_commit_tree (commit cmt)))
(define/allocate commit commit-parent (git_commit_parent (commit cmt) (unsigned-int n)))

(define (commit-create repo ref author commit msg tree . parents)
  (let ((id (make-oid)))
    (guard-errors commit-create
      ((foreign-lambda int git_commit_create
         oid repository c-string signature signature c-string c-string tree int              pointer-vector)
         id  repo       ref      author    commit    #f       msg      tree (length parents) (apply pointer-vector parents)))
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common.h

(define-foreign-record-type (strarray git_strarray)
  (constructor: %make-strarray)
  ((c-pointer c-string) strings %strarray-strings)
  (unsigned-int count strarray-count))

(define (make-strarray)
  (set-finalizer! (%make-strarray) strarray-free))

(define strarray-free (foreign-lambda void git_strarray_free strarray))

;; Gets a GC'd list of strings from the strarray
;; (for return from e.g. git_reference_listall).
(define (strarray-strings sa)
  ((foreign-lambda* c-string-list* ((strarray sa))
     "int i;
      char **t;
      t = malloc(sizeof(char *) * sa->count + 1);
      for(i = 0; i < sa->count; i++) {
        t[i] = malloc(strlen(sa->strings[i]) + 1);
        strcpy(t[i], sa->strings[i]);
      }
      t[i] = NULL;
      C_return(t);")
     sa))

(define (libgit2-version)
  (let-location ((major int) (minor int) (rev int))
    ((foreign-lambda void git_libgit2_version (c-pointer int) (c-pointer int) (c-pointer int))
       (location major)
       (location minor)
       (location rev))
    (vector major minor rev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config.h

(define-foreign-record-type (config-file git_config_file)
  (config cfg config-file-config)
  ((function int (config-file)) open config-file-open)
  ((function int (config-file c-string (pointer c-string))) get config-file-get)
  ((function int (config-file c-string c-string)) set config-file-set)
  ((function int (config-file (function int (c-string c-string c-pointer)) c-pointer)) foreach config-file-foreach)
  ((function int (config-file)) free config-file-free))

(define config-free (foreign-lambda void git_config_free config))
(define/allocate config config-new (git_config_new))
(define/allocate config config-open-global (git_config_open_global))
(define/allocate config config-open-ondisk (git_config_open_ondisk (c-string path)))
(define/retval config-delete (git_config_delete (config cfg) (c-string name)))
(define/retval config-add-file (git_config_add_file (config cfg) (config-file file) (int priority)))
(define/retval config-add-file-ondisk (git_config_add_file_ondisk (config cfg) (c-string path) (int priority)))

(define-syntax define/config-path
  (lambda (e . r)
    `(define (,(cadr e))
       (let ((len (foreign-value GIT_PATH_MAX int)))
         (let ((str (make-string len)))
           (guard-errors ,(cadr e)
             ((foreign-lambda int ,(caddr e) scheme-pointer unsigned-int) str len))
           (substring str 0 (string-index str #\x00)))))))

(define/config-path config-find-global git_config_find_global)
(define/config-path config-find-system git_config_find_system)

(define-syntax define/config
  (syntax-rules (getter setter)
    ((_ getter <type> <fun> <cfun>)
     (define (<fun> cfg name)
       (let-location ((out <type>))
         (guard-errors <fun>
           ((foreign-lambda int <cfun> (c-pointer <type>) config (const c-string)) (location out) cfg name))
         out)))
    ((_ setter <type> <fun> <cfun>)
     (define (<fun> cfg name val)
       (guard-errors <fun>
         ((foreign-lambda int <cfun> config (const c-string) <type>) cfg name val))))))

(define/config getter c-string  config-get-string git_config_get_string)
(define/config getter integer32 config-get-int32  git_config_get_int32)
(define/config getter integer64 config-get-int64  git_config_get_int64)
(define/config getter bool      config-get-bool   git_config_get_bool)
(define/config setter c-string  config-set-string git_config_set_string)
(define/config setter integer32 config-set-int32  git_config_set_int32)
(define/config setter integer64 config-set-int64  git_config_set_int64)
(define/config setter bool      config-set-bool   git_config_set_bool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff.h

(define-foreign-type diff-list (c-pointer "git_diff_list"))
(define-foreign-type diff-file-fn (c-pointer "git_diff_file_fn"))

(define-foreign-enum-type (delta int)
  (delta->int int->delta)
  ((modified  diff/unmodified) GIT_DELTA_UNMODIFIED)
  ((added     diff/added)      GIT_DELTA_ADDED)
  ((deleted   diff/deleted)    GIT_DELTA_DELETED)
  ((modified  diff/modified)   GIT_DELTA_MODIFIED)
  ((renamed   diff/renamed)    GIT_DELTA_RENAMED)
  ((copied    diff/copied)     GIT_DELTA_COPIED)
  ((ignored   diff/ignored)    GIT_DELTA_IGNORED)
  ((untracked diff/untracked)  GIT_DELTA_UNTRACKED))

(define-foreign-record-type (diff-options git_diff_options)
  (unsigned-int32    flags           diff-options-flags           diff-options-flags-set!)
  (unsigned-short    context_lines   diff-options-context-lines   diff-options-context-lines-set!)
  (unsigned-short    interhunk_lines diff-options-interhunk-lines diff-options-interhunk-lines-set!)
  (c-string          old_prefix      diff-options-old-prefix      diff-options-old-prefix-set!)
  (c-string          new_prefix      diff-options-new-prefix      diff-options-new-prefix-set!)
  ((struct strarray) pathspec        diff-options-pathspec        diff-options-pathspec-set!))

(define-foreign-record-type (diff-file git_diff_file)
  ((struct oid)   oid    diff-file-oid)
  (c-string       path   diff-file-path)
  (unsigned-short mode   diff-file-mode)
  (off-t          size   diff-file-size)
  (unsigned-int   flags  diff-file-flags))

(define-foreign-record-type (diff-delta git_diff_delta)
  ((struct diff-file) old_file   diff-delta-old-file)
  ((struct diff-file) new_file   diff-delta-new-file)
  (delta              status     diff-delta-status)
  (unsigned-int       similarity diff-delta-similarity)
  (int                binary     diff-delta-binary))

(define diff-list-free (foreign-lambda void git_diff_list_free diff-list))

(define-syntax define/diff
  (syntax-rules ()
    ((_ <name> (<cfun> (<type> <arg>) ...))
     (define (<name> repo <arg> ...)
       (let-location ((diffs diff-list))
         (guard-errors <name>
           ((foreign-lambda int <cfun>
             repository diff-options <type> ... (c-pointer diff-list))
             repo       #f           <arg>  ... (location diffs)))
         (set-finalizer! diffs diff-list-free))))))

(define/diff diff-tree-to-tree     (git_diff_tree_to_tree (tree old) (tree new)))
(define/diff diff-index-to-tree    (git_diff_index_to_tree (tree old)))
(define/diff diff-workdir-to-tree  (git_diff_workdir_to_tree (tree old)))
(define/diff diff-workdir-to-index (git_diff_workdir_to_index))

(define/retval diff-merge (git_diff_merge (diff-list onto) (diff-list from)))

(define-external (diff_file_fn (scheme-object fn) (diff-delta diff) (float progress)) int
  ((callback-lookup fn) diff))

(define (diff-foreach fn diffs)
  (with-callback fn
   (lambda (callback)
     (guard-errors diff-foreach
      ((foreign-safe-lambda int git_diff_foreach
        diff-list scheme-object diff-file-fn            c-pointer c-pointer)
        diffs     callback      (location diff_file_fn) #f        #f)))))

(define (diff-blobs old new fn diffs)
  (with-callback fn
   (lambda (callback)
     (guard-errors diff-blobs
      ((foreign-safe-lambda int git_diff_blobs
        blob* blob* diff-options scheme-object diff-file-fn            c-pointer c-pointer)
        old   new   #f           callback      (location diff_file_fn) #f        #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; errors.h

(define-foreign-record-type (error git_error)
  (c-string message error-message)
  (int      klass   error-class))

(define-foreign-enum-type (generic-error int)
  (generic-error->int int->generic-error)
  ((ok           err/ok)          GIT_OK)
  ((error        err/error)       GIT_ERROR)
  ((notfound     err/notfound)    GIT_ENOTFOUND)
  ((exists       err/exists)      GIT_EEXISTS)
  ((ambiguous    err/ambiguous)   GIT_EAMBIGUOUS)
  ((bufs         err/bufs)        GIT_EBUFS)
  ((passthrough  err/passthrough) GIT_PASSTHROUGH)
  ((revwalkover  err/revwalkover) GIT_REVWALKOVER))

(define-foreign-enum-type (error-t int)
  (error->int int->error)
  ((nomemory   err/nomemory)   GITERR_NOMEMORY)
  ((os         err/os)         GITERR_OS)
  ((invalid    err/invalid)    GITERR_INVALID)
  ((reference  err/reference)  GITERR_REFERENCE)
  ((zlib       err/zlib)       GITERR_ZLIB)
  ((repository err/repository) GITERR_REPOSITORY)
  ((config     err/config)     GITERR_CONFIG)
  ((regex      err/regex)      GITERR_REGEX)
  ((odb        err/odb)        GITERR_ODB)
  ((index      err/index)      GITERR_INDEX)
  ((object     err/object)     GITERR_OBJECT)
  ((net        err/net)        GITERR_NET)
  ((tag        err/tag)        GITERR_TAG)
  ((tree       err/tree)       GITERR_TREE)
  ((indexer    err/indexer)    GITERR_INDEXER))

(define error-clear (foreign-lambda void giterr_clear))

(define (error-last)
  (and-let* ((err ((foreign-lambda (c-pointer error) giterr_last))))
    (error-message err)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; index.h

(define/allocate index index-open (git_index_open (c-string path)))

(define index-clear (foreign-lambda void git_index_clear index))
(define index-free  (foreign-lambda void git_index_free index))
(define index-find  (foreign-lambda int git_index_find index c-string))
(define index-uniq  (foreign-lambda void git_index_uniq index))

(define/retval index-read   (git_index_read (index ix)))
(define/retval index-write  (git_index_write (index ix)))
(define/retval index-add    (git_index_add (index ix) (c-string path) (int stage)))
(define/retval index-append (git_index_append (index ix) (c-string path) (int stage)))
(define/retval index-remove (git_index_remove (index ix) (int pos)))

(define index-get                  (foreign-lambda index-entry git_index_get index unsigned-int))
(define index-entrycount           (foreign-lambda unsigned-int git_index_entrycount index))
(define index-entrycount-unmerged  (foreign-lambda unsigned-int git_index_entrycount_unmerged index))
;(define index-get-unmerged-bypath  (foreign-lambda index-entry-unmerged git_index_get_unmerged_bypath index c-string))
;(define index-get-unmerged-byindex (foreign-lambda index-entry-unmerged git_index_get_unmerged_byindex index unsigned-int))
(define index-entry-stage          (foreign-lambda int git_index_entry_stage index-entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; merge.h

(define (merge-base repo a b)
  (let ((id (make-oid)))
    (guard-errors merge-base
      ((foreign-lambda int git_merge_base oid repository oid oid) id repo a b))
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; object.h

(define/allocate object object-lookup 
  (git_object_lookup (repository repo) (oid id) (otype type)))

(define object-id          (foreign-lambda oid git_object_id object))
(define object-free        (foreign-lambda void git_object_free object))
(define object-owner       (foreign-lambda repository git_object_owner object))
(define object-type        (foreign-lambda otype git_object_type object))
(define object-type2string (foreign-lambda otype git_object_type2string otype))
(define object-string2type (foreign-lambda otype git_object_string2type c-string))
(define object-typeisloose (foreign-lambda bool git_object_typeisloose otype))

;; Conflicts with built-in, and we don't really need it.
; (define object-size        (foreign-lambda size_t git_object__size otype))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; odb_backend.h
;;
;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; odb.h
;;
;; TODO git_odb_add_backend git_odb_add_alternate git_odb_read_header
;;      git_odb_open_wstream git_odb_open_rstream

(define/allocate odb odb-new  (git_odb_new))
(define/allocate odb odb-open (git_odb_open (c-string dir)))

(define odb-free (foreign-lambda void git_odb_free odb))
(define odb-exists (foreign-lambda bool git_odb_exists odb oid))

(define/allocate odb-object odb-read
  (git_odb_read (odb db) (oid id)))

(define/allocate odb-object odb-read-prefix
  (git_odb_read_prefix (odb db) (oid id) (unsigned-int len)))

(define (odb-write db data len type)
  (let ((id (make-oid)))
    (guard-errors odb-write
      ((foreign-lambda int git_odb_write
         oid odb scheme-pointer size_t otype)
         id  db  data           len    type))
    id))

(define (odb-hash data len type)
  (let ((id (make-oid)))
    (guard-errors odb-hash
      ((foreign-lambda int git_odb_hash
         oid scheme-pointer  size_t otype)
         id  data            len    type))
    id))

(define odb-object-free  (foreign-lambda void git_odb_object_free odb-object))
(define odb-object-id    (foreign-lambda oid git_odb_object_id odb-object))
(define odb-object-data  (foreign-lambda c-pointer git_odb_object_data odb-object))
(define odb-object-size  (foreign-lambda size_t git_odb_object_size odb-object))
(define odb-object-type  (foreign-lambda otype git_odb_object_type odb-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; oid.h

(define (oid-fromstr str)
  (let ((id (make-oid)))
    (guard-errors oid-fromstr
      ((foreign-lambda int git_oid_fromstr oid c-string) id str))
    id))

(define (oid-fromraw raw)
  (let ((id (make-oid)))
    ((foreign-lambda void git_oid_fromraw oid blob) id raw)
    id))

(define (oid-fmt oid)
  (let* ((str (make-string 40)))
    ((foreign-lambda void git_oid_fmt scheme-pointer oid) str oid)
    str))

(define (oid-pathfmt oid)
  (let* ((str (make-string 41)))
    ((foreign-lambda void git_oid_pathfmt scheme-pointer oid) str oid)
    str))

(define oid-allocfmt     (foreign-lambda c-string git_oid_allocfmt oid))

(define (oid-tostr n id)
  (let* ((str (make-string (max n 1))))
    ((foreign-lambda c-string git_oid_tostr
       scheme-pointer size_t  oid)
       str            (+ n 1) id)))

(define oid-cpy          (foreign-lambda void git_oid_cpy oid oid))
(define oid-cmp          (foreign-lambda int git_oid_cmp oid oid))
(define oid-ncmp         (foreign-lambda int git_oid_ncmp oid oid unsigned-int))
(define oid-shorten-new  (foreign-lambda oid-shorten git_oid_shorten_new size_t))
(define oid-shorten-free (foreign-lambda void git_oid_shorten_free oid-shorten))

(define/retval oid-shorten-add
  (git_oid_shorten_add (oid-shorten osh) ((const c-string) oid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reflog.h
;;
;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; refs.h

(define-foreign-enum-type (rtype int)
  (rtype->int int->rtype)
  ((invalid  rtype/invalid)  GIT_REF_INVALID)
  ((oid      rtype/oid)      GIT_REF_OID)
  ((symbolic rtype/symbolic) GIT_REF_SYMBOLIC)
  ((packed   rtype/packed)   GIT_REF_PACKED)
  ((haspeel  rtype/haspeel)  GIT_REF_HAS_PEEL)
  ((listall  rtype/listall)  GIT_REF_LISTALL))

(define/allocate reference reference-lookup
  (git_reference_lookup (repository repo) (c-string name)))

(define/allocate reference reference-create-symbolic
  (git_reference_create_symbolic (repository repo) (c-string name) (c-string target) (bool force)))

(define/allocate reference reference-create-oid
  (git_reference_create_oid (repository repo) (c-string name) (oid id) (bool force)))

(define reference-free   (foreign-lambda void git_reference_free reference))
(define reference-oid    (foreign-lambda oid git_reference_oid reference))
(define reference-target (foreign-lambda c-string git_reference_target reference))
(define reference-type   (foreign-lambda rtype git_reference_type reference))
(define reference-name   (foreign-lambda c-string git_reference_name reference))
(define reference-owner  (foreign-lambda repository git_reference_owner reference))

(define reference-is-packed (foreign-lambda bool git_reference_is_packed reference))

(define/allocate reference reference-resolve (git_reference_resolve (reference ref)))

(define/retval reference-set-target (git_reference_set_target (reference ref) (c-string target)))
(define/retval reference-set-oid    (git_reference_set_oid (reference ref) (oid id)))
(define/retval reference-rename     (git_reference_rename (reference ref) (c-string name) (bool force)))
(define/retval reference-delete     (git_reference_delete (reference ref)))
(define/retval reference-reload     (git_reference_reload (reference ref)))
(define/retval reference-packall    (git_reference_packall (repository repo)))

(define (reference-list repo flags)
  (let ((sa (make-strarray)))
    (guard-errors reference-list
      ((foreign-lambda int git_reference_list strarray repository rtype) sa repo flags))
    (strarray-strings sa)))

(define-external (reference_foreach_cb ((const c-string) name) (scheme-object fn)) int
  ((callback-lookup fn) name))

(define (reference-foreach repo flags fn)
  (with-callback fn
   (lambda (callback)
     (guard-errors reference-foreach
      ((foreign-safe-lambda int git_reference_foreach
        repository rtype (function int ((const c-string) scheme-object)) scheme-object)
        repo       flags (location reference_foreach_cb)                 callback)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; repository.h

(define/allocate repository repository-open
  (git_repository_open (c-string path)))

(define/allocate repository repository-init
  (git_repository_init (c-string path) (bool bare)))

(define/allocate index repository-index
  (git_repository_index (repository repo)))

(define/allocate odb repository-odb
  (git_repository_odb (repository repo)))

(define/allocate reference repository-head
  (git_repository_head (repository repo)))

(define/allocate config repository-config
  (git_repository_config (repository repo)))

(define repository-free     (foreign-lambda void git_repository_free repository))
(define repository-is-empty (foreign-lambda bool git_repository_is_empty repository))
(define repository-is-bare  (foreign-lambda bool git_repository_is_bare repository))
(define repository-path     (foreign-lambda c-string git_repository_path repository))
(define repository-workdir  (foreign-lambda c-string git_repository_workdir repository))

(define repository-head-detached (foreign-lambda bool git_repository_head_detached repository))
(define repository-head-orphan   (foreign-lambda bool git_repository_head_orphan repository))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remote.h
;;
;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; revwalk.h

(define-foreign-enum-type (sort int)
  (sort->int int->sort)
  ((none sort/none) GIT_SORT_NONE)
  ((topo sort/topo) GIT_SORT_TOPOLOGICAL)
  ((time sort/time) GIT_SORT_TIME)
  ((rev sort/rev) GIT_SORT_REVERSE))

(define/allocate revwalk revwalk-new
  (git_revwalk_new (repository repo)))

(define revwalk-free        (foreign-lambda void git_revwalk_free revwalk))
(define revwalk-reset       (foreign-lambda void git_revwalk_reset revwalk))
(define revwalk-sorting     (foreign-lambda void git_revwalk_sorting revwalk sort))
(define revwalk-repository  (foreign-lambda repository git_revwalk_repository revwalk))
(define/retval revwalk-push (git_revwalk_push (revwalk wlk) (oid id)))
(define/retval revwalk-hide (git_revwalk_hide (revwalk wlk) (oid id)))

(define (revwalk-next walker)
  (let ((id (make-oid)))
    (guard-errors revwalk-next
      ((foreign-lambda int git_revwalk_next oid revwalk) id walker))
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; signature.h

(define/allocate signature signature-new
  (git_signature_new (c-string name) (c-string email) (time-t time) (int offset)))

(define/allocate signature signature-now
  (git_signature_now (c-string name) (c-string email)))

(define signature-dup  (foreign-lambda signature git_signature_dup signature))
(define signature-free (foreign-lambda void git_signature_free signature))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; status.h

(define-foreign-enum-type (status int)
  (status->int int->status)
  ((current           status/current)           GIT_STATUS_CURRENT)
  ((index/new         status/index/new)         GIT_STATUS_INDEX_NEW)
  ((index/modified    status/index/modified)    GIT_STATUS_INDEX_MODIFIED)
  ((index/deleted     status/index/deleted)     GIT_STATUS_INDEX_DELETED)
  ((worktree/new      status/worktree/new)      GIT_STATUS_WT_NEW)
  ((worktree/modified status/worktree/modified) GIT_STATUS_WT_MODIFIED)
  ((worktree/deleted  status/worktree/deleted)  GIT_STATUS_WT_DELETED)
  ((ignored           status/ignored)           GIT_STATUS_IGNORED))

;; NOTE if a file is of two statuses (e.g. partially-staged, so it is
;; both index-modified and worktree-modified) this will return '().
;; TODO fix this.
(define/allocate status status-file (git_status_file (repository repo) (c-string path)))

(define (status-should-ignore repo path)
  (let-location ((ignore int))
    (guard-errors status-should-ignore
      ((foreign-lambda int git_status_should_ignore
         (c-pointer int)   repository c-string)
         (location ignore) repo       path))
    (not (zero? ignore))))

;; Maybe TODO foreach.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag.h

(define/allocate tag tag-lookup
  (git_tag_lookup (repository repo) (oid id)))

(define/allocate object tag-target (git_tag_target (tag t)))
(define/allocate object tag-peel   (git_tag_peel (tag t)))

(define tag-free       (foreign-lambda void git_tag_free tag))
(define tag-id         (foreign-lambda oid git_tag_id tag))
(define tag-target-oid (foreign-lambda oid git_tag_target_oid tag))
(define tag-type       (foreign-lambda otype git_tag_type tag))
(define tag-name       (foreign-lambda c-string git_tag_name tag))
(define tag-tagger     (foreign-lambda signature git_tag_tagger tag))
(define tag-message    (foreign-lambda c-string git_tag_message tag))

(define (tag-create repo name target tagger msg force)
  (let ((id (make-oid)))
    (guard-errors tag-create
      ((foreign-lambda int git_tag_create
         oid repository c-string object signature c-string bool)
         id  repo       name     target tagger    msg      force))
    id))

(define/retval tag-delete
  (git_tag_delete (repository repo) (c-string name)))

(define (tag-list repo)
  (let ((sa (make-strarray)))
    (guard-errors tag-list
      ((foreign-lambda int git_tag_list strarray repository) sa repo))
    (strarray-strings sa)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree.h

(define/allocate tree tree-lookup
  (git_tree_lookup (repository repo) (oid id)))

(define/allocate tree tree-lookup-prefix
  (git_tree_lookup_prefix (repository repo) (oid id) (unsigned-int len)))

(define tree-free             (foreign-lambda void git_tree_free tree))
(define tree-id               (foreign-lambda oid git_tree_id tree))
(define tree-entrycount       (foreign-lambda unsigned-int git_tree_entrycount tree))
(define tree-entry-byname     (foreign-lambda tree-entry git_tree_entry_byname tree c-string))
(define tree-entry-byindex    (foreign-lambda tree-entry git_tree_entry_byindex tree unsigned-int))
(define tree-entry-attributes (foreign-lambda unsigned-int git_tree_entry_attributes tree-entry))
(define tree-entry-name       (foreign-lambda c-string git_tree_entry_name tree-entry))
(define tree-entry-id         (foreign-lambda oid git_tree_entry_id tree-entry))
(define tree-entry-type       (foreign-lambda otype git_tree_entry_type tree-entry))

(define/allocate tree tree-get-subtree
  (git_tree_get_subtree (tree tr) ((const c-string) path)))

(define/allocate object tree-entry-to-object
  (git_tree_entry_to_object (repository repo) (tree-entry entry)))

(define (tree-create-fromindex ix)
  (let ((id (make-oid)))
    (guard-errors tree-create-fromindex
      ((foreign-lambda int git_tree_create_fromindex oid index) id ix))
    id))

(define/allocate tree-builder tree-builder-create
  (git_treebuilder_create (tree source)))

(define tree-builder-free  (foreign-lambda void git_treebuilder_free tree-builder))
(define tree-builder-clear (foreign-lambda void git_treebuilder_clear tree-builder))
(define tree-builder-get   (foreign-lambda tree-entry git_treebuilder_get tree-builder c-string))

(define/allocate tree-entry tree-builder-insert
  (git_treebuilder_insert (tree-builder tb) (c-string path) (oid id) (unsigned-int attributes)))

(define/retval tree-builder-remove
  (git_treebuilder_remove (tree-builder tb) (c-string path)))

(define (tree-builder-write tb repo)
  (let ((id (make-oid)))
    (guard-errors tree-builder-write
      ((foreign-lambda int git_treebuilder_write oid repository tree-builder) id repo tb))
    id))

(define-foreign-enum-type (treewalk-mode int)
  (treewalk-mode->int int->treewalk-mode)
  ((pre  treewalk-mode/pre)  GIT_TREEWALK_PRE)
  ((post treewalk-mode/post) GIT_TREEWALK_POST))

(define-external (treewalk_cb (c-string root) (tree-entry te) (scheme-object fn)) int
  ((callback-lookup fn) root te))

(define (tree-walk tree fn mode)
  (with-callback fn
   (lambda (callback)
     (guard-errors tree-walk
      ((foreign-safe-lambda int git_tree_walk
        tree (function int (c-string tree-entry scheme-object)) treewalk-mode scheme-object)
        tree (location treewalk_cb)                             mode          callback)))))

;; Maybe TODO tree-builder-filter

)
