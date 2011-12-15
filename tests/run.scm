(use git test posix)

(define *repository* "repo")
(define *sha1* "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(define *sha1-path* "aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(define *time*
  '("Mon Dec 12 15:10:35 2011" . 2))
(define *messages*
  '("slain the Jabberwock"
    "tagged the Jabberwock slain"))
(define *signature*
  '("Bandersnatch" . "banders@example.com"))
(define *files*
  '("calloo" "callay" "frabjous" "day"))
(define *content*
  '("Twas brillig, and the slithy toves"
    "Did gyre and gimble in the wabe:"
    "All mimsy were the borogoves,"
    "And the mome raths outgrabe."))
(define *config*
  '("[core]"
    "repositoryformatversion = 0"
    "filemode = true"))

(test-group "git"

  (test-group "oid"
    (test-error (string->oid 42))
    (test-error (string->oid "jubjub"))
    (let ((id (string->oid *sha1*)))
      (test #t (oid? id))
      (test *sha1* (oid->string id))
      (test *sha1-path* (oid->path id))))

  (test-group "signature"
    (test-error (make-signature 42))
    (test-error (make-signature 'snicker 'snack))
    (test-error (make-signature "not" "a" "real" "time"))
    (let* ((then (utc-time->seconds (string->time (car *time*))))
           (sig (make-signature (car *signature*) (cdr *signature*) then (cdr *time*))))
      (test #t (signature? sig))
      (test then (signature-time sig))
      (test (cdr *time*) (signature-time-offset sig)))
    ;; XXX I can imagine a horribly slow machine on which this might fail.
    (let ((now (current-seconds))
          (sig (make-signature (car *signature*) (cdr *signature*))))
      (test #t (signature? sig))
      (test (car *signature*) (signature-name sig))
      (test (cdr *signature*) (signature-email sig))
      (test now (signature-time sig))))

  (test-group "repository creation"
    (test-error (create-repository 11))
    (let ((repo (create-repository *repository*)))
      (test #t (directory? *repository*))
      (test #t (repository? repo))
      (test #f (repository-bare? repo))
      (test (normalize-pathname
              (format "~a/~a/.git/"
                      (current-directory)
                      *repository*))
            (repository-path repo))))

  (let ((repo (repository-open *repository*)))

    (test-group "empty repository"
      (test #t (repository-empty? repo))
      (test 0 (length (tags repo)))
      (test 0 (length (commits repo)))
      (test 0 (length (references repo))))

    (test-group "config"
      (test-error (config-open 9))
      (test-error (config-open "not-a-config"))
      (test-error (config-open repo))
      (let ((path (make-pathname *repository* ".git/config")))
        (with-output-to-file path
          (lambda () (for-each write-line *config*)))
        (test #t  (config? (config-open path)))
        (test #t  (config? (config-open repo))))
      (let ((cfg (config-open repo)))
        (test-error  (config-get cfg "not-a-value"))
        (test-error  (config-get cfg "core.not-a-value"))
        (test "true" (config-get cfg "core.filemode"))
        (test "true" (config-get cfg "core.filemode" 'string))
        (test #t     (config-get cfg "core.filemode" 'boolean))
        (test-error  (config-get cfg "core.filemode" 'number))
        (test "0"    (config-get cfg "core.repositoryformatversion"))
        (test "0"    (config-get cfg "core.repositoryformatversion" 'string))
        (test 0      (config-get cfg "core.repositoryformatversion" 'number))
        (test #f     (config-get cfg "core.repositoryformatversion" 'boolean))
        (test-assert (config-set cfg "diff.external" "/an/awesome/path"))
        (test "/an/awesome/path" (config-get cfg "diff.external"))
        (test "/an/awesome/path" (config-get cfg "diff.external" 'string))
        (test-error  (config-get cfg "diff.external" 'number))
        (test-error  (config-get cfg "diff.external" 'boolean))
        (test-assert (config-unset cfg "diff.external"))
        ;; XXX there is an inconsistency here: libgit2 returns
        ;; #f for deleted values rather than an error until the
        ;; configuration object is freed. After recreating the
        ;; object, it reports an error as expected:
        (test #f (config-get cfg "diff.external"))
        (let ((cfg* (config-open repo)))
          (test-error (config-get cfg* "diff.external")))))

    (let ((ix (index-open repo)))

      (test-group "index"
        (test #t (index? ix))
        (test 0 (index-entrycount ix))
        (test 0 (length (index->list ix)))
        (test-error (index-add ix "not-a-file"))
        (test #f (index-remove ix "not-a-file"))
        (test #f (index-ref ix -1))
        (test #f (index-ref ix "not-a-file"))
        (test-group "index-entry"
          (parameterize ((current-directory *repository*))
            (for-each
              (lambda (file content i)
                (with-output-to-file file
                  (lambda ()
                    (display content)))
                (test-assert (index-add ix file))
                (test-assert (index-ref ix i))
                (let ((ie (index-ref ix file))
                      (st (file-stat file)))
                  (test #t (index-entry? ie))
                  (test 0 (index-entry-stage ie))
                  (test file (index-entry-path ie))
                  (test (vector-ref st 0) (index-entry-ino ie))
                  (test (vector-ref st 1) (index-entry-mode ie))
                  (test (vector-ref st 3) (index-entry-uid ie))
                  (test (vector-ref st 4) (index-entry-gid ie))
                  (test (vector-ref st 5) (index-entry-size ie))
                  (test (vector-ref st 7) (index-entry-ctime ie))
                  (test (vector-ref st 8) (index-entry-mtime ie))))
            *files*
            *content*
            (iota (length *files*))))
        (test (length *files*) (index-entrycount ix))
        (test (length *files*) (length (index->list ix)))))

    (test-group "tree creation"
      (test-error (create-tree 1 2 3 4))
      (test-error (create-tree repo))
      (let ((tr (create-tree repo ix)))
        (test #t (tree? tr)))
      (let ((db (odb-open repo))
            (tb (make-tree-builder)))
        (for-each
          (lambda (file content)
            (let* ((bl (odb-write db (string->blob content)))
                   (te (tree-builder-insert tb bl file 33188)))
              (test #t (tree-entry? te))
              (test-assert (tree-builder-ref tb file))))
          *files*
          *content*)
        (let ((tr (tree-builder-write repo tb)))
          (test #t (tree? tr)))))

    (test-group "commit creation"
      (test-error (create-commit 1 2 3 4))
      (test-error (create-commit repo))
      (let* ((tr (create-tree repo ix))
             (cmt (create-commit repo
                           tree: tr
                      reference: "HEAD"
                        message: (car *messages*)
                         author: (make-signature
                                   (car *signature*)
                                   (cdr *signature*)))))
        (test #t (commit? cmt)))
      (test 1 (length (commits repo)))
      (test #f (repository-empty? repo))))

    (let ((cmt (car (commits repo))))

      (test-group "commit"
        (test-error (commit repo 0))
        (test-error (commit repo *sha1*))
        (test #t (commit? cmt))
        (test #t (oid? (object-id cmt)))
        (test 'commit (object-type cmt))
        (test #t (oid? (commit-id cmt)))
        (test #t (tree? (commit-tree cmt)))
        (test #t (signature? (commit-author cmt)))
        (test #t (signature? (commit-committer cmt)))
        (test 0 (commit-parentcount cmt))
        (test (car *messages*) (commit-message cmt))
        (test-assert (commit repo cmt))
        (test-assert (commit repo (commit-id cmt))))

      (test-group "reference creation"
        (test-error (create-reference 1 2 3 4))
        (test-error (create-reference repo))
        (let ((ref (create-reference repo
                             target: cmt
                               name: "refs/heads/mimsy")))
          (test #t (reference? ref))
          (let ((ref (create-reference repo
                               target: ref
                                 name: "TAIL"
                             symbolic: 'yes)))
            (test #t (reference? ref))
            (test 'symbolic (reference-type ref))))
        (test 2 (length (references repo))))

      (test-group "reference"
        (test-error (reference repo 0))
        (test-error (reference repo *sha1*))
        (let ((ref (reference repo "TAIL")))
          (test #t (reference? ref))
          (test #t (oid? (reference-id ref)))
          (test #t (reference? (reference-resolve ref)))
          (test 'symbolic (reference-type ref))
          (test "TAIL" (reference-name ref))
          (test "refs/heads/mimsy" (reference-target ref))
          (test-error (reference-target (reference-resolve ref)))
          (test-error (reference-target-set ref cmt))
          (test-error (reference-rename ref "refs/heads/mimsy"))
          (test-assert (reference-rename ref "BACK"))
          (test "BACK" (reference-name ref))
          (test 'symbolic (reference-type ref))
          (test-assert (reference repo (reference-name ref)))
          (let ((tgt (reference repo (reference-target ref))))
            (test 'oid (reference-type tgt))
            (test-assert (commit repo (reference-id tgt))))))

      (test-group "reference deletion"
        (test-error (reference-delete 0))
        (test-error (reference-delete "mimsy"))
        (test-error (reference-delete "refs/heads/mimsy"))
        (let* ((ref (reference repo "BACK"))
               (tgt (reference-target ref)))
          (test-assert (reference-delete ref))
          (test-assert (reference-delete (reference repo tgt)))
          (test 1 (length (references repo)))))

      (test-group "tag creation"
        (test-error (create-tag 1 2 3 4))
        (test-error (create-tag repo))
        (let ((tg (create-tag repo
                      target: cmt
                        name: "0.0.1-β"
                     message: (cadr *messages*)
                      tagger: (make-signature
                                (car *signature*)
                                (cdr *signature*)))))
          (test #t (tag? tg)))
        (test 1 (length (tags repo))))

      (test-group "tag"
        (test-error (tag repo 0))
        (test-error (tag repo *sha1*))
        (let ((tg (car (tags repo))))
          (test #t (oid? (tag-id tg)))
          (test #t (oid? (object-id tg)))
          (test 'tag (object-type tg))
          (test #t (signature? (tag-tagger tg)))
          (test "0.0.1-β" (tag-name tg))
          (test (cadr *messages*) (tag-message tg))
          (test-assert (tag repo tg))
          (test-assert (tag repo (tag-id tg)))))

      (test-group "tag deletion"
        (test-error (tag-delete 0))
        (test-error (tag-delete "not-a-tag"))
        (let ((tg (car (tags repo))))
          (test-assert (tag-delete tg)))
        (test 0 (length (tags repo))))

      (test-group "blob*"
        (test-error (blob* repo 0))
        (test-error (blob* repo *sha1*))
        (let ((tr (commit-tree cmt)))
          (parameterize ((current-directory *repository*))
            (for-each
              (lambda (file content i)
                (let ((bl (tree-entry->object repo (tree-ref tr file))))
                  (test #t (blob*? bl))
                  (test #t (oid? (object-id bl)))
                  (test 'blob (object-type bl))
                  (test (file-size file) (blob*-size bl))
                  (test-assert (blob* repo bl))
                  (test-assert (blob* repo (object-id bl)))
                  (test content (blob->string (blob*-content bl)))))
              *files*
              *content*
              (iota (length *files*))))))

      (test-group "tree"
        (test-error (tree repo 0))
        (test-error (tree repo *sha1*))
        (let ((tr (commit-tree cmt)))
          (test #t (tree? tr))
          (test #t (oid? (tree-id tr)))
          (test #t (oid? (object-id tr)))
          (test 'tree (object-type tr))
          (test (length *files*) (tree-entrycount tr))
          (test (length *files*) (length (tree->list tr)))
          (test-assert (tree repo tr))
          (test-assert (tree repo (tree-id tr)))
          (test-group "tree-entry"
            (test #f (tree-ref tr -1))
            (test #f (tree-ref tr "not-a-file"))
            (parameterize ((current-directory *repository*))
              (for-each
                (lambda (file content i)
                  (test-assert (tree-ref tr i))
                  (test-assert (tree-ref tr file))
                  (let ((te (tree-ref tr file)))
                    (test #t (tree-entry? te))
                    (test #t (oid? (tree-entry-id te)))
                    (test file (tree-entry-name te))
                    (test #t (blob*? (tree-entry->object repo te)))
                    (test 'blob (tree-entry-type te))
                    (test (vector-ref (file-stat file) 1) (tree-entry-attributes te))))
                *files*
                *content*
                (iota (length *files*)))))))

      (test-group "repository"
        (test-error (repository-ref repo))
        (test-error (repository-ref repo 0))
        (test #f (repository-ref repo "not-a-sha"))
        (test #f (repository-ref repo *sha1*))
        (test #t (commit? (repository-ref repo cmt)))
        (test #t (commit? (repository-ref repo (commit-id cmt))))
        (test #t (commit? (repository-ref repo (oid->string (commit-id cmt)))))
        (test #t (commit? (repository-ref repo (reference repo "HEAD"))))
        (test #t (tree? (repository-ref repo (commit-tree cmt)))))

      (test-group "odb"
        (let ((db (odb-open repo)))
          (test #t (odb? db))
          (test-error (odb-has-object db 0))
          (test-error (odb-has-object db "not-an-object"))
          (let ((tr (commit-tree cmt)))
            (test #t (odb-has-object? db tr))
            (test-group "odb-object"
              (let ((obj (odb-read db tr)))
                (test 'tree (odb-object-type obj))
                (test (oid->string (object-id tr))
                      (oid->string (odb-object-id obj))))
              (for-each
                (lambda (file content)
                  (let* ((bl (tree-entry->object repo (tree-ref tr file)))
                         (id (oid->string (object-id bl)))
                         (data (string->blob content)))
                    (test #t (odb-has-object? db bl))
                    (test #t (oid? (odb-hash data)))
                    (test id (oid->string (odb-hash data)))
                    (test id (oid->string (odb-write db data)))
                    (let ((obj (odb-read db id)))
                      (test data (odb-object-data obj))
                      (test 'blob (odb-object-type obj))
                      (test (blob-size data) (odb-object-size obj))
                      (test id (oid->string (odb-object-id obj))))))
                *files*
                *content*))))))))

(delete-directory *repository* 'recursively)
(test-exit)
