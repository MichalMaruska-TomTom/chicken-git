;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git.scm - libgit2 bindings for Chicken Scheme
;;
;; Copyright (c) 2011, Evan Hanson
;; See LICENSE for details
;;
;; Just a stub. Might be home to a nice API someday.

(module git
  (repository-open repository-path repository-index repository-odb repository-workdir
   repository-ref reference-name reference-oid reference-target reference-type)
  (import scheme files
    (except chicken repository-path)
    (prefix git-lolevel git-))
  (require-library git-lolevel)

(define-record repository
  pointer path index odb workdir)

(define-record reference
  pointer name oid target type)

(define (repository-open path)
  ;; Try opening path as a "normal" repo first
  ;; (i.e. a workdir with a '.git' directory),
  ;; and if that doesn't work try as a "bare" repo.
  (let* ((path (normalize-pathname path))
         (repo (call-with-current-continuation
                (lambda (return)
                  (condition-case
                    (return (git-repository-open (string-append path "/.git")))
                    (exn () (git-repository-open path)))))))
    ;; Not sure what to store here...
    ;; TIME WILL TELL, WON'T IT?
    (make-repository repo
      (git-repository-path repo 'path)
      (git-repository-path repo 'index)
      (git-repository-path repo 'odb)
      (git-repository-path repo 'workdir))))

(define-record-printer (repository r out)
  (display (format "#<repository ~S>" (repository-path r)) out))

(define (repository-ref repo ref)
  (and-let* ((ref (git-reference-resolve
                    (git-reference-lookup
                      (repository-pointer repo)
                      ref))))
    (make-reference ref
      (git-reference-name ref)
      (git-reference-oid ref)
      (git-reference-target ref)
      (git-reference-type ref))))

(define-record-printer (reference r out)
  (display (format "#<reference ~S>" (reference-name r)) out)))
