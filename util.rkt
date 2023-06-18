#lang racket/base

(require scribble/manual
         (for-syntax racket/base
                     (prefix-in c: scribble/comment-reader)
                     syntax/strip-context
                     compiler/cm-accomplice))

(provide racketmodfile
         cursor
         pyblock
         table
         pycode)

(define-syntax-rule (pycode content)
  (code #:lang "scribble/base" content))

(define-syntax-rule (table columns content ...)
  (tabular #:style 'boxed
           #:column-properties '(left right)
           #:row-properties '(bottom-boarder ())
           (list (map bold columns) content ...)))

(define-syntax-rule (pyblock content ...)
  (codeblock #:keep-lang-line? #f "#lang scribble/base\n" content ...))

(define-syntax-rule (cursor poet poem ...)
    (nested (italic poem ...) (verbatim (smaller (string-append "--" poet)))))

(define-syntax (racketmodfile stx)
  (syntax-case stx ()
    [(_ file displayed-name)
     (let ([f (path->complete-path (string-append "examples/" (syntax-e #'file))
                                   (or (current-load-relative-directory)
                                       (current-directory)))])
       (register-external-file f)
       (with-syntax ([(content ...)
                      (call-with-input-file* 
                       f
                       (lambda (in)
                         (read-bytes 6 in)
                         (port-count-lines! in)
                         (let loop ()
                           (let ([v (c:read-syntax (object-name in) in)])
                             (if (eof-object? v) 
                                 null 
                                 (cons (replace-context #'file v) 
                                       (loop)))))))])
         #'(racketmod #:file displayed-name content ...)))]
    [(_ file)
     #'(racketmodfile file file)]))

