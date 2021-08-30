;; language predicate of fully expanded racket

;; Yihao Sun

#lang racket

(provide (all-defined-out))

(define (fkeyword? sym)
  (member sym '(#%expression module module* #%plain-module-begin begin begin-for-syntax
                             #%provide #%declare define-values define-syntaxes #%require
                             #%plain-lambda case-lambda if begin0 let-values letrec-values
                             set! quote quote-syntax with-continuation-mark #%plain-app
                          #%top #%variable-reference)))


;; L I S P data
(define (dt? d)
  (match d
    [(? symbol?) #t]
    [(? number?) #t]
    [(? string?) #t]
    [(? list?) #t]
    [_ #f]))

(define (id? sym)
  (and (symbol? sym)
       (not (fkeyword? sym))))

(define (formals? expr)
  (match expr
    [`(,(? id?) ... . ,(? id?)) #t]
    [`(,(? id?) ...) #t]
    [(? id?) #t]
    [_ #f]))

(define (expr? e)
  (match e
    [`(#%plain-lambda ,(? formals?) ,(? expr?) ...) #t]
    [`(case-lambda (,(? formals?) ,(? expr?) ...) ...) #t]
    [`(if ,(? expr?) ,(? expr?) ,(? expr?)) #t]
    [`(begin ,(? expr?) ...) #t]
    [`(begin0 ,(? expr?) ,(? expr?) ...) #t]
    [`(let-values ([(,(? id?) ...)
                    ,(? expr?)] ...)
        ,(? expr?) ...) #t]
    [`(letrec-values ([(,(? id?) ...)
                       ,(? expr?)] ...)
        ,(? expr?) ...) #t]
    [`(set! ,(? id?) ,(? expr?)) #t]
    [`(quote ,d) #t]
    [`(quote-syntax ,(? dt?)) #t]
    [`(quote-syntax ,(? dt?) #:local) #t]
    [`(with-continuation-mark ,(? expr?) ,(? expr?) ,(? expr?)) #t]
    [`(#%plain-app ,(? expr?) ...) #t]
    [`(#%top . ,(? id?)) #t]
    [`(#%variable-reference ,(? id?)) #t]
    [`(#%variable-reference (#%top . ,(? id?))) #t]
    [`(#%variable-reference) #t]
    [_ #f]))

(define (general-top-level-form? gtlf)
  (match gtlf
    [(? expr?) #t]
    [`(define-values (,(? id?) ...) ,(? expr?)) #t]
    [`(define-syntaxes (,(? id?) ...) ,(? expr?)) #t]
    ;; leave require and module at this time
    [`(#%require ,rrs ...) #t]
    [_ #f]))

(define (submodule-form? sf)
  (match sf
    [`(module ,(? id?) ,modulde-path
        (#%plain-module-begin
         ,(? module-level-form?) ...)) #t]
    [`(module* ,(? id?) ,modulde-path
        (#%plain-module-begin
         ,(? module-level-form?) ...)) #t]
    [`(module ,(? id?) #f
        (#%plain-module-begin
         ,(? module-level-form?) ...)) #t]
    [_ #f]))

(define (module-level-form? mlf)
  (match mlf
    [(? general-top-level-form?) #t]
    [`(#%provide ,raw-provide-spec ...) #t]
    [`(begin-for-syntax ,(? module-level-form?) ...) #t]
    [(? submodule-form?) #t]
    [`(#%declare ,declaration-keyword ...) #t]
    [_ #f]))

(define (top-level-form? tlf)
  (match tlf
    [(? general-top-level-form?) #t]
    [`(#%expression ,(? expr?)) #t]
    [`(module ,(? id?) ,module-path
        (#%plain-module-begin
         ,(? module-level-form?) ...)) #t]
    [`(begin ,(? top-level-form?) ...) #t]
    [`(begin-for-syntax ,(? top-level-form?) ...) #t]
    [_ #f]))

(define (declaration-keyword? k)
  (member k '(#:cross-phase-persistent #:empty-namespace #:unsafe)))
