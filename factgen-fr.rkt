;; fact generator for fully expanded racket

;; Yihao Sun

#lang racket

(require racket/cmdline)
(require racket/sequence)
(require net/uri-codec)
(require "full-expanded.rkt")

;; function gen take a racket term into a IR:
;; ID
;; (TYPE id IR ...)
;; (LIST IR)

(define (fact-type? ft)
  (member ft '(symbol number string ldata formals plain_lambda lambda_bodies
                      case_lambda case_lambda_cases if begin begin0 let_values
                      binds bound_vars letrec_values setb quote quote_syntax
                      quote_syntax_local with_continuation_mark plain_app
                      lam_apply_args variable_reference variable_reference_top
                      defined_vars defined_values define_syntaxes require
                      provide module_level_forms module_level_form begin_for_syntax
                      module module_star declare expression)))

#;(define (ir? i)
  (match i
    [`(,(? fact-type?) ,id ,xs ...) #t]
    [`(,())]))

(define (id-of-ir f)
  (match f
    [`(,(? fact-type?) ,id ,res ...) id]
    [_ (id-of-ir (first f))]))

(define (gen-dt d)
  (match d
    [(? symbol?) `(symbol ,(gensym 'symbol) ,d)]
    [(? number?) `(number ,(gensym 'number) ,d)]
    [(? string?) `(string ,(gensym 'string) ,(uri-encode d))]
    [(? empty?) `((ldata ,(gensym 'ldata) EMPTY))]
    [(? list?)
     (define fid (gensym 'ldata))
     (foldl (λ (x pos res)
              (cons
               `(ldata ,fid ,pos ,x)
               res))
            '()
            d
            (range (length d)))]))

(define (gen-formals expr)
  (match expr
    [`(,(? id? fargs) ... . ,(? id? rest-fargs))
     (define fid (gensym 'formals))
     (foldl (λ (a pos res)
              (cons `(formals ,fid ,pos ,a)
                    res))
            `((formals ,fid -1 ,rest-fargs))
            fargs
            (range (length fargs)))]
    [`(,(? id? fargs) ...)
     (define fid (gensym 'formals))
     (foldl (λ (a pos res)
              (cons `(formals ,fid ,pos ,a)
                    res))
            '()
            fargs
            (range (length fargs)))]
    [(? id? x) `((formals ,(gensym 'formals) -1 ,x))]))

(define (gen-lambda-bodies bodies)
  (define lambda-bodies-id (gensym 'lambda_bodies))
  (foldl (λ (b pos res)
           (cons `(lambda_bodies ,lambda-bodies-id ,pos
                                 ,(gen-expr b))
                 res))
         '()
         bodies
         (range (length bodies))))
(define (gen-bound-vars vs)
  (define bid (gensym 'bound-vars))
  (foldl (λ (a pos res)
           (cons `(bound-vars ,bid ,pos ,a)
                 res))
         '()
         vs
         (range (length vs))))
(define (gen-binds binds)
  (define binds-id (gensym 'binds))
  (foldl (λ (b pos res)
           (cons `(binds ,binds-id ,pos
                         ,(gen-bound-vars (first b))
                         ,(gen-expr (second b)))
                 res))
         '()
         binds
         (range (length binds))))
(define (gen-lam-apply-args args)
  (define lam-apply-args-id (gensym 'lam_apply_args))
  (foldl (λ (arg pos res)
           (cons `(lam_apply_args ,lam-apply-args-id ,pos ,(gen-expr arg))
                 res))
         '()
         args
         (range (length args))))

(define (gen-expr e)
  (match e
    [`(#%plain-lambda ,(? formals? fs) ,(? expr? bodies) ...)
     `(plain_lambda ,(gensym 'plain_lambda)
                    ,(gen-formals fs)
                    ,(gen-lambda-bodies bodies))]
    [`(case-lambda (,(? formals? fs-list) ,(? expr? bodies-list) ...) ...)
     (define clc-id (gensym 'case_lambda_cases))
     `(case_lambda ,(gensym 'case_lambda)
                   ,(foldl (λ (fs bodies pos res)
                             (cons `(case_lambda_cases ,clc-id , pos
                                                       ,(gen-formals fs)
                                                       ,(gen-lambda-bodies bodies))
                                   res))
                           fs-list
                           bodies-list
                           (range (length fs-list))))]
    [`(if ,(? expr? guard) ,(? expr? et) ,(? expr? ef))
     `(if ,(gensym 'if) ,(gen-expr guard) ,(gen-expr et) ,(gen-expr ef))]
    [`(begin ,(? expr? es) ...)
     (define begin-id (gensym 'begin))
     (foldl (λ (e pos res)
              (cons `(begin ,begin-id ,pos ,(gen-expr e))
                    res))
            '()
            es (range (length es)))]
    [`(begin0 ,(? expr? es) ...)
     (define begin0-id (gensym 'begin0))
     (foldl (λ (e pos res)
              (cons `(begin0 ,begin0-id ,pos ,(gen-expr e))
                    res))
            '()
            es (range (length es)))]
    ;; let bodies and lambda body are similar thing so ...
    [`(let-values ,binds
        ,(? expr? bodies) ...)
     `(let_values ,(gensym 'let_values)
        ,(gen-binds binds)
        ,(gen-lambda-bodies bodies))]
    [`(letrec-values ,binds
        ,(? expr? bodies) ...)
     `(letrec_values ,(gensym 'letrec_values)
                  ,(gen-binds binds)
                  ,(gen-lambda-bodies bodies))]
    [`(set! ,(? id? x) ,(? expr? e))
     `(setb ,(gensym 'setb) ,x ,(gen-expr e))]
    [`(quote ,d) `(quote ,(gensym 'quote) ,(gen-dt d))]
    [`(quote-syntax ,(? dt? d)) `(quote_syntax ,(gensym 'quote_syntax) ,(gen-dt d))]
    [`(quote-syntax ,(? dt? d) #:local)
     `(quote_syntax_local ,(gensym 'quote_syntax_local) ,(gen-dt d))]
    [`(with-continuation-mark ,(? expr? key-expr) ,(? expr? val-expr) ,(? expr? res-expr))
     `(with_continuation_mark ,(gensym 'with_continuation_mark)
                              ,(gen-expr key-expr)
                              ,(gen-expr val-expr)
                              ,(gen-expr res-expr))]
    [`(#%plain-app ,(? expr? es) ...)
     `(plain_app ,(gensym 'plain_app) ,(gen-lam-apply-args es))]
    [`(#%top . ,(? id? x))
     `(top ,(gensym 'top) ,x)]
    [`(#%variable-reference ,(? id? x))
     `(variable_reference ,(gensym 'variable_reference) ,x)]
    [`(#%variable-reference (#%top . ,(? id? x)))
     `(variable_reference_top ,(gensym 'variable_reference_top) ,x)]
    [`(#%variable-reference)
     `(variable_reference ,(gensym 'variable_reference) EMPTY)]))

(define (gen-defined-vars vs)
  (define defined-vars-id (gensym 'defined_vars))
  (foldl (λ (a pos res)
           (cons `(defined_vars ,defined-vars-id ,pos ,a)
                 res))
         '()
         vs
         (range (length vs))))

(define (gen-general-top-level-form gtlf)
  (match gtlf
    [(? expr?) (gen-expr gtlf)]
    [`(define-values (,(? id? xs) ...) ,(? expr? e))
     `(define_values ,(gensym 'define_values)
        ,(gen-defined-vars xs)
        ,(gen-expr e))]
    [`(define-syntaxes (,(? id? xs) ...) ,(? expr? e))
     `(define_syntaxes ,(gensym 'define_syntaxes)
        ,(gen-defined-vars xs)
        ,(gen-expr e))]
    ;; leave require and module at this time
    [`(#%require ,rrs ...)
     `(require ,(uri-encode (format "~s" rrs)))]))

(define (gen-submodule-form sf)
  (match sf
    [`(module ,(? id? name) ,modulde-path
        (#%plain-module-begin
         ,(? module-level-form? fs) ...))
     `(module ,(gensym 'module)
          ,name ,(uri-encode modulde-path)
          ,(gen-module-level-forms fs))]
    [`(module* ,(? id? name) ,modulde-path
        (#%plain-module-begin
         ,(? module-level-form? fs) ...))
     `(module ,(gensym 'module_star)
          ,name ,(uri-encode modulde-path)
          ,(gen-module-level-forms fs))]
    [`(module ,(? id? name) #f
        (#%plain-module-begin
         ,(? module-level-form? fs) ...))
     `(module ,(gensym 'module)
          ,name EMPTY
          ,(gen-module-level-forms fs))]))

(define (declaration-keyword->symbol k)
  (match k
    ['#:cross-phase-persistent 'cross_phase_persistent]
    ['#:empty-namespace 'empty_name_space]
    ['#:unsafe 'unsafe]))

(define (gen-module-level-form mlf)
  (match mlf
    [(? general-top-level-form?)
     (gen-general-top-level-form mlf)]
    [`(#%provide ,raw-provide-spec ...)
     `(provide ,(uri-encode (format "~s" raw-provide-spec)))]
    [`(begin-for-syntax ,(? module-level-form? fs) ...)
     (define begin-for-syntax-id (gensym 'begin_for_syntax))
     (foldl (λ (e pos res)
              (cons `(begin_for_syntax ,begin-for-syntax-id ,pos ,(gen-expr e))
                    res))
            '()
            fs (range (length fs)))]
    [(? submodule-form?) (gen-submodule-form mlf)]
    [`(#%declare ,declaration-keyword ...)
     `(declare ,(gensym 'declare)
               ,(declaration-keyword->symbol declaration-keyword))]))

(define (gen-module-level-forms fs)
  (define module-level-forms-id (gensym 'module_level_forms))
  (foldl (λ (f pos res)
           (cons `(module_level_forms ,module-level-forms-id ,pos
                                      ,(gen-module-level-form f))
                 res))
         '() fs (range (length fs))))


(define (gen-top-level-form tlf)
  (match tlf
    [(? general-top-level-form?) (gen-general-top-level-form tlf)]
    [`(#%expression ,(? expr? e))
     `(expression (gensym 'expression) ,(gen-expr e))]
    [`(module ,(? id? name) ,module-path
        (#%plain-module-begin
         ,(? module-level-form? fs) ...))
     `(module ,(gensym 'module)
          ,name ,(uri-encode module-path)
          ,(gen-module-level-forms fs))]
    [`(begin ,(? top-level-form? fs) ...)
     (define begin-id (gensym 'begin))
     (foldl (λ (e pos res)
              (cons `(begin ,begin-id ,pos ,(gen-expr e))
                    res))
            '()
            fs (range (length fs)))    ]
    [`(begin-for-syntax ,(? top-level-form? fs) ...)
     (define begin-for-syntax-id (gensym 'begin_for_syntax))
     (foldl (λ (e pos res)
              (cons `(begin_for_syntax ,begin-for-syntax-id ,pos ,(gen-expr e))
                    res))
            '()
            fs (range (length fs)))]))

;;
;; CLI
;;

(match-define `(,rkt-program ,fact-dir)
  (command-line
   #:program "generate souffle facts for a fully expanded racket program"
   #:args (rkt-program fact-dir)
   `(,rkt-program ,fact-dir)))

