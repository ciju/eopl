(define-datatype env-exp env-exp?
  (empty-env)
  (extend-env
   (var symbol?)
   (val symbol?)
   (embed-env env-exp?)))

(define has-binding?
  (lambda (search-var env)
    (cases env-exp env
         (empty-env false)
         (extend-env (var val embed-env)
                     (if (= search-var var)
                       true
                       (has-binding? search-var embed-env))))))


#lang eopl

(define identifier? 
  (lambda (x)
  (and (not (equal? 'lambda x)) (symbol? x))))

(define-datatype lc-*exp lc-*exp?
  (var-exp* (var identifier?))
  (lambda-exp*
   (bound-vars (list-of identifier?))
   (body lc-*exp?))
  (app-exp*
   (rator lc-*exp?)
   (rand (list-of lc-*exp?))))

(define parse-lc-*exp 
  (lambda (datum)
  (cond
   ((symbol? datum) (var-exp* datum))
   ((pair? datum)
    (if (equal? (car datum) 'lambda)
        (lambda-exp* (cadr datum) (parse-lc-*exp (cadr (cdr datum))))
        (app-exp* (parse-lc-*exp (car datum)) (map parse-lc-*exp (cdr datum))))))))

(parse-lc-*exp 'x)
(parse-lc-*exp '(x y))
(parse-lc-*exp '(lambda (x) (x y z)))




