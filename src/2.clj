;; Implement the four required operations for bigits. Then use your
;; implementation to calculate the factorial of 10. How does the
;; execution time vary as this argument changes? How does the excution
;; time vary as the base changes? Explain why.

(ns eopl.chap-2
  (:use clojure.test))


;; ex: 2.1

(defn base [] 16)

(defn zero [] '())

(defn is-zero? [n] (empty? n))

(defn successor [n]
  (let [ lsb (first n), r (rest n) ]
    (if (nil? lsb)
      (list 1)
      (if (= lsb (dec (base)))
        (cons 0 (successor r))
        (cons (inc lsb) r)))))

(defn predecessor [n]
  (let [ lsb (first n), r (rest n) ]
    (if (= lsb 1)
      (if (empty? r)
        (zero)
        (cons (* (first r) (base)) (rest r)))
      (cons (dec lsb) r))))


(defn times [fn p n]
  (if (= n 1)
    (fn p)
    (fn (times fn p (dec n)))))

(defn factorial [n]
  (if (= n 1)
    (successor (zero))
    (times successor (factorial (dec n)) n)))
  
        
(deftest bigits-test
  (is (is-zero? (predecessor (successor (zero))))))


;; ex: 2.3
;; an interesting representation. is there any learning ?
;; amortized cost ?
;; symbol manipulation

(defn zero [] '(diff (one) (one)))

(defn diff-left [d]
  (first (rest d)))
(defn diff-right [d]
  (second (rest d)))

(defn canonical-negative? [n]
  (= (diff-left n) (zero)))

(defn canonical-positive? [n]
  (not (canonical-negative? n)))


(defn canonical-negate [n]
  (if (= (zero) n)
    n
    (if (canonical-negative? n)
      (second (rest n))
      (list 'diff (zero) n))))

(defn canonical-successor [n]
  (if (= (zero) n)
    (list 'diff '(one) (zero))
    (if (canonical-negative? n)
      (canonical-negate (canonical-predecessor (canonical-negate n)))
      (list 'diff '(one) (canonical-negate n)))))
      
(defn canonical-predecessor [n]
  (if (= (zero) n)
    (canonical-negate (canonical-successor n))
    (if (canonical-negative? n)
      (canonical-negate (canonical-successor (canonical-negate n)))
      (canonical-negate (second (rest n))))))


;; fuck?
(defn canonical [n cn cl cr]
  (if (nil? n)
    (zero)
    (cond (and (= (diff-left n) '(one))
               (= (diff-right n) '(one)))
          cn

          (= (diff-left n) '(one))
          (canonical (diff-right n) (cl cn) cr cl)

          (= (diff-right n) '(one))
          (canonical (diff-left n) (cr cn) cl cr)

          :else (canonical (diff-left n)
                           (canonical (diff-right n) cn cr cl)
                           cl cr))))


(defn is-zero? [n]
  (= (canonical n (zero) canonical-successor canonical-predecessor) (zero)))

(defn predecessor [n]
  (list 'diff n '(one)))

(defn successor [n]
  (list 'diff n (predecessor (zero))))

(defn diff-tree-plus [n1 n2]
  (list 'diff n1 (list 'diff (zero) n2)))


;; ex: 2.5
(defn empty-env []
  '())

(defn extend-env [var val env]
  (cons [var val] env))

;; error shit ?
(defn apply-env [env search-var]
  (if (= env (empty-env))
    (let [bding (first env)]
      (if (= (first bding) search-var)
        (rest bding)
        (recur (rest env) search-var)))))


;; ex: 2.8
(defn empty-env? [env]
  (= env (empty-env)))

;; ex: 2.9
(defn has-binding? [env search-var]
  (if (not (empty-env? env))
    (let [bding (first env)]
      (if (= (first bding) search-var)
        true
        (recur (rest env) search-var)))
    false))

(def test-binding
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'x 7
                                      (extend-env 'y 14
                                                  (empty-env))))))

;; ex: 2.10
;; assumes vars and vals r equal length
(defn extend-env* [vars vals env]
  (if (empty? vars)
    env
    (recur (rest vars) (rest vals)
           (extend-env (first vars) (first vals) env))))

;; ex: 2.11
(defn empty-env []
  '())

(defn extend-env [var val env]
  (cons [var val] env))

(defn check-lst [vars vals search-var]
  (if (not (seq? vars))
    (if (= vars search-var) vals)
    (if (empty? vars)
      false
      (if (= (first vars) search-var)
        (first vals)
        (check-lst (rest vars) (rest vals) search-var)))))

(defn apply-env [env search-var]
  (if (not (= env (empty-env)))
    (let [bding (first env)
          res (check-lst (first bding) (second bding) search-var)]
      (if (= res false)
        (apply-env (rest env) search-var)
        res))
    false))
    
      
(defn extend-env* [vars vals env]
  (cons [vars vals] env))

;; (apply-env (extend-env* '(x y n) '(1 2 3) test-binding) 'd)

;; ex: 2.12

(defn empty-stack []
  (fn [op]
    (if (= op 'empty-stack?)
      true
      (throw (IllegalArgumentException.)))))

(defn spush [x stack]
  (fn [op]
    (case op
      'empty-stack? false
      'top x
      'pop stack
      :else (throw (IllegalArgumentException.)))))


(defn empty-stack? [stack]
  (stack 'empty-stack?))

(defn spop [stack]
  (stack 'pop))

(defn stop [stack]
  (stack 'top))


;;; ex: 2.13
(defn empty-env []
  [ (fn [search-var]
      (throw (Exception.)))
    (fn [] true)])

(defn extend-env [saved-var saved-val saved-env]
  [ (fn [search-var]
      (if (= search-var saved-var)
        saved-val
        (apply-env saved-env search-var)))
    (fn [] false)])

(defn apply-env [env search-var]
  ((first env) search-var))

(defn empty-env? [env]
  ((second env)))


;;; ex: 2.14

(defn empty-env []
  [ (fn [search-var]
      (throw (Exception.)))
    (fn [search-var] false)
    (fn [] true)])

(defn extend-env [saved-var saved-val saved-env]
  (defn apply-op [op op-res]
    (fn [search-var]
      (if (= search-var saved-var)
        (op-res saved-val)
        (op saved-env search-var))))
  
  [ (apply-op apply-env (fn [val] val))
    (apply-op has-binding? (fn [val] true))
    (fn [] false)])

(defn apply-env [[apply & _] search-var]
  (apply search-var))

(defn has-binding? [[_ binding? _] search-var]
  (binding? search-var))

(defn empty-env? [[_ _ efn]] ( efn))


;;; ex: 2.15
(defn var-exp [var] var)
(defn lambda-exp [var exp]
  (list 'lambda (list var) exp))
(defn app-exp [exp1 exp2]
  (list exp1 exp2))

(defn var-exp? [exp]
  (symbol? exp))
(defn lambda-exp? [exp]
  (and (= (first exp) 'lambda)
       (seq? exp)))
(defn app-exp? [exp]
  (and (seq? exp)
       (= (count exp) 2)))


(defn var-exp->var [exp] exp)
(defn lambda-exp->bound-var [exp]
  (first (second exp)))
(defn lambda-exp->body [exp]
  (first (rest (rest exp))))
(defn app-exp->rator [exp]
  (first exp))
(defn app-exp->rand [exp]
  (second exp))

(defn occurs-free? [search-var exp]
  (cond (var-exp? exp) (= search-var (var-exp->var exp))
        (lambda-exp? exp) (and (not (= search-var (lambda-exp->bound-var exp)))
                               (recur search-var (lambda-exp->body exp)))
        :else (or (occurs-free? search-var (app-exp->rator exp))
                  (occurs-free? search-var (app-exp->rand exp)))))


;;; ex: 2.16

(defn lambda-exp [var exp]
  (list 'lambda var exp))
(defn lambda-exp->bound-var [exp]
  (second exp))

;;; ex: 2.18
(defn number->sequence [n]
  (list n () ()))
;;; (number->sequence 7)

(defn current-element [[curr front back]]
  curr)
;;; (current-element '(6 (5 4 3 2 1) (7 8 9)))

(defn move-to-left [[curr front back]]
  (list (first front) (rest front) (cons curr back)))
;;; (move-to-left '(6 (5 4 3 2 1) (7 8 9)))

(defn move-to-right [[curr front back]]
  (list (first back) (cons curr front) (rest back)))
;;; (move-to-right '(6 (5 4 3 2 1) (7 8 9)))zo
(defn insert-to-left [n [curr front back]]
  (list curr (cons n front) back))
;;; (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
(defn insert-to-right [n [curr front back]]
  (list curr front (cons n back)))
;;; (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))
(defn at-left-end? [[curr front back]]
  (= () front))
(defn at-right-end? [[curr front back]]
  (= () back))

;;; ex: 2.19
(defn number->bintree [n]
  (list n '() '()))
(defn current-element [[n lbt rbt]] n)
(defn insert-to-left [n [cn lbt rbt]]
  (list cn (list n lbt ()) rbt))
(defn insert-to-right [n [cn lbt rbt]]
  (list  cn lbt (list n () rbt)))

(defn at-leaf? [[cn lbt rbt]]
  (and (nil? lbt) (nil? rbt)))
(defn move-to-left [[cn lbt rbt]] lbt)
(defn move-to-right [[cn lbt rbt]] rbt)
;;; (number->bintree 13)
;;; (insert-to-left 15 (insert-to-right 14 (insert-to-left 12 (number->bintree 13))))
;;; (at-leaf? (move-to-right (move-to-left (insert-to-right 14 (insert-to-left 12 (number->bintree 13))))))

;;; ex: 2.20
(defn number->bintree [n]
  (list (list n '() '()) (list '() '() '())))
(defn current-element [[[n lbt rbt] [rn rlbt rrbt]]]
  n)
(defn insert-to-left [n [ [cn lbt rbt] backbt ]]
  (list
   (list cn (list n lbt ()) rbt)
   backbt))
(defn insert-to-right [n [ [cn lbt rbt] backbt ]]
  (list
   (list cn lbt (list n () rbt))
   backbt))

(defn at-leaf? [[ [cn lbt rbt] [rb rlbt rrbt] ]]
  (and (nil? lbt) (nil? rbt)))
(defn move-to-left [[ [cn lbt rbt] [rb rlbt rrbt] ]]
  (list lbt
        (list (cons cn rb) (cons () rlbt) (cons rbt rrbt))))
(defn move-to-right [[ [cn lbt rbt] [rb rlbt rrbt] ]]
  (list rbt
        (list (cons cn rb) (cons lbt rlbt) (cons () rrbt))))

(defn move-up [[ cbt [rb rlbt rrbt] ]]
  (println (first rlbt) (empty? (first rlbt)))
  (list
   (if (empty? (first rlbt))
     (list (first rb) cbt (first rrbt))
     (list (first rb) (first rlbt) cbt))
   (list (rest rb) (rest rlbt) (rest rrbt))))
(defn at-root? [[ [cn lbt rbt] [rb rlbt rrbt] ]])



;;; ex: 2.21

(ns user (:use define-datatype))

(define-datatype env-exp env-exp?
  (empty-env)
  (non-empty-env
   (var symbol?)
   (val symbol?)
   (embed-env env-exp?)))

(defn has-binding? [search-var env]
  (cases env-exp env
         (empty-env () false)
         (non-empty-env (var val embed-env)
                     (if (= search-var var)
                       true
                       (has-binding? search-var embed-env)))))

;;; ex: 2.22
(define-datatype stack stack?
  (empty-stack)
  (push
   (val #(-> %))
   (remaining stack?)))

(defn pop [s]
  (cases stack s
         (empty-stack () '())
         (push (val remaining)  remaining)))
(defn top [s]
  (cases stack s
         (empty-stack () '())
         (push (val _) val)))
(defn empty-stack? [s]
  (cases stack s
         (empty-stack () true)
         (push (val _) false)))

;;; ex: 2.23
(defn identifier? [x]
  (and (not (= 'lambda x)) (symbol? x)))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

;;; ex: 2.24
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(defn bintree-to-list [bt]
  (cases bintree bt
         (leaf-node (num) (list 'leaf-node num))
         (interior-node (key left right) (list 'interior-node key
                                               (bintree-to-list left)
                                               (bintree-to-list right)))))

;;; ex: 2.25
(defn max-interior [bt]
  (cases bintree bt
         (leaf-node (num) (list num num 'leaf))
         (interior-node
          (key left right)
          (let [l (max-interior left)
                r (max-interior right)
                ln (second l) rn (second r)
                la (first l) ra (first r)
                t (+ la ra)
                mx (max ln rn t)
                ]
            (cond
             (= mx t) (list t t key)
             (= mx ln) (cons t (rest l))
             (= mx rn) (cons t (rest r)))))))

;;; ex: 2.26
(defn list-of [pred]
  (fn [v]
    (or (empty? v)
        (and (seq? v)
             (pred (first v))
             ((list-of pred) (rest v))))))

(define-datatype rb-tree rb-tree?
  (leaf-node
   (num integer?))
  (red-node
   (left rb-tree?)
   (right rb-tree?))
  (blue-node
   (rb-trees (list-of rb-tree?))))

(defn make-red-count-tree [rbt cnt]
  (cases rb-tree rbt
         (leaf-node (num)
                    (leaf-node cnt))
         (red-node (left right)
                   (red-node (make-red-count-tree left (+ cnt 1))
                             (make-red-count-tree right (+ cnt 1))))
         (blue-node (rb-trees)
                    (blue-node (map #(-> (make-red-count-tree % cnt)) rb-trees)))))


;;; ex: 2.28
(defn unparse-lc-exp [exp]
  (cases lc-exp exp
         (var-exp (var)
                  (str var))
         (lambda-exp (bound-var body)
                     (str "proc " bound-var " => " (unparse-lc-exp body)))
         (app-exp (rator rand)
                  (str (unparse-lc-exp rator) "(" (unparse-lc-exp rand) ")"))))

;;; ex: 2.29
;;; todo: fix the syntax
(define-datatype lc-*exp lc-*exp?
  (var-exp* (var identifier?))
  (lambda-exp*
   (bound-vars (list-of identifier?))
   (body lc-*exp?))
  (app-exp*
   (rator lc-*exp?)
   (rand (list-of lc-*exp?))))


(defn parse-lc-*exp [datum]
  (cond
   (symbol? datum) (var-exp* datum)
   (seq? datum)
   (if (= (first datum) 'lambda)
     (lambda-exp* (second datum) (parse-lc-*exp (second (rest datum))))
     (app-exp* (parse-lc-*exp (first datum)) (map parse-lc-*exp (rest datum))))))

;;; ex: 2.31
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(defn diff-exp? [e]
  (= '- e))

(defn const-exp? [e]
  (number? e))

(defn gen-prefix-exp [rst]
  (if (diff-exp? (first rst))
    (let [[ex-left rst-left] (gen-prefix-exp (rest rst))
          [ex-right rst-right] (gen-prefix-exp rst-left)]
      (list (diff-exp ex-left ex-right) rst-right))
    (list (const-exp (first rst)) (rest rst))))



;; (run-tests)




