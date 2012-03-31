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
(defn empty-stack
  (fn [] '()))
(defn push [x stack]
  (fn []
    (list 'push x stack)))
(defn pop
  (fn []
    (list 'pop stack)))
(defn top
  (list 'top stack))
(defn empty-stack?
  (


(run-tests)

