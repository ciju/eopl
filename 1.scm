(define list-length 
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

;(= (list-length '(a b c)) 3)

; 1.7

(define report-list-too-short
  (lambda (n lst)
    (error 'nth-element
                "list too short by " (+ n 1) " elements " lst)))

(define nth-element-internal
  (lambda (lst n olist)
    (if (null? lst)
        (report-list-too-short n olist)
        (if (= n 0)
            (car lst)
            (nth-element-internal (cdr lst) (- n 1) olist)))))

(define nth-element 
  (lambda (lst n)
    (nth-element-internal lst n lst)))
    
;(nth-element '(a b c d) 3)
;(nth-element '(a b) 4)

;; ex: 1.9
(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))

;; (remove 'a '(a b c a d))
;; (remove 'a '())


;; ex: 1.13
(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))
        
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (map (lambda (sexp)
               (subst-in-s-exp new old sexp)) slist))))

;; (subst 'a 'b '(( b c) (b () d)))
            

; ex: 1.15
(define dup
  (lambda (n x)
    (if (= n 0)
        '()
        (cons x (dup (- n 1) x)))))
(dup 2 3)

;; ex: 1.16
(define invert-2list
  (lambda (lst)
    (if (null? lst)
        lst
        (list (cadr lst) (car lst)))))

(define invert-lst
  (lambda (lst)
    (if (null? lst)
        lst
        (cons (invert-2list (car lst)) (invert-lst (cdr lst))))))


;; ex: 1.17
(define down
  (lambda (lst)
    (map (lambda (le)
           (cons le '()) ) lst)))

;; (down '(1 2 3))
;; (down '((a) (fine) (idea)))
;; (down '(a (more (complicated)) object))

;; ex: 1.18
(define swap-in-sexp
  (lambda (s1 s2 sexp)
    (if (symbol? sexp)
        (if (eqv? sexp s1)
            s2
            (if (eqv? sexp s2)
                s1
                sexp))
        (swapper s1 s2 sexp))))

(define swapper
  (lambda (s1 s2 slist)
    (map (lambda (sexp)
           (swap-in-sexp s1 s2 sexp)) slist)))

;; (swapper 'a 'd '(a b c d))
;; (swapper 'a 'd '(a d () c d))
;; (swapper 'x 'y '((x) y (z (x))))

;; ex: 1.19
(define list-set
  (lambda (lst n x)
     (if (eqv? n 0)
         (cons x (cdr lst))
         (cons (car lst) (list-set (cdr lst) (- n 1) x)))))

;; (list-set '(a b c d) 2 '(1 2))
;; (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)

;; ex: 1.20
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (if (symbol? (car slist))
               (if (eqv? (car slist) s) 1 0)
               (count-occurrences s (car slist)))
           (count-occurrences s (cdr slist))))))

;; (count-occurrences 'x '((f x) y (((x z) x))))
;; (count-occurrences 'x '((f x) y (((x z) () x))))
;; (count-occurrences 'w '((f x) y (((x z) x))))

;; ex: 1.21
(define product-sub
  (lambda (s sos res)
    (if (null? sos)
        res
        (product-sub s (cdr sos) (cons (cons s (car sos)) res)))))

(define product
  (lambda (sos1 sos2)
    (if (null? sos1)
        '()
        (product-sub (car sos1) sos2 (product (cdr sos1) sos2)))))

;; (product '(a b c) '(x y))


;; ex: 1.22
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))
;; (filter-in number? '(a 2 (1 3) b 7))
;; (filter-in symbol? '(a (b c) 17 foo))

;; ex: 1.23
;; pretty fucked up method.
(define list-index
  (lambda (pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            0
            (if (number? (list-index pred (cdr lst)))
                (+ 1 (list-index pred (cdr lst)))
                #f)))))

;; (list-index number? '(a 2 (1 3) b 7))
;; (list-index symbol? '(a (b c) 17 foo))
;; (list-index symbol? '(1 2 (a b) 3))

;; ex: 1.24
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f))))

;; (every? number? '(a b c 3 e))
;; (every? number? '(1 2 3 5 4))

;; ex: 1.25
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            #t
            (exists? pred (cdr lst))))))

;; (exists? number? '(a b c 3 e))
;; (exists? number? '(a b c d e))

;; ex: 1.26
(define join-lst
  (lambda (lst res)
    (if (null? lst)
        res
        (if (pair? lst)
            (cons (car lst) (join-lst (cdr lst) res))
            (cons lst res)))))

(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (join-lst (car lst) (up (cdr lst))))))

;; (up '((1 2) (3 4)))
;; (up '((x (y)) z))

;; ex: 1.27
(define flatten
  (lambda (slist)
    (if (null? slist)
        '()
        (if (symbol? slist)
            slist
            (join-lst                   ;rest should be pair
             (flatten (car slist))
             (flatten (cdr slist)))))))

;; (flatten '(a b c))
;; (flatten '((a) () (b ()) () (c)))
;; (flatten '((a b) c (((d)) e)))
;; (flatten '(a b (() (c))))


;; ex: 1.28
(define merge
  (lambda (loi1 loi2)
    (join-lst loi1 loi2)))

;; (merge '(1 4) '(1 2 8))
;; (merge '(35 62 81 90 91) '(3 83 85 90))

;; ex: 1.29
(define sort-insert 
  (lambda (pred n lloi gloi)
    (if (null? gloi)
        (merge lloi (list n))
        (if (pred n (car gloi))
            (merge (merge lloi (list n)) gloi)
            (sort-insert pred n (merge lloi (list (car gloi))) (cdr gloi))))))
        
(define sort
  (lambda (loi)
    (if (null? loi)
        '()
        (sort-insert < (car loi) '() (sort (cdr loi))))))

;; (sort '(8 2 5 2 3))

;; ex: 1.30
(define sort/predicate 
  (lambda (pred loi)
    (if (null? loi)
        '()
        (sort-insert pred (car loi) '() (sort/predicate pred (cdr loi))))))
;; (sort/predicate < '(8 2 5 2 3))
;; (sort/predicate > '(8 2 5 2 3))


;; ex: 1.31
(define test-tree
  '(baz
      (bar 1 (foo 1 2))
      (biz 4 5)))

(define leaf
  (lambda (n)
    n))

(define interior-node
  (lambda (s r l)
    (list s r l)))

(define leaf?
  (lambda (bt)
    (number? bt)))

(define content-of 
  (lambda (bt)
    (if (leaf? bt)
        bt
        (car bt))))

(define lson
  (lambda (bt)
    (cadr bt)))
(define rson
  (lambda (bt)
    (caddr bt)))


;; ex: 1.32
(define double-tree
  (lambda (bt)
    (if (leaf? bt)
        (leaf (* 2 (content-of bt)))
        (interior-node (content-of bt) (double-tree (lson bt)) (double-tree (rson bt))))))

(define test-tree1
  (interior-node 'red
                 (interior-node 'bar
                                (leaf 26)
                                (leaf 12))
                 (interior-node 'red
                                (leaf 11)
                                (interior-node 'quux
                                               (leaf 117)
                                               (leaf 14)))))

;; ex: 1.33
;; (define mark-leaves-with-red-depth
;;   (lambda 



;; ex: 1.34

(define path-test '(14 (7 () (12 () ())) (26 (20 (17 () ())
                                                 ())
                                             (31 () ()))))
(define path
  (lambda (n bt)
    (if (or (null? bt) (= n (car bt)))
        '()
        (if (> n (car bt))
            (cons 'right (path n (caddr bt)))
            (cons 'left (path n (cadr bt)))))))

;; (path '17 path-test)

;; ex: 1.35

(define number-leaves-test
  (interior-node 'foo
                  (interior-node 'bar
                                  (leaf 26)
                                  (leaf 12))
                  (interior-node 'baz
                                  (leaf 11)
                                  (interior-node 'quux
                                                  (leaf 117)
                                                  (leaf 14)))))


(define number-leaves-node
  (lambda (nbtl nbtr bt)
    (list (car nbtr)
          (interior-node
           (content-of bt)
           (cadr nbtl)
           (cadr nbtr)))))
    

(define number-leaves-right
  (lambda (nbtl bt)
    (number-leaves-node nbtl (number-leaves-sub (car nbtl) (rson bt)) bt)))

(define number-leaves-left
  (lambda (n bt)
    (number-leaves-right (number-leaves-sub n (lson bt)) bt)))


;; n and bt passed around to pass the original/initial thingi
;; the complex ds being returned, to create the solution.

(define number-leaves-sub
  (lambda (n bt)
    (if (leaf? bt)
        (list (+ n 1) (leaf n))
        (number-leaves-left n bt))))

(define number-leaves
  (lambda (bt)
    (cadr (number-leaves-sub 0 bt))))

;; (number-leaves number-leaves-test)


;; ex: 1.36
(define adjust
  (lambda (pair)
    (cons (+ (car pair) 1) (cdr pair))))

(define number-g
  (lambda (first lst)
    (cons first (map adjust lst))))

(define number-elements-1
  (lambda (lst)
    (if (null? lst) '()
        (number-g (list 0 (car lst)) (number-elements-1 (cdr lst))))))
  
