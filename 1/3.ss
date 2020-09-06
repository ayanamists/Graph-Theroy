(define (edge-first edge)
  (car edge))
(define (edge-second edge)
  (cadr edge))
(define (edge a b)
  (list a b))
(define (edge-equal? a b)
  (let ((afi (edge-first a))
        (ase (edge-second a))
        (bfi (edge-first b))
        (bse (edge-second b)))
    (or 
      (and (= afi bfi) (= ase bse))
      (and (= ase bfi) (= afi bse)))))
(define (member? a lis)
  (if (equal? (find (lambda (x) (edge-equal? a x)) lis) #f)
      #f
      #t))
(define (graph-equal? a b)
  (let ((len-a (length a))
        (len-b (length b)))
    (cond 
      ((= len-a len-b)
       (if (null? a)
           #t
           (if (member? (car a) b)
               (graph-equal? (cdr a)
                             (filter (lambda (x) (not (edge-equal? x (car a))))
                                     b))
               #f)))
       (else #f))))

(define (random-choose size li)
  (let ((len (length li)))
    (cond
      ((= size 1) (map (lambda (x) (list x)) li))
      ((> size len) '())
      ((= size len) (list li))
      ((< size len) 
       (append
         (let ((this (car li)))
           (map (lambda (x) (cons this x))
                (random-choose (- size 1)
                               (cdr li))))
         (random-choose size (cdr li)))))))

(define (test-n-degree graph target-list n)
  (define (get-count graph target)
    (fold-right
      (lambda (x res)
        (let ((first (edge-first x))
              (second (edge-second x)))
          (let* ((tf (if (equal? target first)
                         (+ res 1)
                         res))
                 (tr (if (equal? target second)
                         (+ tf 1)
                         tf)))
            tr)))
      0
      graph))
  (call/cc 
    (lambda (k)
      (for-each 
        (lambda (x) 
          (let ((count (get-count graph x)))
            (if (= count n)
                #t
                (k #f))))
        target-list))))

(define target-list '(0 1 2 3 4 5))

(define (test graph)
  (test-n-degree graph target-list 3))

(define list-with-homo
  (filter test (random-choose 9 (random-choose 2 target-list))))

(define (map-edge old rule)
  (let ((first (edge-first old))
        (second (edge-second old))
        (r-f (car rule))
        (r-r (cdr rule)))
    (edge (if (equal? r-f first)
              r-r
              first)
          (if (equal? r-f second)
              r-r
              second))))

(define (mark a)
  (cons 'm a))

(define (unmark l)
  (map (lambda (x) (edge 
                     (cdr (edge-first x))
                     (cdr (edge-second x))))
       l))

(define (apply-per graph per)
  (unmark
  (let loop ((graph graph) (per per) (n 0))
    (if (null? per)
        graph
        (loop (map
                (lambda (x) (map-edge x (cons n (mark (car per)))))
                graph)
              (cdr per)
              (+ n 1))))))

(define (gen-all-homo graph)
  (let ((pers (permutations target-list)))
    (map (lambda (x) (apply-per graph x))
         pers)))

(define (filter-by-homo homos graphs)
  (if (null? homos)
      graphs
      (filter-by-homo (cdr homos)
                      (filter (lambda (x) (not (graph-equal? x (car homos))))
                              graphs))))
          

(define (remove-homo graphs) 
  (define (inner unc c)
    (if (null? unc)
        (reverse c)
        (let* ((now (car unc))
               (homos (gen-all-homo now)))
          (inner (filter-by-homo homos (cdr unc))
                 (cons now c)))))
  (inner graphs '()))

(define (permutations s)
  (cond [(null? s) '()]
        [(null? (cdr s)) (list s)]
        [else
         (let splice [(l '()) (m (car s)) (r (cdr s))]
           (append
            (map (lambda (x) (cons m x))
                 (permutations (append l r)))
            (if (null? r)
                '()
                (splice (cons m l) (car r) (cdr r)))))]))

(display (remove-homo list-with-homo))

(define (homo? a b)
  (find (lambda (x) (graph-equal? x b))
        (gen-all-homo a)))
