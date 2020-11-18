#lang racket/base
(module+ main
  (go! 09 05))

;; Don't change after this line
(require racket/runtime-path
         racket/match
         json)
(define-runtime-path src "scriptures-json")
(define fs
  '((old-testament books) (new-testament books) (book-of-mormon books)
    (doctrine-and-covenants sections) (pearl-of-great-price books)))

(define (go! m d)
  (for ([ft (in-list fs)])
    (match-define (list f t) ft)
    (define fp (build-path src (format "~a.json" f)))
    (define fj (with-input-from-file fp read-json))
    (define (do! f)
      (match t
        ['books
         (for ([b (in-list (hash-ref fj t))])
           (define cs (list->vector (hash-ref b 'chapters)))
           (f cs))]
        ['sections
         (f (list->vector (hash-ref fj t)))]))
    (do!
     (λ (cs)
       (for ([xy (in-list (list (cons m d) #;(cons d m)))])
         (match-define (cons cn vn) xy)
         (when (<= cn (vector-length cs))
           (define c (vector-ref cs (sub1 cn)))
           (define vs (list->vector (hash-ref c 'verses)))
           (when (<= vn (vector-length vs))
             (define v (vector-ref vs (sub1 vn)))
             (define r (hash-ref v 'reference))
             (define t (hash-ref v 'text))
             (printf "~a\n  ~a\n\n" r t))))))))
