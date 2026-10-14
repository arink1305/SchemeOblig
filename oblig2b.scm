;; Oppgave 1

;;a)

(define (make-counter)
  (let ((count 0))
  (lambda()
    (set! count(+ count 1))
         count)))


;; Oppgave 2

(define (make-stack items)
  (let ((items items))

    (lambda (message . args)
      (cond
        ((eq? message 'push!)
         (for-each
          (lambda (item)
            (set! items (cons item items)))
          args))

        ((eq? message 'pop!)
         (if (null? items)
             '()
             (let ((top (car items)))
               (set! items (cdr items))
               top)))

        ((eq? message 'stack)
         items)

        (else
         (error "Ukjent melding" message))))))
  
