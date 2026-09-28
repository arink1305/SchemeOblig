;; Oppgave 1 a)

(define (p-cons x y)
(lambda (proc) (proc x y))) ;;Med lambda så har vi nå "låst inn" verdiene x og i, i minne sammen. verdiene er låst til vi bruker en annen funksjon på de

(define(p-car pair) 
  (pair(lambda (x y) x))) ;;Vi bruker lambda igjen for å returnere x fra paret x og y.

(define(p-cdr pair)
  (pair(lambda (x y) y))) ;; vi gjør det samme her men returnerer y, som representerer cdr.

;;Oppgave 1 b)

(define foo 42)

((lambda (foo x)
  (if (= x foo)
     'same
     'different))
   5 42)



((lambda (bar baz)
   ((lambda (bar foo)
     (list foo bar))
   (list bar baz)baz))
 42 'towel)



;;Oppgave 1 c)

(define (infix-eval exp)
 ((car(cdr exp)) (car exp)  (car(cdr(cdr exp)))))

;;Svar: operatoren er andre element i listen, men det er den første elementen vi må finne listen fordi alle operasjoner starter med operatoren i scheme

;;Oppgave 1d)

(define bah '(84 / 2))

;;Dette kallet kommer ikke til å funke for prosedyren, fordi når vi bruker '() så blir alle elementer lest uten evaluering.
;;det betyr at / blir bare lest som symbolet / og ikke som en operasjon.

         
     
;;Oppgave 2a)

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
      (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))))
 
 
 





