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
 
 ;; Oppgave 2b) 

 ;; Resultatet er "Samurais fight ninjas by night"

 ;; Oppgave 2c) 

 (define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((memq symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol not in tree - ENCODE-SYMBOL" symbol))))

;; Oppgave 2d) 

(define (grow-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge nodes)
  (if (= (length nodes) 1)
      (car nodes)
      (let ((new-tree
             (make-code-tree (car nodes)
                             (cadr nodes))))
        (successive-merge
         (adjoin-set new-tree
                     (cddr nodes))))))

;; Oppgave 2e)

(define freqs
  '((samurais 57)
    (ninjas 20)
    (fight 45)
    (night 12)
    (hide 3)
    (in 2)
    (ambush 2)
    (defeat 1)
    (the 5)
    (sword 4)
    (by 12)
    (assassin 1)
    (river 2)
    (forest 1)
    (wait 1)
    (poison 1)))

(define codebook
  (grow-huffman-tree freqs))

;; meldingen blir totalt 5+10+6+3+4=38 bits
;; 38 bits/17 symboler = 2.24 gjennomsnittlige bits
;; log av 16 = 4. 17 * 4 = 68 En fast-lengde kode trenger minst 68 bits. fordi: alfabetet inneholder 16 ulike symboler, 
;; da trenger du 4 bits for hvert symbol, meldingen inneholder 17

;; Oppgave 2f) 

(define (huffman-leaves tree)
  (if (leaf? tree)
      (list (list (symbol-leaf tree)
                  (weight-leaf tree)))
      (append (huffman-leaves (left-branch tree))
              (huffman-leaves (right-branch tree)))))
