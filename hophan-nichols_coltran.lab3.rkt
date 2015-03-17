#lang racket
(define (f lst)
  ; (a) ;
  (if (null? lst) ; Check to see if lst is empty ;
      ; (b) ;
      '() ; if so, do nothing ;
      ; c ;
      (cons (+ 1 (car lst)) (f (cdr lst))))) ; otherwise add one to the head of lst and recursively call f on the tail of lst ;

(define (member? e lst) ; is e in lst? ;
  (cond
    ((null? lst) #f) ; if the list is empty or there are no more to check, e is not a member ;
    ((eq? e (car lst)) #t) ; if e is the same as the element we are checking, it is in the list ;
    (else (member? e (cdr lst))))) ; otherwise, recursively test the rest of the list ;

(define (set? lst) ; is lst a proper set? ;
  (cond
    ((null? lst) #t) ; if the list is empty or everything has been checked, lst is a proper set ;
    ((member? (car lst) (cdr lst)) #f) ; if the first element can be found in the rest of the list, it is not a proper set ;
    (else (set? (cdr lst))))) ; otherwise, recursively check the rest of the list ;

(define (unions lst) ; removes duplicates from lst ;
  (cond
    ((null? lst) '()) ; if empty, done ;
    ((member? (car lst) (cdr lst)) (unions (cdr lst))) ; if the head appears int the tail moove on to the tail ;
    (else (cons (car lst) (unions (cdr lst)))))) ; if the head is not in the tail, it is in the intersection. check rest ;

(define (union lst1 lst2) ; union of lst1 lst2 ;
  (unions (append lst1 lst2))) ; remove duplicates from the concatination of lst1 and lst2 ;

(define (intersect lst1 lst2) ; intersection of lst1, lat2 ;
  (cond
    ((null? lst1) '()) ; if lst empty, done ;
    ((member? (car lst1) lst2) (cons (car lst1) (intersect (cdr lst1) lst2))) ; if head of lst1 is in lst2, it is in the intersection. check rest ; 
    (else (intersect (cdr lst1) lst2)))) ; otherwise remove head and check rest ;

(define (flattens lst) ; flattens a single list ;
  (cond
    ((null? lst) '()) ; if list is empty, done ;
    ((list? (car lst)) (flattens (append (car lst) (cdr lst)))) ; if the head is a list, append it to the tail and call flattens on it ;
    (else (cons (car lst) (flattens (cdr lst)))))) ; otherwise, take the head and recursively call flattens on the tail ;

(define (flatten lst1 lst2) ; flatten two lists ;
  (flattens (append lst1 lst2))) ; flatten the concatination of lst1 and lst2 ;