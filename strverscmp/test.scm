(use-modules (guix utils))
(use-modules (srfi srfi-1))
(use-modules (ice-9 textual-ports))

(define tests (list
                "set1.of.tests" ; https://www.gnu.org/software/libc/manual/html_node/String_002fArray-Comparison.html#index-strverscmp
                "set2.of.tests" ; https://git.savannah.gnu.org/gitweb/?p=gnulib.git;a=blob;f=tests/test-strverscmp.c
                "set3.of.tests" ; -//-
                "set4.of.tests")) ; gnulib git tags

(load "strverscmp.scm")

(define (comb l) (append-map (lambda (x) (map (lambda (y) (cons x y)) l)) l))

(define (testversions f) (delete "" (string-split (call-with-input-file f get-string-all) #\newline)))

(define (testpairs f) (comb (testversions f)))

(define (compare-funcs f1 f2 pair)
  (let
    ((r1 (f1 (car pair) (cdr pair)))
     (r2 (f2 (car pair) (cdr pair))))
    (if (eq? r1 r2)
      1
      (begin
        (display r1)
        (display " ")
        (display r2)
        (display " pair: ")
        (display pair)
        (newline)
        0))))

(define (test f1 f2 l)
  (let
    ((res (map (lambda (x) (compare-funcs f1 f2 x)) l)))
    (let
      ((len (length res)))

      (display "tests#:\t")
      (display len)
      (newline)
      (display "pass:\t")
      (display (apply + res))
      (newline)
      (display "fail:\t")
      (display (- len (apply + res)))
      (newline)
    )
  )
)

(define (runtests f)
  (display "==============")(newline)
  (display f)(newline)
  (display "==============")(newline)
  (test version-compare strverscmp (testpairs f)))

(map runtests tests)

