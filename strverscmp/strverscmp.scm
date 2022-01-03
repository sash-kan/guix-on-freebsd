(use-modules (srfi srfi-1))

(define (digit? c)
  (string-any c "0123456789"))

(define (strverscmp x y)
  (let
    (
     (lenx (string-length x))
     (leny (string-length y))
     (lx (string->list x))
     (ly (string->list y))
    )
    (if (< lenx leny)
      (set! lx (append lx (make-list (- leny lenx) #\nul)))
      (if (> lenx leny)
        (set! ly (append ly (make-list (- lenx leny) #\nul)))
      )
    )
    (set! lx (append lx (make-list 1 #\nul)))
    (set! ly (append ly (make-list 1 #\nul)))
    (car (fold myiter '(= c "" "") lx ly))))

(define (myiter x y r)
  (let
    (
     (res (car r))
     (prev (cadr r))
     (xd (caddr r))
     (yd (cadddr r))
     )
    (if (equal? res '=)
      (cond
        ((equal? prev 'c)
         ; были не-цифры
         (if (and (digit? x) (digit? y))
           ; оба символа - цифры. проверяем на нули
           (cond
             ((and (equal? x #\0) (equal? y #\0))
              ; обе равны нулю. ставим признак
              '(= z "" ""))
             ((and (equal? x #\0) ((negate equal?) y #\0))
              ; y не ноль, значит больше
              '(< c "" ""))
             ((and ((negate equal?) x #\0) (equal? y #\0))
              ; x не ноль, значит больше
              '(> c "" ""))
             (else
               ; обе цифры не нули, накапливаем
               (list '= 'd (string x) (string y))))
           ; один из символов — не-цифра
           ; сравниваем лексикографически
           (cond ((char>? x y) '(> c "" ""))
                 ((char<? x y) '(< c "" ""))
                 (else '(= c "" "")))))

        ((equal? prev 'z)
         ; были нули
         (cond
           ((and (equal? x #\0) (equal? y #\0))
            ; продолжаем
            (list '= prev "" ""))
           ((and (digit? x) ((negate digit?) y))
            ; с нецифрой будет меньше
            '(< c "" ""))
           ((and ((negate digit?) x) (digit? y))
            ; с нецифрой будет меньше
            '(> c "" ""))
           ((and (digit? x) (digit? y))
            ; если текущие цифры равны,
            ; то продолжаем как со смесью
            (cond ((char>? x y) '(> c "" ""))
                  ((char<? x y) '(< c "" ""))
                  (else '(= m "" "")))
            )
           (else
             ; были нули, а теперь не цифры
             ; сравниваем лексикографически
             (cond ((char>? x y) '(> c "" ""))
                   ((char<? x y) '(< c "" ""))
                   (else '(= c "" "")))
             )
           )
         )

        ((equal? prev 'm)
         ; была смесь
         (cond
           ((and (equal? x #\0) (equal? y #\0))
            ; продолжаем
            (list '= prev "" ""))
           ((and (digit? x) ((negate digit?) y))
            ; с нецифрой будет больше
            '(> c "" ""))
           ((and ((negate digit?) x) (digit? y))
            ; с нецифрой будет больше
            '(< c "" ""))
           ((and (digit? x) (digit? y))
            ; если текущие цифры равны,
            ; то продолжаем как со смесью
            (cond ((char>? x y) '(> c "" ""))
                  ((char<? x y) '(< c "" ""))
                  (else '(= m "" "")))
            )
           (else
             ; были нули, а теперь не цифры
             ; сравниваем лексикографически
             (cond ((char>? x y) '(> c "" ""))
                   ((char<? x y) '(< c "" ""))
                   (else '(= c "" "")))
             )
           )
         )

        ((equal? prev 'd)
         ; были цифры
         (cond
           ((and (digit? x) (digit? y))
            ; обе цифры, продолжаем накопление
            (list '= 'd
              (string-append xd (string x))
              (string-append yd (string y))
              )
            )
           ((digit? x)
            ; только x - цифра, значит больше
            '(> c "" "")
            )
           ((digit? y)
            ; только y - цифра, значит больше
            '(< c "" "")
            )
           (else
             ; цифры кончились,
             ; сравниваем накопленное
             (cond
               ((string>? xd yd)
                '(> c "" "")
                )
               ((string<? xd yd)
                '(< c "" "")
                )
               (else
                 ; накопленные числа равны,
                 ; сравниваем текущие символы
                 ; лексикографически
                 (cond ((char>? x y) '(> c "" ""))
                       ((char<? x y) '(< c "" ""))
                       (else '(= c "" "")))
                 )
               )
             )
           )
         )
        )
      ; res уже определён, возвращаем что есть
      (list res 'c "" "")
      )
    )
  )
