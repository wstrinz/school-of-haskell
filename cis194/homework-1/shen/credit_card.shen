(tc +)

(datatype number
  _____________________________________
  (number? N) : verified >> N : number;)

(datatype ccnumber
  CC : number;
  ==========
  CC : ccnumber;)

\* flatten function from chibi-shen core *\

(defun flatten (V13222) (cond ((= () V13222) ()) ((cons? V13222) (append (shen.flatten (hd V13222)) (shen.flatten (tl V13222)))) (true (cons V13222 ()))))



\* String functions from Willi Riha's Strings library *\

(define number-test
  {string --> (list unit) --> number}
   _ [N] -> N         where (number? N)
   Str _ -> (error "~S cannot be read as a number~%" Str))

(define string->number
  {string --> number}
  Str -> (number-test Str (read-from-string Str)))




\* Math functions from Willi Riha's Maths library *\

(define sign
  {number --> number}
  X -> 1  where (> X 0)
  X -> -1 where (< X 0)
  _ -> 0)

(define abs
  {number --> number}
  X -> (if (>= X 0) X (- 0 X)))

(define /-pos
  {number --> number --> (number * number)}
  _ 0 -> (error "division by 0!~%")
  A B -> (@p 0 A) where (> B A)
  _ B -> (error "divisor must be an integer!~%") where (not(integer? B))
  A B -> (let Pow2 (pow-2div B A 1)
                          (div-w A (* Pow2 B) Pow2 0)))

(define pow-2div
  {number --> number --> number --> number}
  B A Pow2 -> (if (<= B A) (pow-2div (* B 2) A (* Pow2 2)) Pow2))

(define div-w
  {number --> number --> number --> number --> (number * number)}
  A B 1 Q -> (if (<= B A) (@p (+ Q 1) (- A B)) (@p Q A))
  A B P Q -> (if (<= B A) (div-w (- A B) (/ B 2) (/ P 2) (+ Q P))
                          (div-w A (/ B 2) (/ P 2) Q)))

(define mod-h
  {number --> number --> number --> number --> number}
  B 0  _  _ -> 0
  B R  1  1 -> R
  B R -1 -1 -> (- 0 R)
  B R  1 -1 -> (+ B R)
  B R -1  1 -> (- B R))

(define mod
  {number --> number --> number}
  A B -> (let QR (/-pos (abs A) (abs B))
                          (mod-h B (snd QR) (sign A) (sign B))))





\* HW1 functions *\

(define to_digits
  {ccnumber --> (list ccnumber)}
  A -> [] where (= "-" (hdstr (str A)))
  A -> (map string->number (explode (str A))))

(define to_digits_rev
  {ccnumber --> (list ccnumber)}
  A -> (reverse (to_digits A)))

(define double_every_other'
  {(list ccnumber) --> (list ccnumber)}
  [X Y | XS] -> [X (* Y 2) | (double_every_other' XS)]
  [X Y] -> [X (* Y 2)]
  X -> X)

(define double_every_other
  {(list ccnumber) --> (list ccnumber)}
  X -> (reverse (double_every_other' (reverse X))))

(define sum_digits
  {(list ccnumber) --> ccnumber}
  X -> (sum (flatten (map to_digits X))))

(define validate
  {ccnumber --> boolean}
  X -> (== 0 (mod (sum_digits (double_every_other (to_digits X)))
          10)))
