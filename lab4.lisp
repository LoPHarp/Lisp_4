(defun remove-first (elem lst)
  (if (eql elem (car lst))
      (cdr lst)
      (cons (car lst) (remove-first elem (cdr lst)))))

(defun findMinElem (elem lst)
  (cond
    ((null lst) elem)
    ((> elem (car lst)) (findMinElem (car lst) (cdr lst)))
    (t (findMinElem elem (cdr lst)))))

(defun FchoiseSort (lst)
  (if (null (cdr lst))
      lst
      (let* ((min (findMinElem (car lst) (cdr lst)))
             (restList (remove-first min lst)))
        (cons min (FchoiseSort restList)))))

;----------------------------------------------------------------------------------

(defun choise-Sort (lst &key (key nil) (test #'<))
  (let ((remove-first (lambda (elem input-list)
                        (remove elem input-list :count 1 :test #'equal))))
    (labels
        ((find-min-elem (%lst min-elem key-min-elem)
           (if (null %lst)
               min-elem
               (let* ((check-elem (car %lst))
                      (key-check-elem (if key
                                          (funcall key check-elem)
                                          check-elem)))
                 (if (funcall test key-check-elem key-min-elem)
                     (find-min-elem (cdr %lst) check-elem key-check-elem)
                     (find-min-elem (cdr %lst) min-elem key-min-elem)))))

         (my-sort (%lst)
           (if (null (cdr %lst))
               %lst
               (let* ((first-min (car %lst))
                      (key-first-min (if key
                                         (funcall key first-min)
                                         first-min))
                      (min (find-min-elem (cdr %lst) first-min key-first-min))
                      (rest (funcall remove-first min %lst))
                      (sorted-rest (my-sort rest)))
                 (cons min sorted-rest)))))
      (my-sort lst))))

 ;----------------------------------------------------------------------------------

(defun check-my-sort (name input expected &key (key nil) (test #'<))
  "Виконує choise-Sort на 'input' з 'key' і 'test', порівнює з 'expected'"
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (choise-Sort input :key key :test test) expected)
          name))

(defun test-my-sort ()
  (check-my-sort "Test 1: Просте сортування"
                 '(5 1 4 2 3)
                 '(1 2 3 4 5))
  
  (check-my-sort "Test 2: Зворотнє сортування (test #'>)"
                 '(5 1 4 2 3)
                 '(5 4 3 2 1)
                 :test #'>)
  
  (check-my-sort "Test 3: Сортування за ключем (key #'second)"
                 '((1 9) (2 5) (3 8))
                 '((2 5) (3 8) (1 9))
                 :key #'second)
  
  (check-my-sort "Test 4: Порожній список" '() '())
  )

;----------------------------------------------------------------------------------

(defun merge-spinning-tuples-fn (&key (shift-step 1))
  (let ((current-shift 0))
    (labels
        ((shift (%lst n)
                         (if (null %lst)
                             nil
                             (let ((n (mod n (length %lst))))
                               (if (= n 0)
                                   %lst
                                   (let*
                                       ((new-head (nthcdr n %lst))
                                        (new-body (subseq %lst 0 n)))
                                     (append new-head new-body)))))))
    (lambda (&rest args)
      (let* ((new-args (shift args current-shift)))
             (incf current-shift shift-step)
        new-args)))))

;----------------------------------------------------------------------------------

(defun check-merge-spinning-tuples (name lst1 lst2 lst3 shift-step expected)
  (let* ((func (if shift-step 
                   (merge-spinning-tuples-fn :shift-step shift-step)
                   (merge-spinning-tuples-fn)))
         (args (list lst1 lst2 lst3))
         (result (apply #'mapcar func args)))
    (format t "~:[FAILED~;passed~] ~a~%" (equal result expected) name)))

(defun test-merge ()
  (check-merge-spinning-tuples "test 1" '(1 2 3) '(a b c) '(x y z) nil '((1 A X) (B Y 2) (Z 3 C)))
  (check-merge-spinning-tuples "test 2" '(a b c) '(1 2 3) '(d e f) 2 '((A 1 D) (E B 2) (3 F C)))
  (check-merge-spinning-tuples "test 3" '(1 2 3 4) '(a b c d) '(x y z w) 1 '((1 A X) (B Y 2) (Z 3 C) (4 D W)))
  (check-merge-spinning-tuples "test 4" '(1 2 3 4) '(a b c d) '(x y z w) 3 '((1 A X) (2 B Y) (3 C Z) (4 D W))))

(test-merge)


