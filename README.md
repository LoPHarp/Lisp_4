<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>

<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"><b>Студент</b>: Мануйлов Денис Денисович</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
1.  Переписати функціональну реалізацію алгоритму сортування з лабораторної роботи 3 з такими змінами:
    * використати функції вищого порядку для роботи з послідовностями (де/якщо це доречно, в разі, якщо функції вищого порядку не були використані при реалізації л.р. №3);
    * додати до інтерфейсу функції (та використання в реалізації) два ключових параметра: `key` та `test`, що працюють аналогічно до того, як працюють параметри з такими назвами в функціях, що працюють з послідовностями (р. 12). При цьому `key` має виконатись мінімальну кількість разів.
2.  Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом 11. Використання псевдофункцій не забороняється, але, за можливості, має бути зменшене до необхідного мінімуму.

## Варіант першої частини 11(11 % 5 = 1)
Алгоритм сортування вибором за незменшенням.
```Lisp
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
```

## Лістинг реалізації першої частини завдання
```lisp
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
```

### Тестові набори та утиліти першої частини
```lisp
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
  
  (check-my-sort "Test 4: Порожній список" '() '()))
```

### Тестування першої частини
```lisp
CL-USER> (test-my-sort)
passed Test 1: Просте сортування
passed Test 2: Зворотнє сортування (test #'>)
passed Test 3: Сортування за ключем (key #'second)
passed Test 4: Порожній список
NIL
```

## Варіант другої частини 11
Написати функцію `merge-spinning-tuples-fn` , яка має один ключовий параметр — `shift-step` . `merge-spinning-tuples-fn` має повернути функцію, яка при застосуванні в якості першого аргументу mapcar робить наступне: об'єднує всі поточні елементи зі списків-аргументів `mapcar` в один список (кортеж), циклічно зміщуючи елементи в кортежі. Величина зміщення збільшується для кожного наступного кортежу на значення `shift-step` , починаючи з 0. Якщо `shift-step` не зазначений користувачем, тоді величина "кроку" зміщення — 1.
```lisp
CL-USER> (mapcar (merge-spinning-tuples-fn) '(1 2 3) '(a b c))
((1 A) (B 2) (3 C))

CL-USER> (mapcar (merge-spinning-tuples-fn :shift-step 2)
                 '(a b c)
                 '(1 2 3)
                 '(d e f))
((A 1 D) (E B 2) (3 F C))
```

## Лістинг реалізації другої частини завдання
```lisp
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
```

### Тестові набори та утиліти другої частини
```lisp
;;; Лістинг реалізації утилітних тестових функцій та тестових наборів
```

### Тестування другої частини
```lisp
;;; Виклик і результат виконання тестів
```

