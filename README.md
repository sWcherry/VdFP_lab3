<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>

<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"><b>Студентка</b>: Панченко Вікторія Володимирівна КВ-12</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання

Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного списку. Не допускається використання: псевдо-функцій, деструктивних операцій, циклів, функцій вищого порядку або функцій для роботи зі списками/послідовностями, що використовуються як функції вищого порядку. Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і
деструктивних функцій (псевдофункцій). Не допускається використання функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Тим не менш, оригінальний список цей варіант реалізації також не має змінювати, тому перед виконанням
деструктивних змін варто застосувати функцію copy-list (в разі необхідності). Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).

Алгоритм, який необхідно реалізувати, задається варіантом.

Кожна реалізована функція має бути протестована для різних тестових наборів. Тести мають бути оформленні у вигляді модульних тестів.

## Варіант 8

Алгоритм сортування обміном №4 ("шейкерне сортування") за незменшенням.

## Лістинг функції з використанням конструктивного підходу

```lisp
(defun left-right (lst i k middle)
  (if (cdr lst)
      (let ((current (car lst))
            (next (second lst)))
        (if (> current next)
            (left-right (remove next lst :count 1)
                        (1+ i) i
                        (cons next middle))
            (left-right (remove current lst :count 1) 
                        (1+ i) k
                        (cons current middle))))
      (values k (append (reverse middle) lst))))

(defun right-left (lst i k middle)
  (if (cdr lst)
      (let ((current (car lst))
            (next (second lst)))
        (if (> next current)
            (right-left (remove next lst :count 1)
                        (1- i) i
                        (cons next middle))
            (right-left (remove current lst :count 1) 
                        (1- i) k
                        (cons current middle))))
      (values (1+ k) (append lst middle))))

(defun functional (lst &optional (L 0) (R (1- (list-length lst))))
  (if (>= L R)
      lst
      (multiple-value-bind (new-R new-middle)
          (left-right (subseq lst L (1+ R)) L L nil)
        (let ((new-lst (append (subseq lst 0 L)
                               new-middle
                               (nthcdr (1+ R) lst))))
          (multiple-value-bind (new-L new-middle)
              (right-left (reverse (subseq new-lst L (1+ new-R))) 
                          (1- new-R) new-R nil)
            (functional (append (subseq new-lst 0 L)
                                new-middle
                                (nthcdr (1+ new-R) new-lst))
                        new-L new-R))))))
```

### Тестові набори та утиліти

```lisp
(defun check-functional (name lst expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (functional lst) expected)
          name))

(defun test-functional ()
  (check-functional "test1" '(0.3 5 0 -1 -3 -3 5.1) '(-3 -3 -1 0 0.3 5 5.1))
  (check-functional "test2" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-functional "test3" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-functional "test4" '(2 2 2 2 0) '(0 2 2 2 2)))
```

### Тестування

```lisp
CL-USER> (test-functional)
passed test1
passed test2
passed test3
passed test4
NIL
```

## Лістинг функції з використанням деструктивного підходу

```lisp
(defun imperative (lst)
  (let ((L 0)
        (R (1- (list-length lst)))
        (k 0)
        (copy (copy-list lst)))
    (loop
      (when (>= L R) (return copy))
      
      (do ((i L (1+ i)))
          ((= i R) (setq R k))
        (when (> (nth i copy) (nth (1+ i) copy))
          (rotatef (nth i copy) (nth (1+ i) copy))
          (setq k i)))

      (do ((i (1- R) (1- i)))
          ((< i L) (setq L (1+ k)))
        (when (> (nth i copy) (nth (1+ i) copy))
          (rotatef (nth i copy) (nth (1+ i) copy))
          (setq k i))))))
```

### Тестові набори та утиліти

```lisp
(defun check-imperative (name lst expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (imperative lst) expected)
          name))

(defun test-imperative ()
  (check-imperative "test1" '(0.3 5 0 -1 -3 -3 5.1) '(-3 -3 -1 0 0.3 5 5.1))
  (check-imperative "test2" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-imperative "test3" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-imperative "test4" '(2 2 2 2 0) '(0 2 2 2 2)))
```

### Тестування

```lisp
CL-USER> (test-imperative)
passed test1
passed test2
passed test3
passed test4
NIL
```
