(in-package #:cl-user)

;;;; ------------------------------------
;;;; Чтение входных данных и их валидация
;;;; ------------------------------------

(defun edge-form-p (edge)
  "Ребро — это список из трёх элементов: (from label to)"
  (and (listp edge) (= (length edge) 3)))

(defun valid-label-p (label)
  "Метка – символ"
  (symbolp label))

(defun valid-state-p (s)
  "Состояние – символ или строка"
  (or (symbolp s) (stringp s)))

(defun validate-edge (edge)
  "Проверяет форму и типы: (FROM LABEL TO) и возвращает T, если всё корректно"
  (destructuring-bind (from label to) edge
    (unless (valid-state-p from)
      (error "Некорректная вершина FROM в ребре ~a" edge))
    (unless (valid-label-p label)
      (error "Некорректная метка LABEL в ребре ~a" edge))
    (unless (valid-state-p to)
      (error "Некорректная вершина TO в ребре ~a" edge))
    T))

(defun read-automaton-form (&optional (stream *standard-input*))
  "Считывает одно выражение из потока, бросает ошибку если EOF"
  (let ((form (read stream nil :eof)))
    (when (eq form :eof)
      (error "Не найдено описание автомата во входных данных"))
    form))

(defun parse-automaton (data)
  "Возвращает три значения: EDGES, START, FINALS
   Формат входа:
      список рёбер: ((H a S) ...) — старт H, финал S по умолчанию"
  (cond

    ((and (listp data) (every #'edge-form-p data))
     (mapc #'validate-edge data)
     (values data 'H 'S))

    (T
     (error "Неподдерживаемый формат автомата: ~s" data))))

(defun read-and-parse (&optional (stream *standard-input*))
  "Считывает форму и парсит; возвращает три значения"
  (parse-automaton (read-automaton-form stream)))

;;;; ------------------------------------------------------
;;;; Базовые конструкции и проверка на определённые лексемы
;;;; ------------------------------------------------------

(defparameter +empty+ :empty)
(defparameter +epsilon+ '(:epsilon))

(defun empty-p   (e) (eq e +empty+))
(defun epsilon-p (e) (and (consp e) (eq (car e) :epsilon)))
(defun literal-p (e) (and (consp e) (eq (car e) :literal)))
(defun union-p   (e) (and (consp e) (eq (car e) :union)))
(defun concat-p  (e) (and (consp e) (eq (car e) :concat)))
(defun star-p    (e) (and (consp e) (eq (car e) :star)))

;;;; ----------------------------------
;;;; Конструкторы операций в выражениях
;;;; ----------------------------------

(defun make-literal (s) (list :literal s))

(defun make-union (&rest xs)
  ; Заполняем список элементов объединения без мутаций
  (labels ((flatten (expr)
             (cond
               ((empty-p expr) '())
               ((union-p expr) (flatten-list (cdr expr)))
               (T (list expr))))
           (flatten-list (lst)
             (if (null lst)
                 '()
                 (append (flatten (car lst))
                         (flatten-list (cdr lst))))))
    ; Убираем дупликаты и приводим к нормальной форме
    (let* ((items (remove-duplicates (flatten-list xs) :test #'equal)))
      (cond
        ; Рассматриваем тривиальные случаи и возвращаем объединение
        ((null items) +empty+)
        ((null (cdr items)) (car items))
        (T (cons :union items))))))

(defun make-concat (&rest xs)
  (let ((empty-marker (list :empty-marker)))
    ; Заполняем список элементов конкатенации, отслеживая пустоту
    (labels ((flatten (expr)
               (cond
                 ((empty-p expr) empty-marker)
                 ((epsilon-p expr) '())
                 ((concat-p expr) (flatten-list (cdr expr)))
                 (T (list expr))))
             (flatten-list (lst)
               (if (null lst)
                   '()
                   (let ((items (flatten (car lst))))
                     (if (eq items empty-marker)
                         empty-marker
                         (let ((rest (flatten-list (cdr lst))))
                           (if (eq rest empty-marker)
                              empty-marker
                              (append items rest))))))))
      (let ((items (flatten-list xs)))
        (cond
          ; Рассматриваем тривиальные случаи и возвращаем конкатенацию
          ((eq items empty-marker) +empty+)
          ((null items) +epsilon+)
          ((null (cdr items)) (car items))
          (T (cons :concat items)))))))

(defun make-star (e)
  (cond
    ; Рассматриваем тривиальные случаи 
    ((empty-p e) +epsilon+)
    ((epsilon-p e) +epsilon+)
    ((star-p e) e)
    ; Случай (a | eps)* переводит в a*
    ((and (union-p e) (find-if #'epsilon-p (cdr e)))
     (let* ((filtered (remove-if #'epsilon-p (cdr e)))
            (reduced (cond
                       ((null filtered) +epsilon+)
                       ((null (cdr filtered)) (car filtered))
                       (T (apply #'make-union filtered)))))
       (make-star reduced)))
    (T (list :star e))))

;;;; -------------------------
;;;; Инструменты для алгоритма
;;;; -------------------------

(defun normalize-label (label)
  "Нормализуем метку:
    1. Если eps, то переводим в специальный символ
    2. Иначе приводим к нижнему регистру"
  (let* ((name (string label)))
    (if (string-equal name "eps")
        +epsilon+
        (make-literal (string-downcase name)))))

(defun all-states (edges)
  "Множество состояний автомата"
  (remove-duplicates
   (append (mapcar #'first edges)
           (mapcar #'third edges))))

(defun iota (n &optional (start 0))
  "Список натуральных чисел [start, start+n)"
  (labels ((build (k)
             (if (= k n)
                 '()
                 (cons (+ start k) (build (+ 1 k))))))
    (build 0)))

(defun matrix-ref (matrix i j)
  (nth j (nth i matrix)))

(defun direct-expression (edges from to)
  (let* ((matching (remove-if-not
                    (lambda (edge)
                      (and (equal (first edge) from)
                           (equal (third edge) to)))
                    edges))
         (labels (mapcar (lambda (edge)
                           (normalize-label (second edge)))
                         matching)))
    (if labels
        (apply #'make-union labels)
        +empty+)))

(defun base-matrix (states edges)
  (let ((indices (iota (length states))))
    ; Формируем базовую матрицу переходов (R ^ 0)
    (mapcar
     (lambda (i)
       (mapcar
        (lambda (j)
          (let* ((from (nth i states))
                 (to   (nth j states))
                 (direct (direct-expression edges from to)))
            (if (= i j)
                (make-union direct +epsilon+)
                direct)))
        indices))
     indices)))

(defun step-matrix (matrix k)
  (let* ((indices (iota (length matrix)))
         (rkk (matrix-ref matrix k k)))
    ; Вычисляем очередное приближение по формуле R := R | R (R_kk)* R
    (mapcar
     (lambda (i)
       (mapcar
        (lambda (j)
          (make-union
           (matrix-ref matrix i j)
           (make-concat
            (matrix-ref matrix i k)
            (make-star rkk)
            (matrix-ref matrix k j))))
        indices))
     indices)))

(defun iterate-matrix (matrix limit)
  (labels ((advance (k current)
             ; Итеративно улучшаем матрицу по всем промежуточным вершинам
             (if (= k limit)
                 current
                 (advance (1+ k) (step-matrix current k)))))
    (advance 0 matrix)))

;;;; --------------------------
;;;; Алгоритм перевода ДКА в РВ
;;;; --------------------------

(defun automaton-to-regex (edges start final)
  "Метод транзитивного закрытия"
  (let* ((states (remove-duplicates
                  (append (all-states edges) (list start final))
                  :test #'equal))
         (n (length states))
         (base (base-matrix states edges))
         (final-matrix (iterate-matrix base n))
         (i (position start states :test #'equal))
         (j (position final states :test #'equal)))
    (unless (and i j)
      (error "Не удаётся найти старт (~a) или финальное (~a) состояние среди состояний автомата"
             start final))
    (matrix-ref final-matrix i j)))

(defun automaton-to-regex-string (edges start finals)
  (expression->string (automaton-to-regex edges start finals)))

;;;; -------------------------------------------
;;;; Перевод выражения в строку для вывода в CLI
;;;; -------------------------------------------

(defun expression-precedence (e)
  (cond
    ((or (empty-p e) (epsilon-p e) (literal-p e)) 4)
    ((star-p e) 3)
    ((concat-p e) 2)
    ((union-p e) 1)
    (t 0)))

(defun expression->string (expr &optional (outer-prec 0))
  (let* ((my (expression-precedence expr))
         (s
           (cond
             ((empty-p expr) "empty")
             ((epsilon-p expr) "eps")
             ((literal-p expr) (cadr expr))
             ((star-p expr)
              (concatenate 'string (expression->string (cadr expr) 3) "*"))
             ((concat-p expr)
              (apply #'concatenate 'string
                     (mapcar (lambda (p) (expression->string p 2)) (cdr expr))))
             ((union-p expr)
              (format nil "~{~a~^|~}"
                      (mapcar (lambda (p) (expression->string p 1)) (cdr expr))))
             (t (error "Unknown regex node: ~a" expr)))))
    (if (and (< my outer-prec)
             (not (empty-p expr)) (not (epsilon-p expr)) (not (literal-p expr)))
        (format nil "(~a)" s)
        s)))

;;;; --------------------------
;;;; Main запуск всей программы
;;;; --------------------------

(defun main ()
  (multiple-value-bind (edges start finals)
      (read-and-parse)
    (let ((re (automaton-to-regex-string edges start finals)))
      (format T "~a~%" re)
      (finish-output))))

;; авто-вызов при sbcl --script
(when (member :sbcl *features*)
  (main))
