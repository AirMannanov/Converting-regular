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
     (dolist (e data) (validate-edge e))
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
  (let ((items '()))
    ; Заполняем список элементов объединения
    (dolist (e xs)
      (cond
        ((empty-p e) nil)
        ; Аккумулируем знак объединения
        ((union-p e) (setf items (append items (copy-list (cdr e)))))
        (T (push e items))))
    ; Убираем дупликаты
    (setf items (remove-duplicates items :test #'equal))
    ; Рассматриваем тривиальные случаи и выдаём итоговое объединение
    (cond
      ((null items) +empty+)
      ((null (cdr items)) (car items))
      (T (cons :union items)))))


(defun make-concat (&rest xs)
  (let ((items '()))
    ; Заполняем список элементов конкатенации
    (dolist (e xs)
      (cond
        ; Досрочно выходим из функции если есть пустой символ
        ((empty-p e) (return-from make-concat +empty+))
        ((epsilon-p e) nil)
        ; Аккумулируем знак конкатенации
        ((concat-p e) (setf items (append items (copy-list (cdr e)))))
        (T (setf items (append items (list e))))))
    ; Рассматриваем тривиальные случаи и выдаём итоговую конкатенацию
    (cond
      ((null items) +epsilon+)
      ((null (cdr items)) (car items))
      (T (cons :concat items)))))

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

(defun copy-matrix (m)
  "Копируем матрицу для следующей итерации алгоритма"
  (let* ((dims (array-dimensions m))
         (copied  (make-array dims)))
    (dotimes (k (array-total-size m) copied)
      (setf (row-major-aref copied k)
            (row-major-aref m  k)))))

;;;; --------------------------
;;;; Алгоритм перевода ДКА в РВ
;;;; --------------------------

(defun automaton-to-regex (edges start final)
  "Метод транзитивного закрытия"
  (let* ((states (all-states edges))
         (n      (length states))
         (R      (make-array (list n n) :initial-element +empty+)))

      ; Оперделим локальную функцию поиска индекса состояния
      (flet ((i-of (s)
        (position s states)))

      ; Добавить исходные ребра в матрицу. 
      ; По факту инициализация матрицы начальными значениям (R ^ 0)
      (dolist (e edges)
        (destructuring-bind (from lab to) e
          (let* ((i (i-of from))
                 (j (i-of to))
                 (x (normalize-label lab)))
            (setf (aref R i j) (make-union (aref R i j) x)))))

      ; Добавляем eps на диагональные элементы матрицы.
      ; По факту разбираемся с путями нулевой длины (т.е. из состояния q_i в q_i переход eps)
      (dotimes (i n)
        (setf (aref R i i) (make-union (aref R i i) +epsilon+)))

      ; Итерационно заполняем матрицу
      ; На k-ой итераии R := R | R (R_kk)* R
      (dotimes (k n)
        (let ((prev (copy-matrix R)))
          (dotimes (i n)
            (dotimes (j n)
              (setf (aref R i j)
                    (make-union
                     (aref prev i j)
                     (make-concat (aref prev i k)
                                  (make-star (aref prev k k))
                                  (aref prev k j))))))))

      ; Ответ
      (aref R (i-of start) (i-of final)))))


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



