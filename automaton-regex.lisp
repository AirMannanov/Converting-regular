(in-package #:cl-user)

;;;; ------------------------------------
;;;; Чтение входных данных и их валидация
;;;; ------------------------------------

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defun edge-form-p (edge)
  "Ребро — это список из трёх элементов: (from label to)"
  (and (listp edge) (= (length edge) 3)))

(defun plistp-with-keywords (x)
  "Проверка, что x — plist, начинающийся с keyword"
  (and (listp x) (keywordp (first x))))

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
    (dolist (e xs)
      (cond
        ((empty-p e) nil)
        (T (push e items))))
    (cons :union items)))

(defun make-concat (&rest xs)
  (let ((items '()))
    (dolist (e xs)
      (cond
        ((empty-p e) e)
        ((epsilon-p e) nil)
        (T (push e items))))
    (cons :concat items)))

(defun make-star (e)
  (cond
    ((empty-p e) +epsilon+)
    ((epsilon-p e) +epsilon+)
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
  (automaton-to-regex edges start finals))

;;;; --------------------------
;;;; Main запуск всей программы
;;;; --------------------------

(defun main ()
  (multiple-value-bind (edges start finals)
      (read-and-parse)
    (let ((re (automaton-to-regex-string edges start finals)))
      (format t "~a~%" re)
      (finish-output))))

;; авто-вызов при sbcl --script
(when (member :sbcl *features*)
  (main))

;;; Примеры запуска:
;;;   echo '((H a S) (S b S))' | sbcl --script automaton-regex.lisp