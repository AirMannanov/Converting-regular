(in-package #:cl-user)

;;;; ---------------
;;;; Валидация входа
;;;; ---------------

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defun edge-form-p (edge)
  "Ребро — это список из трёх элементов: (from label to)"
  (and (listp edge) (= (length edge) 3)))

(defun plistp-with-keywords (x)
  "Проверка, что x — plist, начинающийся с keyword"
  (and (listp x) (keywordp (first x))))

(defun valid-label-p (label)
  "Метка – символ."
  (symbolp label))

(defun valid-state-p (s)
  "Состояние – символ или строка."
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
     (values data 'H '(S)))

    (T
     (error "Неподдерживаемый формат автомата: ~s" data))))

(defun read-and-parse (&optional (stream *standard-input*))
  "Считывает форму и парсит; возвращает три значения"
  (parse-automaton (read-automaton-form stream)))

;;;; ----------------------
;;;; Main и обёртки для CLI
;;;; ----------------------

(defun main ()
  (read-and-parse))

;; Если запущено через sbcl --script — вызываем main
(when (member :sbcl *features*)
  (main))

;;; Примеры запуска:
;;;   echo '((H a S) (S b S))' | sbcl --script automaton-regex.lisp

