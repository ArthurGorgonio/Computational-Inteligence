
(defun create-board ()
  (list (list nil nil nil) (list nil nil nil) (list nil nil nil) )
)

;;;;;;;;;;;;;;;
;; variables ;;
;;;;;;;;;;;;;;;

(defvar board (create-board))
(defvar cpu "O")
(defvar human "X")
(defvar current human)
(load "ai.lisp") ;; in this file have the ai to win the game

;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;

(defun adp (row col)
  (equalp (element board row col) human)
)

(defun change-turn ()
  (if (equal current human) (setq current cpu) (setq current human))
)

(defun check-draw ()
  (cond ((equal (nil-board board) 0) (write-line "Draw") t)
        (t nil)
  )
)

(defun check-win ()
  (cond ( (or (and (mep 0 0) (mep 0 1) (mep 0 2))
              (and (mep 1 0) (mep 1 1) (mep 1 2))
              (and (mep 2 0) (mep 2 1) (mep 2 2))
              (and (mep 0 0) (mep 1 0) (mep 2 0))
              (and (mep 0 1) (mep 1 1) (mep 2 1))
              (and (mep 0 2) (mep 1 2) (mep 2 2))
              (and (mep 0 0) (mep 1 1) (mep 2 2))
              (and (mep 0 2) (mep 1 1) (mep 2 0))
          )
          (write-line "You Lose!")
          (return-from check-win t)
        )
        ( (or (and (adp 0 0) (adp 0 1) (adp 0 2))
              (and (adp 1 0) (adp 1 1) (adp 1 2))
              (and (adp 2 0) (adp 2 1) (adp 2 2))
              (and (adp 0 0) (adp 1 0) (adp 2 0))
              (and (adp 0 1) (adp 1 1) (adp 2 1))
              (and (adp 0 2) (adp 1 2) (adp 2 2))
              (and (adp 0 0) (adp 1 1) (adp 2 2))
              (and (adp 0 2) (adp 1 1) (adp 2 0))
          )
          (write-line "You Win!")
          (return-from check-win t)
        )
        (t nil)
  )
)

(defun cpu-turn ()
  (write-line "CPU thinking...")
  (loop for action in ai do
    (if (eval (car action) )
      (return-from cpu-turn (eval (cdr action)))
    )
  )
  (random-turn)
)

(defun draw-board (row col current)
  (setf
    (nth col (nth row board)) current
  )
)

(defun element (board row col)
  (nth col (nth row board))
)

(defun human-turn ()
  (let (row col)
    (loop
      (write-line "Your turn!")
      (setq row (read))
      (setq col (read))
      (when (valid-turn row col)
        (return (cons row col))
      )
    )
  )
)

(defun mep (row col)
  (equalp (element board row col) cpu)
)

(defun nil-board (board)
  (let ((temp 0))
    (loop for i in board do
      (setq temp (+ temp (count nil i)))
    )
    temp
  )
)

(defun nonep (row col)
  (equalp (element board row col) nil)
)

(defun print-board (board)
  (let ((a 0))
    (terpri)
    (format t "       0  1  2~%")
    (format t "      ---------~%")
    (loop for i in board do
      (format t " ~D #   " a)
      (loop for j in i do
        (if (equal j nil) (format t "_  ") (format t "~A  " j))
      )
      (setq a (+ a 1))
      (terpri)
    )
    (terpri)
  )
)

(defun random-turn ()
  (setq *random-state* (make-random-state t))
  (let (row col)
    (loop
      (setq row (random 3))
      (setq col (random 3))
      (when (valid-move row col)
        (return (cons row col))
      )
    )
  )
)

(defun valid-turn (row col)
  (cond ((or (> row 2) (< row 0)) nil)
        ((or (> col 2) (< col 0)) nil)
        (t (nonep row col))
  )
)

;;;;;;;;;;
;; main ;;
;;;;;;;;;;

(defun main ()
  (let (row col pair)
    (print-board board)
    (loop
      (if (equal current human)
        (setq pair (human-turn))
        (setq pair (cpu-turn))
      )
      (setq row (car pair))
      (setq col (cdr pair))
      (draw-board row col current)
      (change-turn)
      (print-board board)
      (when
        (or (check-win)
            (check-draw)
        )
        (return)
      )
    )
  )
)

(main)

(terpri)
