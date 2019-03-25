;;;; teeko.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:teeko)

(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 2) (safety 3) (size 0) (debug 3)))

(defclass click-streamer ()
  ((freq  :initform 440.0d0 :initarg :freq)
   (phase :initform 0.0d0))
  (:documentation "Streamer object for generating sounds."))

(defun sound-function (phase)
  "Generate a sound wave."
  (loop for val from 1 to 2 summing (+ (sin (/ phase val)) (sin (* phase val)))))

(defmethod mixalot:streamer-mix-into ((streamer click-streamer) mixer buffer offset length time)
  "Streamer callback that plays a sound into the mixer."
  (declare (ignore time))
  (with-slots (freq phase) streamer
    (loop for index upfrom offset
       repeat length
       with dp = (* 2.5 pi freq 1/44100)
       as sample = (round (* 200 (sound-function phase)))
       do
         (mixalot:stereo-mixf (aref buffer index) (mixalot:mono->stereo sample))
         (incf phase dp)))
  (mixalot:mixer-remove-streamer mixer streamer))


(defstruct player
  "A player object containing a score, a function for getting a next move."
  (name "player" :type string)
  (pieces nil :type list)
  (add-function nil)
  (move-function nil))

(defstruct teeko
  "A structure representing a Dots and Boxes game."
  (board (make-array '(5 5) :initial-element -1))
  (opening-game t)
  (players (make-array 2 :initial-contents (list (make-player)
                                                 (make-player))))
  (current-player 0))

(defun pt-sort (a b)
  (if (/=  (car a ) (car b)) (< (car a) (car b)) (< (cdr a) (cdr b))))

(defun game-over-p (teeko)
  "Check if the game is over.  Returns the winning player number or nil."
  (with-slots (players current-player) teeko
    (if (every (lambda (pl) (= (length (player-pieces pl)) 4)) players)
        (dotimes (i 2)
          (with-slots (pieces) (aref players i)
            (setf pieces (sort pieces #'pt-sort))
            (let* ((same-car (every (lambda (val) (= (car val) (caar pieces))) pieces))
                   (cdr-diff (- (cdar (last pieces)) (cdar pieces)))
                   (same-cdr (every (lambda (val) (= (cdr val) (cdar pieces))) pieces))
                   (car-diff (- (caar (last pieces)) (caar pieces)))
                   (differences (loop for i in pieces for j in (cdr pieces)
                                   collecting (cons (- (car j) (car i)) (- (cdr j) (cdr i)))))
                   (diagnol-down (every
                                  (curry #'equal '(1 . 1))
                                  differences))
                   (diagnol-up (every
                                (curry #'equal '(1 . -1))
                                differences)))
              (when (or
                     (and same-car (= cdr-diff 3))
                     (and same-cdr (= car-diff 3))
                     diagnol-down
                     diagnol-up)
                (return-from game-over-p i)))))
        nil)))

(defun empty-p (board i j)
  "Check if board position i j is empty."
  (= (aref board i j) -1))


(defun get-status-at (board i j)
  "Return nil if there's no player at board position i j or if the position is invalid.  Otherwise return which player is there."
  (cond ((or (< i 0) (< j 0) (>= i 5) (>= j 5))
         nil)
        (t
         (aref board i j))))

(defun get-empty-adjacent (board i j)
  "Find squares adjacent to i j that are empty."
  (let ((empty nil))
    (loop for a from -1 to 1
       do
         (loop for b from -1 to 1
            for bl = (get-status-at board (+ a i) (+ b j)) then (get-status-at board (+ a i) (+ b j))
            when (and bl (= bl -1))
            do (push (cons (+ i a) (+ j b)) empty)))
    empty))

(defun proxy-move-computer (game)
  "This is a trick to allow recompiling from Slime.  Since this is called by funcall, it doesn't seem to get updated
   when move-computer is recompiled."
  (move-computer game))

(defun move-computer (game)
  "Move randomly."
  (with-slots (board current-player players) game
    (let* ((moving 
            (with-slots (pieces) (aref players current-player)
              ;; Pick a random piece with empty adjacent squares
              (loop for moving = (nth (random 4) pieces) then (nth (random 4) pieces)
                 until (get-empty-adjacent board (car moving) (cdr moving))
                 finally (return moving))))

           ;; Get the adjacent spots
           (adjacent (get-empty-adjacent board (car moving) (cdr moving)))

           ;; Pick one at random
           (goes-to (nth (random (length adjacent)) adjacent)))
      ;; Return the coordinates of the moving piece and where it's going to
      (values (car moving) (cdr moving) (car goes-to) (cdr goes-to)))))

(defun add-computer (teeko)
  "Randomly add a game piece for the computer."
  (with-slots (board) teeko
    (loop
       for i = (random 5) then (random 5)
       for j = (random 5) then (random 5)
       until (empty-p board i j)
       finally (return (values i j)))))

(defun create-teeko-game ()
  "Create a computer vs human Teeko game."
  (make-teeko :board (make-array '(5 5) :initial-element -1)
              :players (make-array 2 :initial-contents (list (make-player
                                                              :name "Human"
                                                              :add-function nil
                                                              :move-function nil)
                                                             (make-player
                                                              :name "Computer"
                                                              :add-function #'add-computer
                                                              :move-function #'proxy-move-computer)))
              :current-player 0))



