;;;; teeko.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:teeko)

;;; "teeko" goes here. Hacks and glory await!

(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 0) (safety 3) (size 0) (debug 3)))

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
  (with-slots (n freq phase playing) streamer
    (loop for index upfrom offset
       repeat length
       with dp = (* 2.0 pi freq 1/44100)
       as sample = (round (* 1500 (sound-function phase)))
       do
         (mixalot:stereo-mixf (aref buffer index) (mixalot:mono->stereo sample))
         (incf phase dp)))
  (mixalot:mixer-remove-streamer mixer streamer))

(defun get-computer-move (game)
  "Find a move for the computer."
  (values 0 0))

(defun add-computer (teeko)
  (with-slots (board) teeko
    (loop
       for i = (random (array-dimension board 0)) then (random (array-dimension board 0))
       for j = (random (array-dimension board 1)) then (random (array-dimension board 1))
       until (empty-p board i j)
       finally (return (values i j)))))

(defstruct player
  "A player object containing a score, a function for getting a next move."
  (name "player" :type string)
  (pieces nil :type list)
  (add-function nil)
  (move-function nil))

(defstruct teeko
  "A structure representing a Dots and Boxes game."
  (board (make-array '(5 5) :initial-element -1))
  (players (make-array 2 :initial-contents '((make-player)
                                             (make-player))))
  (current-player 0))

(defun game-over-p (teeko)
  "Check if the game is over yet."
  nil)

(defstruct teeko-pt
  (i 0)
  (j 0))

(defun create-teeko-game ()
  "Create a computer vs human Teeko game."
  (make-teeko :board (make-array '(5 5) :initial-element -1)
              :players (make-array 2 :initial-contents '((make-player
                                                          :name "Human"
                                                          :add-function nil
                                                          :move-function nil)
                                                         (make-player 
                                                          :name "Computer"
                                                          :add-function #'add-random
                                                          :move-function #'get-computer-move)))
              :current-player 0))

(defun show-teeko-board (teeko)

  ;; Top row of column numbers
  (terpri)
  (format t "  ")
  (dotimes (i 5)
    (format t "  ~a   " i))
  (terpri)
  (with-slots (board players) teeko
    (dotimes (j 5)

      ;; Empty space to account for row number
      (format t "   ")

      ;; Top of box
      (dotimes (i 5)
        (format t "+---+ "))
      (terpri)

      ;; Row number
      (format t "~2d " j)

      ;; Boxes with player info
      (dotimes (i 5)
        (cond ((= (aref board i j) -1)
               (format t "|   |-"))
              (t
               (format t "| ~1d |-" (aref board i j)))))
      (terpri)

      ;; Empty space for row number
      (format t "   ")

      ;; Bottom of boxes
      (dotimes (i 5)
        (format t "+---+ "))
      (terpri)

      ;; Empty space for row number
      (format t "   ")

      ;; Connecting lines at bottom
      (when (/= j 4)
        (dotimes (i 5)
          (format t "  |   "))
        (terpri)))))

(defun empty-p (board i j)
  (= (aref board i j) -1))

(defun add-player (teeko)
  (with-slots (board) teeko
    (loop
       for out = (format t "Where do you want to place your piece? ")
       then (format t "That position was invalid!~%Where do you want to place your piece? ")
       for i = (read) then (read)
       for j = (read) then (read)
       until (and (integerp i)
                  (integerp j)
                  (>= i 0)
                  (>= j 0)
                  (< i (array-dimension board 0))
                  (< i (array-dimension board 1))
                  (empty-p board i j))
       finally (return (values i j)))))

(defun get-player-move (teeko)
  (values 0 0))

(defun repl-teeko (&optional (name "Human"))
  (let ((teeko (make-teeko :board (make-array '(5 5) :initial-element -1)
                           :players (make-array 2 :initial-contents (list (make-player
                                                                           :name name
                                                                           :add-function #'add-player
                                                                           :move-function #'get-player-move)
                                                                          (make-player 
                                                                           :name "Computer"
                                                                           :add-function #'add-computer
                                                                           :move-function #'get-computer-move)))
                           :current-player 0)))
    (with-slots (board players current-player) teeko
      (dotimes (i 4)
        (dotimes (j 2)
          (show-teeko-board teeko)
          (multiple-value-bind (x y) (funcall (player-add-function (aref players j)) teeko)
            (setf (aref board x y) j)
            (push (player-pieces ))))
    (show-teeko-board teeko)))







(define-widget main-window (QMainWindow)
  ((next-game-size :initform 5 :type fixnum)
   (mixer    :initform (mixalot:create-mixer)))
  (:documentation "A Window containing a Teeko widget."))


(define-widget teeko-game-widget (QWidget)
  ((teeko-game :initform (create-teeko-game 5))
   (mixer :initform nil)
   (click-streamer :initform (make-instance 'click-streamer :freq 1200))
   (win-streamer :initform (make-instance 'click-streamer :freq 1400))
   (lose-streamer :initform (make-instance 'click-streamer :freq 400)))
  (:documentation "A widget that playes a plays Teeko."))


(define-initializer (teeko-game-widget setup)
  "Turn on mouse tracking."
  (setf (q+:mouse-tracking teeko-game-widget) t))


(define-slot (teeko-game-widget new-game) ((size int))
  "Start a new game."
  (declare (connected teeko-game-widget (new-game int)))
  (setf teeko-game (create-teeko-game size))
  (q+:repaint teeko-game-widget))

(define-slot (teeko-game-widget computer-turn) ()
  "Place an edge for the computer."
  (declare (connected teeko-game-widget (computer-turn)))
  (with-slots (players ) teeko-game
    (multiple-value-bind (v1 v2) (funcall (player-edge-function computer-player) graph)
      (let ((result (handle-new-edge teeko-game v1 v2 mixer box-streamer click-streamer)))
        (q+:repaint teeko-game-widget)
        (cond ((eq result :game-over) (signal! teeko-game-widget (game-over)))
              ((eq result :computer) (signal! teeko-game-widget (computer-turn))))))))

(define-slot (teeko-game-widget game-over) ()
  "Handle the end of the game."
  (declare (connected teeko-game-widget (game-over)))
  (with-slots (computer-player human-player) teeko-game
    (let* ((player-score (player-score human-player))
           (computer-score (player-score computer-player))
           (tie (= player-score computer-score))
           (human-won (> player-score computer-score))
           (message (if tie
                        "It was a tie! Try again!"
                        (if human-won
                            (format nil "You won, ~a to ~a!" player-score computer-score)
                            (format nil "You've lost! ~a to ~a!" player-score computer-score))))
           (sound (if tie
                      win-streamer
                      (if human-won
                          win-streamer
                          lose-streamer))))
      (mixalot:mixer-add-streamer mixer sound)
      (q+:qmessagebox-information teeko-game-widget "Game Over!" message))))

(define-override (teeko-game-widget paint-event paint) (ev)
  "Handle paint events."

  (with-finalizing 
      ;; Create a painter object to draw on
      ((painter (q+:make-qpainter teeko-game-widget))
       (green-pen (q+:make-qpen (q+:make-qcolor 0 205 0)))
       (red-pen (q+:make-qpen (q+:make-qcolor 205 0 0))))

    (with-slots (graph squares owners) teeko-game

      ;; Clear the background
      (q+:fill-rect painter (q+:rect teeko-game-widget) (q+:qt.black))
      (q+:set-pen painter green-pen)
      (let* ((height (q+:height teeko-game-widget))
             (width (q+:width teeko-game-widget))
             (smallest (min height width))
             (adjusted-size (+ 2 (teeko-game-size teeko-game)))
             (step-size (floor (/ smallest adjusted-size)))
             (points (graph-points graph)))

        ;; Draw filled squares
        (flet ((draw-square (square &optional temp)
                 (let ((pt (aref (graph-points graph) square)))
                   (q+:fill-rect painter 
                                 (+ step-size (* step-size (point-x-loc pt) ))
                                 (+ step-size (* step-size (point-y-loc pt) ))
                                 step-size
                                 step-size
                                 (if temp
                                     (q+:qt.blue)
                                     (cdr (assoc square owners)))))))

          ;; Finished squares
          (dolist (square squares)
            (draw-square square))

          ;; Potentially finished by player's next move
          (when two-closest
            (let ((v0 (cdar two-closest))
                  (v1 (cdadr two-closest)))
              (when (not (has-edge-p graph v0 v1 ))
                (add-edge graph v0 v1)
                (let ((new-squares (set-difference (find-complete-squares graph) squares)))
                  (dolist (square new-squares)
                    (draw-square square t)))
                (remove-edge graph v0 v1)))))

        ;; Draw edges
        (loop for i from 0
           for edges across (graph-edges graph)
           do
             (dolist (vert edges)
               (when (< i vert)
                 (let ((pt1 (aref points i))
                       (pt2 (aref points vert)))
                   (q+:draw-line painter
                                 (+ step-size (* step-size (point-x-loc pt1)))
                                 (+ step-size (* step-size (point-y-loc pt1)))
                                 (+ step-size (* step-size (point-x-loc pt2)))
                                 (+ step-size (* step-size (point-y-loc pt2))))))))

        ;; Draw player's potential next move
        (when (and two-closest (not (has-edge-p graph (cdar two-closest) (cdadr two-closest))))
          (let ((pt1 (aref points (cdar two-closest)))
                (pt2 (aref points (cdadr two-closest))))
            (q+:set-pen painter red-pen)
            (q+:draw-line painter
                          (+ step-size (* step-size (point-x-loc pt1)))
                          (+ step-size (* step-size (point-y-loc pt1)))
                          (+ step-size (* step-size (point-x-loc pt2)))
                          (+ step-size (* step-size (point-y-loc pt2))))))

        ;; Draw points
        (q+:set-pen painter green-pen)
        (loop for pt across points
           do
             (let ((pt-x (- (+ step-size (* step-size (point-x-loc pt) )) 10))
                   (pt-y (- (+ step-size (* step-size (point-y-loc pt) )) 10)))
               (q+:draw-arc painter
                            pt-x
                            pt-y
                            20 20
                            0 (* 16 360))))))))

(defun distance-squared (x1 y1 x2 y2)
  "Squared distance between two points."
  (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))))

(defun find-two-closest (x y step-size graph)
  "Return the two closest points to point x,y in graph."
  (sort (loop 
           for idx from 0
           for pt across (graph-points graph)
           collecting (cons (distance-squared x y 
                                              (+ step-size (* step-size (point-x-loc pt)))
                                              (+ step-size (* step-size (point-y-loc pt))))
                            idx))
        #'<
        :key #'car))


(define-override (teeko-game-widget mouse-release-event mouse-release) (ev)
  "Handle a mouse click by possibly adding a new edge."
  (with-slots (game-size squares players graph owners current-player) teeko-game
    (when (and (not (game-over-p teeko-game)) (eq current-player :human))
      (let* ((height (q+:height teeko-game-widget))
             (width (q+:width teeko-game-widget))
             (smallest (min height width))
             (adjusted-size (+ 2 (teeko-game-size teeko-game)))
             (step-size (floor (/ smallest adjusted-size)))

             (x-loc (q+:x ev))
             (y-loc (q+:y ev))
             (closest (find-two-closest x-loc y-loc step-size graph))
             (v1 (cdar closest))
             (v2 (cdadr closest)))
        (when (not (has-edge-p graph v1 v2))
          (setf two-closest nil)
          (let ((result (handle-new-edge teeko-game v1 v2 mixer click-streamer box-streamer)))
            (cond ((eq result :game-over) (signal! teeko-game-widget (game-over)))
                  ((eq result :computer) (signal! teeko-game-widget (computer-turn)))))
          (q+:repaint teeko-game-widget))))))

(define-override (teeko-game-widget mouse-move-event mouse-move) (ev)
  "Find the closest edge to the user's mouse and highlight it if it's not already in the graph."
  (let* ((height (q+:height teeko-game-widget))
         (width (q+:width teeko-game-widget))
         (smallest (min height width))
         (adjusted-size (+ 2 (teeko-game-size teeko-game)))
         (step-size (floor (/ smallest adjusted-size)))
         (graph (teeko-graph teeko-game))
         (x-loc (q+:x ev))
         (y-loc (q+:y ev))
         (closest (find-two-closest x-loc y-loc step-size graph))
         (v1 (cdar closest))
         (v2 (cddr closest)))

    (if (not (has-edge-p graph v1 v2))
        (setf two-closest closest)
        (setf two-closest nil)))
  (q+:repaint teeko-game-widget))

(define-subwidget (main-window teeko-widget) (make-instance 'teeko-game-widget)
  "The teeko-game-widget itself.")

(define-override (main-window close-event) (ev)
  "Handle close events."
  (mixalot:mixer-remove-all-streamers mixer)
  (mixalot:destroy-mixer mixer)
  (q+:accept ev))

(define-menu (main-window Game)
  (:item ("New Game" (ctrl n))
         (signal! teeko-widget (new-game int) next-game-size)
         (q+:repaint main-window))
  (:separator)
  (:item ("Quit" (ctrl q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information main-window "About" "The game of Tweeko.")))

(define-slot (main-window game-size) ((size int))
  "Handle a change in game size."
  (setf next-game-size size))

(define-initializer (main-window setup)
  "Set the window title and set the teeko-widget to be the central widget."
  (setf (q+:window-title main-window) "Dots And Boxes")
  (setf (q+:mouse-tracking main-window) t)
  (setf (slot-value teeko-widget 'mixer) mixer)
  (setf (q+:central-widget main-window) teeko-widget))

(defun main ()
  "Create the main window."
  (trivial-main-thread:call-in-main-thread #'mixalot:main-thread-init)
  (with-main-window (window (make-instance 'main-window))))
