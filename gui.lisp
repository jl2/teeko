;;;; gui.lisp
;;;;
;;;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:teeko)

(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 2) (safety 3) (size 0) (debug 3)))
(define-widget main-window (QMainWindow)
  ((mixer    :initform (mixalot:create-mixer)))
  (:documentation "A Window containing a Teeko widget."))


(define-widget teeko-game-widget (QWidget)
  ((teeko-game :initform (create-teeko-game))
   (holding-piece :initform nil)
   (mixer :initform nil)
   (click-streamer :initform (make-instance 'click-streamer :freq 1200))
   (bad-click-streamer :initform (make-instance 'click-streamer :freq 800))
   (win-streamer :initform (make-instance 'click-streamer :freq 1400))
   (lose-streamer :initform (make-instance 'click-streamer :freq 400)))
  (:documentation "A widget that playes a plays Teeko."))


(define-initializer (teeko-game-widget setup)
  "Turn on mouse tracking.")

(define-slot (teeko-game-widget new-game) ()
  "Start a new game."
  (declare (connected teeko-game-widget (new-game)))
  (setf teeko-game (create-teeko-game))
  (q+:repaint teeko-game-widget))

(define-slot (teeko-game-widget computer-turn) ()
  "Place an edge for the computer."
  (declare (connected teeko-game-widget (computer-turn)))
  (with-slots (opening-game players board current-player) teeko-game
    (let* ((comp-player (aref players current-player))
           (add-function (player-add-function comp-player))
           (move-function (player-move-function comp-player)))

      (mixalot:mixer-add-streamer mixer click-streamer)
      (if opening-game
          (multiple-value-bind (i j) (funcall add-function teeko-game)
            (setf (aref board i j) current-player)
            (push (cons i j) (player-pieces (aref players current-player)))
            (when (= (length (player-pieces (aref players current-player))) 4)
              (setf opening-game nil)))

          (multiple-value-bind (i j ni nj) (funcall move-function teeko-game)
            (with-slots (pieces) (aref players current-player)
              (setf pieces (sort (nsubst (cons ni nj) (cons i j) pieces :test #'equal) #'pt-sort))
              (setf (aref board ni nj) current-player)
              (setf (aref board i j) -1))))

      (q+:repaint teeko-game-widget)
      (if (game-over-p teeko-game)
          (signal! teeko-game-widget (game-over))
          (setf current-player (mod (1+ current-player) 2))))))

(define-slot (teeko-game-widget game-over) ()
  "Handle the end of the game."
  (declare (connected teeko-game-widget (game-over)))
  (with-slots (players ) teeko-game
    (let* ((winner (game-over-p teeko-game))
           (human-won (= winner 0))
           (message (if human-won
                        "You've won!"
                        "You've lost!"))
           (sound (if human-won
                      win-streamer
                      lose-streamer)))
      (mixalot:mixer-add-streamer mixer sound)
      (q+:qmessagebox-information teeko-game-widget "Game Over!" message))))

(define-override (teeko-game-widget paint-event paint) (ev)
  "Handle paint events."
  (with-finalizing 
      ;; Create a painter object to draw on
      ((painter (q+:make-qpainter teeko-game-widget))
       (green-pen (q+:make-qpen (q+:make-qcolor 0 205 0))))

    (with-slots (players current-player board) teeko-game

      ;; Clear the background
      (q+:fill-rect painter (q+:rect teeko-game-widget) (q+:qt.black))
      (q+:set-pen painter green-pen)
      (let* ((height (q+:height teeko-game-widget))
             (width (q+:width teeko-game-widget))
             (smallest (min height width))
             (adjusted-size (+ 2 5))
             (step-size (floor (/ smallest adjusted-size)))
             (colors (make-array 3 :initial-contents (list (q+:qt.blue) (q+:qt.red) (q+:qt.green)))))

        ;; Draw edges
        (loop for i from 0 to 5
           do
             (q+:draw-line painter
                           (+ step-size (* i step-size)) step-size
                           (+ step-size (* i step-size)) (* (1- adjusted-size) step-size))
             (q+:draw-line painter
                           step-size (+ step-size (* i step-size) )
                           (* (1- adjusted-size) step-size) (+ step-size (* i step-size))))
        (dotimes (i 2)
          (dolist (piece (player-pieces (aref players i)))
            (q+:fill-rect painter 
                          (+ 1 step-size (* step-size (car piece) ))
                          (+ 1 step-size (* step-size (cdr piece) ))
                          (1- step-size)
                          (1- step-size)
                          (aref colors i))))

        (when holding-piece
          (dolist (piece (get-empty-adjacent board (car holding-piece) (cdr holding-piece)))
            (q+:fill-rect painter 
                          (+ step-size (* step-size (car piece) ))
                          (+ step-size (* step-size (cdr piece) ))
                          step-size
                          step-size
                          (aref colors 2))))))))

(define-override (teeko-game-widget mouse-release-event mouse-release) (ev)
  "Handle a mouse click by possibly adding a new edge."
  (with-slots (opening-game board players current-player) teeko-game
    (when (and (not (game-over-p teeko-game)) (= current-player 0))
      (let* ((height (q+:height teeko-game-widget))
             (width (q+:width teeko-game-widget))
             (smallest (min height width))
             (adjusted-size (+ 5 2))
             (step-size (floor (/ smallest adjusted-size)))
             (max-click (* 5 step-size))
             (x-loc (- (q+:x ev) step-size))
             (y-loc (- (q+:y ev) step-size))
             
             (i (floor (/ (* 5 x-loc) max-click)))
             (j (floor (/ (* 5 y-loc) max-click))))

        (cond ((or (< x-loc 0) (< y-loc 0 ) (>= i 5) (>= j 5))
               (mixalot:mixer-add-streamer mixer bad-click-streamer))
              
              (opening-game
               (mixalot:mixer-add-streamer mixer click-streamer)
               (setf (aref board i j) current-player)
               (push (cons i j) (player-pieces (aref players current-player)))
               (setf current-player (mod (1+ current-player) 2))
               (q+:repaint teeko-game-widget)
               (signal! teeko-game-widget (computer-turn)))

              ((and holding-piece
                    (= (aref board i j) -1)
                    (find (cons i j) (get-empty-adjacent board (car holding-piece) (cdr holding-piece)) :test #'equal))
               (mixalot:mixer-add-streamer mixer click-streamer)
               (setf (aref board i j) current-player)
               (setf (aref board (car holding-piece) (cdr holding-piece)) -1)
               (with-slots (pieces) (aref players current-player)
                 (setf pieces (sort (nsubst (cons i j) holding-piece pieces :test #'equal) #'pt-sort)))
               (setf holding-piece nil)
               (q+:repaint teeko-game-widget)
               (cond ((game-over-p teeko-game)
                      (signal! teeko-game-widget (game-over)))
                     (t
                      (setf current-player (mod (1+ current-player) 2))
                      (signal! teeko-game-widget (computer-turn)))))

              (holding-piece 
               (mixalot:mixer-add-streamer mixer bad-click-streamer))

              ((and (= (aref board i j) current-player)
                    (get-empty-adjacent board i j))
               (mixalot:mixer-add-streamer mixer click-streamer)
               (setf holding-piece (cons i j))
               (q+:repaint teeko-game-widget))
              (t
               (mixalot:mixer-add-streamer mixer bad-click-streamer)))))))

(define-subwidget (main-window teeko-widget) (make-instance 'teeko-game-widget)
  "The teeko-game-widget itself.")

(define-override (main-window close-event) (ev)
  "Handle close events."
  (mixalot:mixer-remove-all-streamers mixer)
  (mixalot:destroy-mixer mixer)
  (q+:accept ev))

(define-menu (main-window Game)
  (:item ("New Game" (ctrl n))
         (signal! teeko-widget (new-game))
         (q+:repaint main-window))
  (:separator)
  (:item ("Quit" (ctrl q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information main-window "About" "The game of Tweeko.")))

(define-slot (main-window game-size) ((size int))
  "Handle a change in game size.")

(define-initializer (main-window setup)
  "Set the window title and set the teeko-widget to be the central widget."
  (setf (q+:window-title main-window) "Dots And Boxes")
  (setf (slot-value teeko-widget 'mixer) mixer)
  (setf (q+:central-widget main-window) teeko-widget))

(defun main ()
  "Create the main window."
  (trivial-main-thread:call-in-main-thread #'mixalot:main-thread-init)
  (with-main-window (window (make-instance 'main-window))))
