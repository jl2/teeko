;;;; teeko.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

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
  (with-slots (n freq phase playing) streamer
    (loop for index upfrom offset
       repeat length
       with dp = (* 2.0 pi freq 1/44100)
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

(defun move-computer (game)
  "Move randomly."
  (with-slots (board current-player players) game
    (let* ((moving 
            (loop for moving = (nth (random 4) (player-pieces (aref players current-player)))
               then (nth (random 4) (player-pieces (aref players current-player)))
               until (get-empty-adjacent board (car moving) (cdr moving))
               finally (return moving)))
           (adjacent (get-empty-adjacent board (car moving) (cdr moving)))
           (goes-to (nth (random (length adjacent)) adjacent)))
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
                                                              :move-function #'move-computer)))
              :current-player 0))


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
