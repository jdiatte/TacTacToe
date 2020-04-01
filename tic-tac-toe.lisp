;; John Diatte
;; DePaul University
;; CSC 358 Peter Hastings
;; Final Project
;; 3/14/20


#| ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

This is a Tic-Tac-Toe game for my final project for CSC 358: Symbolic Programming

2 Players:
	- Player T is the AI (O's)
	- Player NIL is the human (X's)

To play:
	1. Open Lisp in the directory that contains this file
	2. > (load "tic-tac-toe")
	3. > (begin *board*)
	
NOTE:
	The design and implementation of this game is based off of Conrad Barski's Dice of Doom from
	Land of Lisp. While the design is similar and many of function and variable names are the same,
	the only time Barski's code is used exactly is when memoizing the game tree with the variable
	old-game-tree and the function game-tree.
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; |#

	


;; Array representation of the 3-by-3 board
(defparameter *board* (make-array 9))

;; All possible winning lines (by array index)
;; (Add 1 to each to get the displayed numbers from display-board-index and the
;; numbers the human uses to make a move)
(defparameter *win-lines* '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6)))


;; Makes a copy of the board (array)
(defun copy-board (board)
	(make-array (length board) :initial-contents board))

;; Displays the board with numbers (starting from 1) showing
;; which index corresponds to that spot. It's an aid to the player
;; if they get confused about which slot corresponds to the number.
(defun display-board-index ()
	(princ "Here's the board with indices included:")
	(format t "~% 1 | 2 | 3~&")
	(format t " -   -   -~&")
	(format t " 4 | 5 | 6~&")
	(format t " -   -   -~&")
	(format t " 7 | 8 | 9~%"))
	
;; Displays board with NIL replaced by empty spaces
(defun display-board (board)
	;; makes a copy of the board to display the NIL as empty spaces
	;; while not changing the contents of the actual board
	(let ((dboard (copy-board board)))
		(loop for i below (length dboard)
			do (if (not (aref dboard i))
					(setf (aref dboard i) " ")))
		(format t "~% ~a | ~a | ~a~&" (aref dboard 0) (aref dboard 1) (aref dboard 2))
		(format t " -   -   -~&")
		(format t " ~a | ~a | ~a~&" (aref dboard 3) (aref dboard 4) (aref dboard 5))
		(format t " -   -   -~&")
		(format t " ~a | ~a | ~a~%" (aref dboard 6) (aref dboard 7) (aref dboard 8))))

;; t if board is full, otherwise nil
(defun board-full (board)
	(setf bf nil)
	(loop for i below (length board)
		do (if (aref board i)
				(setf bf t)
				(return-from board-full nil)))
	bf)
	
;; Returns an array with the same elements as the list argument
(defun list-to-array (lis)
	(setf arr (make-array (length lis)))
	(loop for i below (length lis)
		do (setf (aref arr i) (nth i lis)))
	arr)
	
;; Returns a list of the indices in the board that are empty (nil)
(defun empty-slots (board)
	(let ((empty nil))
		(loop for i below (length board)
			do (if (equal (aref board i) nil)
					(push i empty)))
	empty))
	
;; "Rule engine" for the game
;; Takes in the current state and gets all the legal possible moves from that state
(defun game-tree (board player)
	(list board player (moves board player)))

;; Memoized game-tree function
;; From Barski Land of Lisp Chapter 15 page 330
;; From Dice of Doom (Version 1)
(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
      (setf (gethash rest previous) (apply old-game-tree rest)))))

;; Function to get all possible moves from the current board state
;; Plays the role of the "successor function" in minimax
(defun moves (board player)
	;; check for empty spots on the board
	(let ((empty (empty-slots board)))
		(mapcan (lambda (x) ;; Make the move at each index in empty and call game-tree with each different board
					(list (game-tree (make-move board player x) (not player)))) empty)))

;; Function to add an X or O to the specified index on the board
;; Returns the updated board
(defun make-move (board player index)
	(if player
		(progn
			(setf o (copy-board board))
			(setf (aref o index) 'O)
			o)
		(progn
			(setf x (copy-board board))
			(setf (aref x index) 'X)
			x)))
			
;; Everything needed for the human player to make their next move
(defun handle-human (tree)
	(fresh-line)
	(format t "Choose your next move.~&")
	(format t "Type in the index of the spot you want to put your X.~&")
	(let ((move (read)))
		(if (equal move 'end)
			nil
			(if (and (integerp move)
					 (> move 0)
					 (< move 10)
					 (member (1- move) (empty-slots (car tree))))
				(new-tree move tree)
				(progn 
					(princ "Invalid selection.")
					(handle-human tree))))))
					
;; Updates the tree after making the move the human player selected
(defun new-tree (move tree)
	(let ((new-tree nil))
		;; Loops through all nine (maximum) DIRECT children of the current board
		;; Goes to the child that exists (not nil) and where the spot has been chosen 
		;; (because in the other children, different selections will be made and
		;; the spot will be open (nil) still)
		(loop for i from 1 to (length (empty-slots (car tree)))
			do (let ((board (car tree)))
				(cond ((equal i 1)
					;; child variable is first, second, third, etc. immediate child in the current tree
					;; child is an array (just the board of child state)
					(let ((child (car (first (caddr tree)))))
						(if (and child (aref child (1- move)))
							(setf new-tree (first (caddr tree))))))
				  ((equal i 2)
					(let ((child (car (second (caddr tree)))))
						(if (and child (aref child (1- move)))
							(setf new-tree (second (caddr tree))))))
				  ((equal i 3)
					(let ((child (car (third (caddr tree)))))
						(if (and child (aref child (1- move)))
							(setf new-tree (third (caddr tree))))))
				  ((equal i 4)
					(let ((child (car (fourth (caddr tree)))))
						(if (and child (aref child (1- move)))
							(setf new-tree (fourth (caddr tree))))))
				  ((equal i 5)
					(let ((child (car (fifth (caddr tree)))))
						(if (and child (aref child (1- move)))
							(setf new-tree (fifth (caddr tree))))))
				  ((equal i 6)
					(let ((child (car (sixth (caddr tree)))))
						(if (and child (aref child (1- move)))
							(setf new-tree (sixth (caddr tree))))))
				  ((equal i 7)
					(let ((child (car (seventh (caddr tree)))))
						(if (and child (aref child (1- move)))
							(setf new-tree (seventh (caddr tree))))))
				  ((equal i 8)
					(let ((child (car (eighth (caddr tree)))))
						(if (and child (aref child (1- move)))
							(setf new-tree (eighth (caddr tree))))))
				  ((equal i 9)
					(let ((child (car (ninth (caddr tree)))))
						(if (and child (aref child (1- move)))
							(setf new-tree (ninth (caddr tree))))))
				  (t (invalid-input tree)))))
		new-tree))

;; Checks to see if a move is a winning move
;; If no winner -> return nil
;; If no winner but game over (board is full i.e. a draw) -> return list (t)
;; If winner -> return list (t player), where player is winner
(defun check-win (board)
	(let ((w nil))
		(loop for x in *win-lines*
				;; if someone won -> three in a row and not nil
			do (if (and (equal (aref board (first x)) (aref board (second x)))
						(equal (aref board (first x)) (aref board (third x)))
						(aref board (first x)))
					;; check who won
					(if (equal (aref board (first x)) 'X)
						(setf w '(t nil)) ;; X win
						(setf w '(t t))))) ;; O win
		;; if no one won and board is full
		(if (and (eq w nil) (board-full board))
			(setf w '(t))) ;; draw 
	w))
	

;; Evaluation Function (Heuristics)
;; E(n) = M(n) - O(n) 
	;; M(n) = total possible winning lines for MAX
	;; O(n) = total possible winning lines for opponent
(defun evaluation (board maks)
	(let ((en 0))
		(let ((ai 0)
			 (hu 0))
			 (loop for n in *win-lines*
					;; number of possible wins for AI
				do (if (and (o-or-nil (aref board (first n))) (o-or-nil (aref board (second n))) (o-or-nil (aref board (third n))))
						(setf ai (1+ ai)))
					;; number of possible wins for human
					(if (and (x-or-nil (aref board (first n))) (x-or-nil (aref board (second n))) (x-or-nil (aref board (third n))))
						(setf hu (1+ hu))))
			;; to decide who max player is
			(if maks
				(setf en (- ai hu)) ;; AI is max
				(setf en (- hu ai)))) ;; human is max
		en))
			
;; helpers to evaluation
(defun x-or-nil (n)
	(or (equal n nil) (equal n 'X)))
	
(defun o-or-nil (n)
	(or (equal n nil) (equal n 'O)))

;; Minimax code
(defun rate-pos (tree player plies)
	(let ((win (check-win (car tree))))
				;; if at depth or board full
		(cond ((or (equal plies 0) (equal (length win) 1))
					;; return the value from the evaluation function
					(evaluation (car tree) player))
				;; if someone won
			  ((equal (length win) 2)
					;; check who won
					(if (second win)
						10 ;; AI won
						-10)) ;; human won
				;; if no one won and game continues
			  ((equal (length win) 0)
					(apply (if (equal (cadr tree) player)
						#'max ;; take max of follow-up moves if it's your move
						#'min) ;; take min of follow-up moves if it's opponent's move
					(get-ratings tree player (1- plies)))))))

;; Maps rate-pos across each available follow-up move for the given branch 
(defun get-ratings (tree player plies)
	(mapcar (lambda (move)
				(rate-pos move player plies))
		(caddr tree)))
		
;; for calling the minimax code to make the AI's move
(defun handle-computer (tree plies)
	(let ((ratings (get-ratings tree (cadr tree) plies)))
			(nth (position (apply #'max ratings) ratings) (caddr tree))))
			
(defun print-info (tree)
	(fresh-line)
	(display-board (car tree)))
	
(defun end-game (win player)
	(fresh-line)
	(if win
		(if player
			(princ "Game over! Player O wins!")
			(princ "Game over! Player X wins!"))
		(princ "Game over! It's a draw!")))

;; to play the game against the computer
(defun play (tree plies)
	(print-info tree)
	(let ((win (check-win (car tree))))
		(let ((winner (length win)))
				  ;; if someone won 
			(cond ((eq winner 2) (end-game t (cadr win)))
				  ;; if it's a draw
				  ((eq winner 1) (end-game nil nil))
				  ;; if the game's still going, choose the next player
				  ((not (cadr tree)) (play (handle-human tree) plies))
				  (t (play (handle-computer tree plies) plies))))))
				  
;; To start the game
;; This is where the difficulty is selected to determine how deep
;; minimax goes throughout the game
(defun begin (board)
	(format t "~%Hello! Welcome to Tic-Tac-Toe!~&")
	(let ((plies nil))
		(labels ((get-dif ()
					(format t "Please choose a difficulty:~%")
					(format t "1. Easy~%")
					(format t "2. Medium~%")
					(format t "3. Hard~%")
					(let ((dif (read)))
						(if (and (integerp dif)
								 (> dif 0)
								 (< dif 4))
								(setf plies dif)
								(progn 
									(princ "Invalid selection.")
									(get-dif))))))
			(get-dif))
		(display-board-index)
		(play (game-tree board nil) plies)))
		
		
		
		
		
		
		
