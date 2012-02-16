;; To play, enter these two lines in the scratch buffer and evaluate (C-x C-e
;; with the point at the end of the line) them in order:
;;
;;  (load-file "chapter_6/wizard_game.el")
;;  (wizard-game)

(defvar nodes '((living-room (you are in the living-room.
				  a wizard is snoring loudly on the couch.))
		(garden (you are in a beautiful garden.
			     there is a well in front of you.))
		(attic (you are in the attic.
			    there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defvar edges '((living-room (garden west door)
			     (attic upstairs ladder))
		(garden (living-room east door))
		(attic (living-room downstairs ladder))))

(defun describe-path (edge)
    `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
    (apply 'append (mapcar 'describe-path (cdr (assoc location edges)))))

(defvar objects '(whiskey bucket frog chain))

(defvar object-locations '((whiskey living-room)
			   (bucket living-room)
			   (chain garden)
			   (frog garden)))

(defun objects-at (loc objs obj-loc)
  (labels ((is-at (obj)
		  (eq (cadr (assoc obj obj-loc)) loc)))
    (remove-if-not 'is-at objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
			 `(you see a ,obj on the floor.)))
    (apply 'append (mapcar 'describe-obj (objects-at loc objs obj-loc)))))

(defvar location 'living-room)

(defun look ()
  (append (describe-location location nodes)
	  (describe-paths location edges)
	  (describe-objects location objects object-locations)))

(defun walk (direction)
  (labels ((correct-way (edge)
			(eq (cadr edge) direction)))
    (let ((next (find-if 'correct-way (cdr (assoc location edges)))))
      (if next
	  (progn (setf location (car next))
		 (look))
	'(you cannot go that way.)))))

(defun pickup (object)
  (cond ((member object (objects-at location objects object-locations))
	 (push (list object 'body) object-locations)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body objects object-locations)))

(defun game-eval-print (command)
  (game-print (game-eval command)))

(defun game-read (input)
  (let ((cmd (car (read-from-string (concat "(" input ")")))))
    (flet ((quote-it (x)
		     (list 'quote x)))
      (cons (car cmd) (mapcar 'quote-it (cdr cmd))))))

(defvar allowed-commands '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) allowed-commands)
      (eval sexp)
    '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eql item ?\s) (cons item (tweak-text rest caps lit)))
	    ((member item '(?! ?? ?.)) (cons item (tweak-text rest t lit)))
	    ((eql item ?\") (tweak-text rest caps (not lit)))
	    ((eql item ?\\) (tweak-text rest caps lit))
	    (lit (cons item (tweak-text rest nil lit)))
	    (caps (cons (upcase item) (tweak-text rest nil lit)))
	    (t (cons (downcase item) (tweak-text rest nil nil)))))))

(defun regex-replace (str regex replacement &optional fixedcase literal)
  "Replace a regular expression in the passed string, if it occurs."
  (or (when (string-match regex str)
	(replace-match replacement fixedcase literal str))
      str))

(defun game-print (lst)
  (coerce
   (tweak-text
    (coerce (regex-replace (regex-replace (prin1-to-string lst) "^[() ]+" "" t t) "[() ]+$" "" t t)
	    'list)
    t nil)
   'string))

;; Major mode and trigger function

(require 'comint)

(setq max-lisp-eval-depth 10000)  ;; needed for tweak-text

(defvar wizard-game-prompt "> ")

(defun wizard-game-process ()
  "Returns the dummy process for this game."
  (get-buffer-process (current-buffer)))

(defun wizard-game-output (output)
  "Writes game output."
  (comint-output-filter (wizard-game-process) output))

(defun wizard-game-input-sender (_proc input)
  "Process input for the game."
  (let ((command (game-read input)))
    (if (eq (car command) 'quit)
	(kill-buffer (current-buffer))
      (wizard-game-output (concat (game-eval-print command) "\n" wizard-game-prompt)))))

(define-derived-mode wizard-game-mode comint-mode "Wizard-Game"
  "A major mode for interactively playing the wizard's game from Land of Lisp."

  (setq comint-prompt-regexp (concat "^" wizard-game-prompt))
  (setq comint-input-sender 'wizard-game-input-sender)

  ;; A dummy process to keep comint happy. It will never get any input
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (condition-case nil
	(start-process "wizard-game" (current-buffer) "hexl")
      (file-error (start-process "wizard-game" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (wizard-game-process) nil)
    (wizard-game-output "*** Wizard's Game ***  Ported from Land of Lisp\n")
    (wizard-game-output (concat (game-eval-print '(look)) "\n" wizard-game-prompt))
    (goto-char (point-max))))

(defun wizard-game ()
    "Starts a session of the Wizard's game from Land of Lisp."
    (interactive)
    (let (old-point)
      (unless (comint-check-proc "*wizard-game*")
	(with-current-buffer (get-buffer-create "*wizard-game*")
	  (unless (zerop (buffer-size)) (setq old-point (point)))
	  (wizard-game-mode)))
      (switch-to-buffer "*wizard-game*")
      (when old-point (push-mark old-point))))
