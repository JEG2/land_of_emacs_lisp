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
