(defvar small 1)
(defvar big 100)

(defun guess-my-number ()
     (ash (+ small big) -1))

(defun smaller ()
     (setq big (1- (guess-my-number)))
     (guess-my-number))

(defun bigger ()
     (setq small (1+ (guess-my-number)))
     (guess-my-number))

(defun start-over ()
   (setq small 1)
   (setq big 100)
   (guess-my-number))
