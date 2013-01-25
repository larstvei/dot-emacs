;-------------[ This function creates headers like this one ]-------------;

(defun header ()
  (interactive)
  (beginning-of-line)
  (if (and (eolp) (bolp))
    (insert ";;")
      (progn
        (insert ";[ ")
        (move-end-of-line 1)
        (insert " ];")))
  (add-dashes))

(defun add-dashes ()
  ;; lets wait, just for fun.
  (sit-for 0.006)
  (end-of-line)
  (let ((line-length (current-column)))
    (if (oddp line-length)
	(progn                                                 
	  (beginning-of-line)
	  (forward-char))
      (backward-char))
    (insert "-")
    (unless (< 73 line-length)                                
      (add-dashes))))
