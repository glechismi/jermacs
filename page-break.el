;; * Page-break

(defvar page-break-face 'default)
(defvar page-break-string-char ?-)

(defun page-break-display-table (window)
  "Create a display-table that displays page-breaks prettily."
  (let ((table (or (copy-sequence (window-display-table window))
                   (make-display-table))))
    (aset table ?\^L
          (let ((face-offset (lsh (face-id page-break-face) 19)))
            (vconcat (mapcar (lambda (c) (+ face-offset c)) 
			     (make-string (1- (window-width window))
					  page-break-string-char)))))
    table))


(defun page-break-mode-hook-function  ()
  "Function called for updating display table"
  (mapcar (lambda (window) 
	    (set-window-display-table window 
				      (page-break-display-table window)))
	  (window-list nil 'no-minibuffer)))

(define-minor-mode page-break-mode
  "Toggle Page Break mode.

In Page Break mode, page breaks (^L characters) are displayed as a
horizontal line of `page-break-string-char' characters."
  :global t 
  :lighter " Pgbrk" 
  (if page-break-mode
      (add-hook 'window-configuration-change-hook 
		'page-break-mode-hook-function )
    (remove-hook 'window-configuration-change-hook 
		 'page-break-mode-hook-function)))

(defun turn-on-page-break-mode ()
  (page-break-mode 1))

(defun turn-off-page-break-mode ()
  (page-break-mode -1))

(turn-on-page-break-mode)

(provide 'page-break)
