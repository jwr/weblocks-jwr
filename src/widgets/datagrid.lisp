
(in-package :weblocks)

(export '(datagrid datagrid-data))

(defclass datagrid (widget)
  ((data :accessor datagrid-data
	 :initform nil
	 :initarg :data
	 :documentation "Either a sequence of data objects that will
	 be rendered and modified by this widget, or a function that
	 accepts sorting and paging parameters. If this slot is bound
	 to a sequence, datagrid will do the paging and sourting
	 itself in memory. If the slot is bound to a function, the
	 function is expected to return a properly sorted and paged
	 sequence.")
   (sort :accessor datagrid-sort
	 :initform nil
	 :initarg :sort
	 :documentation "Holds a dotted pair of a name of the sorted
         column and the direction of the sort (:ascending
         or :descending).")
   (allow-sorting :accessor datagrid-allow-sorting
		  :initform t
		  :initarg :allow-sorting
		  :documentation "This slot controls whether the
		  datagrid object should support sorting. If set to
		  t (default), sorting is allowed; if set to nil
		  sorting is disallowed. If set to a list of slot
		  names, only these slots will be available for the
		  user to sort on. Note, if generic functions
		  'strictly-less' and 'equivalent' aren't defined on
		  the datatype of the column, sorting for that column
		  will be turned off regardless of the value of this
		  slot.")))

(defmethod render-table-header-cell :around (obj slot-name slot-value &rest keys
						 &key grid-obj &allow-other-keys)
  (when (or (null grid-obj)
	    (typep slot-value 'standard-object)
	    (not (datagrid-column-sortable-p grid-obj slot-name slot-value)))
    (apply #'call-next-method obj slot-name slot-value keys)
    (return-from render-table-header-cell))
  (apply #'render-datagrid-header-cell obj slot-name slot-value keys))

(defmethod render-datagrid-header-cell (obj slot-name slot-value &rest keys
					&key (human-name slot-name) grid-obj &allow-other-keys)
  (let ((href-class (when (equalp slot-name (car (datagrid-sort grid-obj)))
		      (concatenate 'string "sort-" (string (cdr (datagrid-sort grid-obj)))))))
    (with-html
      (:th :class (concatenate 'string (attributize-name href-class)
			       " " (attributize-name slot-name))
	   (:a :href (make-action-url
		      (make-action (lambda ()
				     (let (slot dir (new-dir :ascending))
				       (unless (null (datagrid-sort grid-obj))
					 (setf slot (car (datagrid-sort grid-obj)))
					 (setf dir (cdr (datagrid-sort grid-obj))))
				       (when (equalp slot slot-name)
					 (setf new-dir (negate-sort-direction dir)))
				       (setf (datagrid-sort grid-obj) `(,slot-name . ,new-dir))))))
	       (str (humanize-name human-name)))))))

(defun datagrid-update-sort-column (grid &rest args)
  (mapc (lambda (column)
	  (let ((column-name (cdr column)))
	    (when (and (null (datagrid-sort grid))
		       (datagrid-column-sortable-p grid column-name
						   (get-slot-value (car (datagrid-data grid)) (car column))))
	      (setf (datagrid-sort grid) (cons column-name :ascending)))))
	(apply #'object-visible-slots (car (datagrid-data grid)) args)))

(defun datagrid-column-sortable-p (grid-obj column-name column-value)
  (and
   (datagrid-allow-sorting grid-obj)
   (if (listp (datagrid-allow-sorting grid-obj))
       (member column-name
	       (datagrid-allow-sorting grid-obj)
	       :test #'equalp)
       t)
   (compute-applicable-methods #'strictly-less (list column-value column-value))
   (compute-applicable-methods #'equivalent (list column-value column-value))))

(defmethod render-widget-body ((obj datagrid) &rest args)
  (apply #'datagrid-update-sort-column obj args)
  (render-table (datagrid-sort-data obj) :grid-obj obj
		:summary (format nil "Ordered by ~A ~A."
				 (humanize-name (car (datagrid-sort obj)))
				 (humanize-name (cdr (datagrid-sort obj))))))

(defun datagrid-sort-data (grid-obj)
  (with-slots (data sort) grid-obj
    (if (and (typep data 'sequence) sort)
	(setf data (sort data (if (equalp (cdr sort) :ascending)
				  #'strictly-less
				  (lambda (a b)
				    (and (not (strictly-less a b))
					 (not (equivalent a b)))))
			 :key (curry-after #'slot-value (car sort))))
	data)))

(defmethod strictly-less ((a number) (b number))
  (< a b))

(defmethod strictly-less ((a string) (b string))
  (string-lessp a b))

(defmethod equivalent (a b)
  (equalp a b))

(defmethod datagrid-data ((grid-obj datagrid))
  (with-slots (data) grid-obj
    (etypecase data
      (function (funcall data))
      (sequence data))))

(defun negate-sort-direction (dir)
  (ecase dir
    (:ascending :descending)
    (:descending :ascending)))
