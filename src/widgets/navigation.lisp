
(in-package :weblocks)

(export '(navigation render-navigation-menu init-navigation make-navigation))


(defwidget navigation (static-selector)
  ((pane-names :accessor navigation-pane-names
	       :initarg :pane-names
	       :initform nil
	       :documentation "An alist mapping url-tokens to
	       human-readable pane names (rendered as a menu). Use nil
	       as the key for the default item.")
   (current-pane :accessor navigation-current-pane :initform nil
		 :documentation "The uri-token corresponding to the
		 currently selected pane, or nil if the default pane is
		 selected."))
  (:documentation "The navigation widget can act as a menu controls, a
  tabbed control, etc. It is a convenience combination of the
  static-selector widget and a menu snippet."))

(defun navigation-pane-name-for-token (navigation token)
  "Return the pane name for a given uri-token or NIL if not found."
  (cdr (assoc token (navigation-pane-names navigation))))

(defgeneric render-navigation-menu (obj &rest args)
  (:documentation "Renders the HTML menu for the navigation widget.")
  (:method ((obj navigation) &rest args)
    (declare (ignore args))
    (render-menu (mapcar (lambda (pane)
			   (cons (navigation-pane-name-for-token obj (car pane))
				 (compose-uri-tokens-to-url (car pane))))
			 (static-selector-panes obj))
		 :selected-pane (navigation-current-pane obj)
		 :header (if (widget-name obj)
			     (humanize-name (widget-name obj))
			     "Navigation")
		 :container-id (ensure-dom-id obj)
		 :empty-message "No navigation entries")))

(defmethod render-widget-body ((obj navigation) &rest args)
  (with-html 
    (:div :class "navigation-body"
	  (mapc #'render-widget (widget-children obj))))
  (apply #'render-navigation-menu obj args))

(defmethod per-class-dependencies append ((obj navigation))
  (list (make-local-dependency :stylesheet "menu")))

(defun init-navigation (obj &rest args)
  (mapc (lambda (pane-info)
	  (let ((token (or (third pane-info) (attributize-name (first pane-info))))
		(name (first pane-info))
		(widget (second pane-info)))
	    (when (string-equal token "")
	      (setf token nil))
	    (push-end (cons token name) (navigation-pane-names obj))
	    (push-end (cons token widget) (static-selector-panes obj))))
	args)
  obj)

(defun make-navigation (name &rest args)
  "Instantiates the default navigation widget via 'make-instance'
and forwards it along with 'args' to 'init-navigation'.

The navigation widgets bears the title NAME."
  (let ((nav (make-instance 'navigation :name name)))
    (apply #'init-navigation nav args)
    nav))

