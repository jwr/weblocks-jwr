
(in-package :weblocks)

(export '(navigation render-navigation-menu init-navigation make-navigation
	  navigation-render-content))

(defwidget navigation (static-selector)
  ((pane-names :accessor navigation-pane-names
	       :initarg :pane-names
	       :initform nil
	       :documentation "An alist mapping uri-tokens to
	       human-readable pane names (rendered as a menu). Use nil
	       as the key for the default item.")
   (header :accessor navigation-header
	   :initarg :header
	   :initform nil
	   :documentation "A heading that will be rendered in a <h1> tag")
   (hidden-panes :accessor navigation-hidden-panes
		 :initarg :hidden-panes
		 :initform nil
		 :documentation "A list of uri-tokens corresponding to
		 panes that should be hidden (not rendered in a menu,
		 but accessible from within this navigation object.)")
   (render-content :accessor navigation-render-content
		   :initarg :render-content
		   :initform t
		   :documentation "Whether navigation should also render its contents"))
  (:documentation "The navigation widget can act as a menu controls, a
  tabbed control, etc. It is a convenience combination of the
  static-selector widget and a menu snippet."))

(defun navigation-pane-name-for-token (navigation token)
  "Return the pane name for a given uri-token or NIL if not found. Token
may be NIL in which case the default pane name is provided."
  (cdr (assoc token (navigation-pane-names navigation) :test #'equalp)))

(defgeneric render-navigation-menu (obj &rest args)
  (:documentation "Renders the HTML menu for the navigation widget.")
  (:method ((obj navigation) &rest args)
    (declare (ignore args))
    (render-menu
     (remove nil
	     (mapcar (lambda (pane)
		       (let ((token (car pane)))
			 (unless (member token (navigation-hidden-panes obj)
					 :test #'string-equal)
			   (cons (navigation-pane-name-for-token obj token)
				 (compose-uri-tokens-to-url token)))))
		     (static-selector-panes obj)))
     :base (selector-base-uri obj)
     :selected-pane (static-selector-current-pane obj)
     :header (navigation-header obj)
     :container-id (ensure-dom-id obj)
     :empty-message "No navigation entries")))

(defmethod render-widget-body ((obj navigation) &rest args)
  (apply #'render-navigation-menu obj args)
  (when (navigation-render-content obj)
    (with-html 
      (:div :class "navigation-body"
	    (mapc #'render-widget (widget-children obj))))))

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

;; TODO: rework this, add :header option somewhere
(defun make-navigation (name &rest args)
  "Instantiates the default navigation widget via 'make-instance'
and forwards it along with 'args' to 'init-navigation'.

The navigation widgets bears the title NAME."
  (let ((nav (make-instance 'navigation :name name)))
    (apply #'init-navigation nav args)
    nav))


(export '(teleport teleport-source teleport-key))

(defwidget teleport ()
  ((source :accessor teleport-source
	   :initarg :source
	   :documentation "Source widget that should be teleported and
	   rendered.")
   (key :accessor teleport-key
	:initarg :key
	:initform #'identity
	:documentation "The function that will be used to access the
   widget from the source.")))

(defmethod render-widget-body ((obj teleport) &rest args)
  (declare (ignore args))
  (render-widget (funcall (teleport-key obj) (teleport-source obj))))


