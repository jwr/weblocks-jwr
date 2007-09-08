
(in-package :weblocks)

(export '(*submit-control-name* *cancel-control-name* with-html-form
	  render-link render-button render-checkbox render-dropdown
	  *dropdown-welcome-message*))

(defparameter *submit-control-name* "submit"
  "The name of the control responsible for form submission.")

(defparameter *cancel-control-name* "cancel"
  "The name of the control responsible for cancellation of form
  submission.")

(defmacro with-html-form ((method-type action-code &key id class) &body body)
  "Transforms to cl-who (:form) with standard form code (AJAX support, actions, etc.)"
  `(with-html
     (:form :id ,id :class ,class :action (request-uri-path) :method (attributize-name ,method-type)
	    :onsubmit (format nil "initiateFormAction(\"~A\", $(this), \"~A\"); return false;"
			      (url-encode ,action-code)
			      (session-name-string-pair))
	    (with-extra-tags
	      (htm (:fieldset
		    ,@body
		    (:input :name *action-string* :type "hidden" :value ,action-code)))))))

(defun render-link (action-code name)
  "Renders an action into an href link. The link will be rendered in
such a way that the action will be invoked via AJAX, or will fall back
to regular request if JavaScript is not available. When the user
clicks on the link, the action will be called on the server.

'action-code' - The action created with 'make-action'.
'name' - A string that will be presented to the user in the
link."
  (let ((url (make-action-url action-code)))
    (with-html
      (:a :href url :onclick (format nil "initiateAction(\"~A\", \"~A\"); return false;"
				     action-code (session-name-string-pair))
	  (str name)))))

(defun render-button (name  &key (value (humanize-name name)) id (class "submit"))
  "Renders a button in a form.

'name' - name of the html control. The name is attributized before
being rendered.
'value' - a value on html control. Humanized name is default.
'id' - id of the html control. Default is nil.
'class' - a class used for styling. By default, \"submit\"."
  (with-html
    (:input :name (attributize-name name) :type "submit" :id id :class class
	    :value value :onclick "disableIrrelevantButtons(this);")))

(defun render-checkbox (name checkedp &key id (class "checkbox"))
  "Renders a checkbox in a form.

'name' - name of the html control. The name is attributized before
being rendered.
'checkedp' - if true, renders the box checked.
'id' - id of the html control. Default is nil.
'class' - a class used for styling. By default, \"submit\"."
  (with-html
    (if checkedp
	(htm (:input :name (attributize-name name) :type "checkbox" :id id :class class
		     :value "t" :checked "checked"))
	(htm (:input :name (attributize-name name) :type "checkbox" :id id :class class
		     :value "t")))))

(defparameter *dropdown-welcome-message* "[Select ~A]"
  "A welcome message used by dropdowns as the first entry.")

(defun render-dropdown (name selections &key id class selected-value welcome-name)
  "Renders a dropdown HTML element (select).

'name' - the name of html control. The name is attributized before
being rendered.

'selections' - a list of strings to render as selections. Each element
may be a string or a cons cell. If it is a cons cell, the car of each
cell will be used as the text for each option and the cdr will be used
for the value.

'id' - an id of the element.

'class' - css class of the element.

'selected-value' - a list of strings. Each option will be tested
against this list and if an option is a member it will be marked as
selected. A single string can also be provided.

'welcome-name' - a string used to specify dropdown welcome option (see
*dropdown-welcome-message*). If nil, no welcome message is used. If
'welcome-name' is a cons cell, car will be treated as the welcome name
and cdr will be returned as value in case it's selected."
  (when welcome-name
    (setf welcome-name (car (list->assoc (list welcome-name)
					 :map (lambda (&rest args) nil)))))
  (with-html
    (:select :id id
	     :class class
	     :name (attributize-name name)
	     (mapc (lambda (i)
		     (if (member (car i) (ensure-list selected-value) :test #'string=)
			 (htm (:option :value (cdr i) :selected "selected" (str (car i))))
			 (htm (:option :value (cdr i) (str (car i))))))
		   (list->assoc (append (when welcome-name
					  (list
					   (cons (format nil *dropdown-welcome-message* (car welcome-name))
						 (cdr welcome-name))))
					selections)
				:map (lambda (i) nil))))))

