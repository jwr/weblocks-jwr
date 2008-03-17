;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-asd
  (:use :cl :asdf)
  (:nicknames :wop)
  (:export #:test #:test-op #:doc #:doc-op #:make-app #:make-app-op))

(in-package :weblocks-asd)

(defsystem weblocks
  :name "weblocks"
  :version "0.0.1"
  :maintainer "Slava Akhmechet"
  :author "Slava Akhmechet"
  :licence "LLGPL"
  :description "A Common Lisp web framework."
  :depends-on (:closer-mop :metatilities :hunchentoot :cl-who :cl-ppcre :cl-json :puri :md5
			   :fare-matcher :cl-cont)
  :components ((:module src
		:components (
		 (:file "weblocks")
		 (:module utils
			  :components ((:file "misc")
				       (:file "runtime-class"))
			  :depends-on ("weblocks"))
		 (:file "page-template"
			:depends-on ("weblocks" utils "application"))
		 (:file "actions"
			:depends-on ("weblocks" utils))
		 (:file "debug-mode"
			:depends-on ("weblocks" "actions"))
		 (:file "request-hooks"
			:depends-on ("weblocks"))
		 (:file "request-handler"
			:depends-on (utils "weblocks" "page-template" "debug-mode"
					   "actions" "request-hooks" "application"
					   "request" store))
		 (:module snippets
			  :components ((:file "suggest")
				       (:file "isearch"
					      :depends-on ("html-utils"))
				       (:file "html-utils"))
			  :depends-on ("weblocks" "request" "server" "actions"))
		 (:module linguistic
			  :components ((:file "grammar"))
			  :depends-on ("weblocks" utils))
		 (:module views
			  :components ((:module view
						:components ((:file "view")
							     (:file "utils"
								    :depends-on ("view"))
							     (:file "compiler"
								    :depends-on ("view"))
							     (:file "scaffold"
								    :depends-on ("view" "utils"))
							     (:file "presentation"
								    :depends-on ("view" "compiler"))))
				       (:module dataview
						:components ((:file "dataview")
							     (:file "scaffold"
								    :depends-on ("dataview")))
						:depends-on (view))
				       (:module formview
						:components ((:file "formview")
							     (:file "helpers")
							     (:file "parser"
								    :depends-on ("formview"))
							     (:file "scaffold"
								    :depends-on ("formview" "parser"))
							     (:file "validation"
								    :depends-on ("formview"))
							     (:file "request-deserialization"
								    :depends-on ("formview" "parser"
											    "validation")))
						:depends-on (view))
				       (:module tableview
						:components ((:file "tableview")
							     (:file "scaffold"
								    :depends-on ("tableview")))
						:depends-on (view dataview))
				       (:module
					types
					:components ((:file "us-states")
						     (:file "boolean")
						     (:file "member"
							    :depends-on (presentations parsers))
						     (:file "password")
						     (:module
						      presentations
						      :components ((:file "choices")
								   (:file "radio"
									  :depends-on ("choices"))
								   (:file "dropdown"
									  :depends-on ("choices"))
								   (:file "textarea")
								   (:file "paragraph")
								   (:file "excerpt")
								   (:file "image")
								   (:file "url")))
						     (:module
						      parsers
						      :components ((:file "common"))))
					:depends-on (view formview dataview)))
			  :depends-on ("weblocks" utils snippets))
		 (:module store
			  :components ((:file "store-api")
				       (:file "store-utils"))
			  :depends-on (weblocks utils))
		 (:module widgets
			  :components ((:module widget
						:components ((:file "widget"
								    :depends-on ("widget-mop"))
							     (:file "widget-mop")))
				       (:file "flash"
					      :depends-on (widget))
				       (:file "dataform"
					      :depends-on (widget))
				       (:module datagrid
					:components ((:file "datagrid"
							    :depends-on ("filter" "sort" "select"
										  "drilldown"
										  "gridview"
										  #-cmu "item-ops-action"))
							     #-cmu (:file "item-ops-action")
							     (:file "filter")
							     (:file "sort"
								    :depends-on ("gridview"))
							     (:file "select"
								    :depends-on ("gridview"))
							     (:file "drilldown"
								    :depends-on ("gridview"))
						             (:file "gridview"))
						:depends-on (widget "flash"))
				       (:module gridedit
					:components ((:file "gridedit"
							    :depends-on (#-cmu "delete-action"))
						     #-cmu (:file "delete-action"))
						:depends-on (datagrid "dataform"))
				       (:file "pagination"
					      :depends-on (widget "flash"))
				       (:file "composite"
					      :depends-on (widget))
				       (:file "navigation"
					      :depends-on ("composite" widget)))
			  :depends-on (snippets views utils "actions" "server" "request"
						"request-hooks" linguistic store))
		 (:module control-flow
			  :components ((:file "call-answer")
				       (:file "dialog"
					      :depends-on ("call-answer"))
				       (:file "workflow"
					      :depends-on ("call-answer")))
			  :depends-on ("weblocks" "widgets" "request-handler" "snippets"))
		 (:file "server"
			:depends-on ("weblocks" utils store))
		 (:file "request"
			:depends-on ("weblocks" "actions"))
		 (:file "application"
			:depends-on ("weblocks"))
		 (:file "default-application"
			:depends-on ("server" "weblocks" utils "request-handler")))))
  :in-order-to ((asdf:test-op (load-op "weblocks-test"))
		(test-op (load-op "weblocks-test"))
		(doc-op (load-op "weblocks-scripts"))
		(make-app-op (load-op "weblocks-scripts"))))

;;; test-op
(defmethod perform ((o asdf:test-op) (c (eql (find-system :weblocks))))
  "A method specializer to run the weblocks test suite through ASDF."
  (funcall (intern (symbol-name :test-weblocks) (find-package :weblocks-test))))

(defmethod operation-done-p ((o asdf:test-op) (c (eql (find-system :weblocks))))
  nil)

;;;; test operation (same functionality as asdf:test-op, but defined for consistency)
(defclass wop::test-op (operation)
  ()
  (:documentation "Allows to specialize built-in ASDF methods to run
   the Weblocks test suite."))

(defmethod perform ((o wop::test-op) (c (eql (find-system :weblocks))))
  "A method specializer to run the weblocks test suite through ASDF."
  (funcall (intern (symbol-name :test-weblocks) (find-package :weblocks-test))))

(defmethod operation-done-p ((o wop::test-op) (c (eql (find-system :weblocks))))
  nil)

;;;; doc-op operation
(defclass doc-op (operation)
  ()
  (:documentation "Allows to specialize built-in ASDF methods to run
   the Weblocks documentation generation."))

(defmethod perform ((o doc-op) (c component))
  "Runs the documentation generating function."
  nil)

(defmethod perform ((o doc-op) (c (eql (find-system :weblocks))))
  "Runs the documentation generating function."
  (funcall (intern (symbol-name :document-weblocks) (find-package :weblocks-scripts))))

(defmethod operation-done-p ((o doc-op) (c (eql (find-system :weblocks))))
  nil)

;;;; make-app-op operation
(defclass make-app-op (operation)
  ()
  (:documentation "Allows to specialize built-in ASDF methods to create
   a new Weblocks app."))

(defmethod perform ((o make-app-op) (c component))
  "Creates a new Weblocks application"
  nil)

(defmethod perform ((o make-app-op) (c (eql (find-system :weblocks))))
  "Creates a new Weblocks application when (wop:make-app 'name \"/path/to/target/\")
   is called."
  (let ((app-name (cadr (member :name (asdf::operation-original-initargs o))))
	(app-target (cadr (member :target (asdf::operation-original-initargs o)))))
    (funcall (intern (symbol-name :make-application) (find-package :weblocks-scripts))
	     app-name app-target)))

(defmethod operation-done-p ((o make-app-op) (c (eql (find-system :weblocks))))
  nil)

;;;; helper functions that hide away the unnecessary arguments to
;;;; (asdf:operate)
(defun test ()
  "Runs the Weblocks test suite together with loading the necessary packages."
  (asdf:operate 'test-op :weblocks))

(defun doc ()
  "Generates Weblocks documentation together with loading the necessary packages."
  (asdf:operate 'doc-op :weblocks))

(defun make-app (name &optional target)
  "Creates a new Weblocks app named <name> into directory <target> 
   based on the new-app-template."
  (asdf:operate 'make-app-op :weblocks :name name :target target))

