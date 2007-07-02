;;; Code shared accross the entire weblocks framework
(defpackage #:weblocks
  (:use :cl :c2mop :metabang.utilities :moptilities :hunchentoot :cl-who :json)
  (:shadowing-import-from :c2mop #:defgeneric
			  #:standard-generic-function #:defclass #:ensure-generic-function
			  #:standard-class #:defgeneric #:standard-generic-function #:defclass
			  #:ensure-generic-function #:standard-class)
  (:documentation
   "Weblocks is a Common Lisp framework that eases the pain of
web application development. It achieves its goals by
standardizing on various libraries, providing flexible and
extensible generic renderers, and exposing a unique widget-based
approach to maintaining UI state."))

(in-package :weblocks)

(export '(*weblocks-output-stream* *current-navigation-url* with-html
	  reset-sessions str server-type server-version with-javascript))

(defparameter *weblocks-output-stream* nil
  "Output stream for Weblocks framework created for each request
and available to code executed within a request as a special
variable. All html should be rendered to this stream.")

(defparameter *current-navigation-url* nil
  "Always contains a navigation URL at the given point in rendering
  cycle. This is a special variable modified by the navigation
  controls during rendering so that inner controls can determine their
  location in the application hierarchy.")

(defparameter *dirty-widgets* nil
  "Contains a list of dirty widgets at the current point in rendering
  cycle. This is a special variable modified by the actions that
  change state of widgets.")

(defmacro with-html (&body body)
  "A wrapper around cl-who with-html-output macro."
  `(with-html-output (*weblocks-output-stream* nil :indent nil)
     ,@body))

(defmacro with-javascript (source &rest args)
  "Places 'source' between script and CDATA elements. Used to avoid
having to worry about special characters in JavaScript code."
  `(with-html
     (:script :type "text/javascript"
	      (fmt "~%// <![CDATA[~%")
	      (fmt ,source ,@args)
	      (fmt "~%// ]]>~%"))))

(defun server-type ()
  "Hunchentoot")

(defun server-version ()
  hunchentoot::*hunchentoot-version*)

;;; This turns off a regex optimization that eats A LOT of memory
(setq cl-ppcre:*use-bmh-matchers* nil)
