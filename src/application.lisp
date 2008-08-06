
(in-package :weblocks)

(export '(defwebapp start-webapp stop-webapp restart-webapp get-webapp 
	  get-webapps-for-class initialize-webapp finalize-webapp
	  webapp-application-dependencies webapp-name webapp-description webapp-prefix
	  running-webapp make-webapp-uri reset-webapp-session webapp-session-value
	  define-permanent-action define-permanent-action/cc 
	  remove-webapp-permanent-action))

(defvar *registered-webapps* nil
  "A list of applications that the system knows about")

(defvar *webapp-permanent-actions*
  (make-hash-table))

(defclass weblocks-webapp ()
  ((name :accessor weblocks-webapp-name :initarg :name :initform nil :type string)
   (description :accessor weblocks-webapp-description :initarg :description 
		:initform nil :type (or null string)
		:documentation "The name of the application.  This slot will be used 
                   by 'page-title' to generate the default title for each page.")
   (public-files-path :accessor weblocks-webapp-public-app-path :initarg :public-app-path 
		    :initform nil 
		    :documentation "The directory path for public files")
   (prefix :accessor weblocks-webapp-prefix :initarg :prefix :initform "" :type string
	   :documentation "The default dispatch will allow a webapp to be invoked 
              as a subtree of the URI space at this site.  This does not support 
              webapp dispatch on virtual hosts, browser types, etc.")
   (application-dependencies :accessor weblocks-webapp-application-dependencies 
			     :initarg :application-dependencies :initform nil :type list
			     :documentation "The public dependencies for all pages rendered by this 
                                application.  The automatic dependencies system will handle all of 
                                the context or request specific dependencies.")
   (init-user-session :accessor weblocks-webapp-init-user-session :initarg :init-user-session
		      :initform nil :type symbol
		      :documentation "'init-user-session' must be defined by weblocks client in the
                         same package as 'name'. This function will accept a single parameter - a 
                         composite widget at the root of the application. 'init-user-session' is 
                         responsible for adding initial widgets to this composite.")
   (debug :accessor weblocks-webapp-debug :initarg :debug :initform nil))
  (:documentation 
"A class that encapsulates a unique web application and all relevant resources.
A webapp is a unique set of dependencies and information that can be enabled or
disabled independently of others.  Multiple webapps can be active concurrently 
and incoming connections are dispatched to the root of the webapp according to a 
prefix parameter that defines the URLs parsed by that webapp.  The webapp does 
not see the prefix parameter in URLs that are provided to it.  You can, for 
instance, have different sites (e.g. mobile vs. desktop) with vastly different 
layout and dependencies running on the same server."))


(defmacro defwebapp (name &key 
		     subclasses 
		     slots
		     description 
		     (prefix (concatenate 'string "/" (attributize-name name)))
		     ignore-default-dependencies
		     dependencies 
		     public-app-path
		     (init-user-session (find-symbol (symbol-name '#:init-user-session)
						     (symbol-package name)))
		     (autostart t))
  "This macro defines the key parameters for a stand alone web application.  
It defines both a class with name 'name' and registers an instance of that class.
It also instantiates a defvar with an instance of this class.  This is intended
to be the primary way a web application is defined.

:subclasses - if you want to inherit subclass behvior from other webapps, you
can.  It's not likely to be needed much

:slots - webapps are class so slots are a list of definitions just as in defclass,
but as slots are likely to be rare on webapps, we make this a keyword argument.

:name - instantiates a username (and the default title for) a webapp.  use this
name to get and delete this webapp.  Multiple instances of a webapp class can
co-exist, so long as they have different prefixes

:description - A description of the application for the title page

:ignore-default-dependencies inhibits appending the default dependencies to
the dependencies list.  By default 'defwebapp' adds the following resources:

  Stylesheets: layout.css, main.css
  Scripts: prototype.js, weblocks.js, scriptaculous.js

:dependencies - is a list of dependencies to append to the default dependencies
list of the application.

:public-app-path - The pathname for the public files for this application.
The URL will be relative to the object prefix.  So a path of /home/user/webapp/my-pub 
would result in a URL http://mysite.com/mypub-prefix/pub/scripts/wow.js to map to
/home/user/webapp/my-pub/scripts/wow.js.  Alternatively you can choose to accept
the global default by assigning nil or :system-default to this slot.  The default
initform for this argument assumes that a /pub directory exists relative to
the system which has a name matching the current package.  This may be a bogus
assumption, so best to assign this parameter explicitly.

:init-user-session - This is a symbol that is used to find a function to initialize
new user sessions.

:autostart - Whether this webapp is started automatically when start-weblocks is
called (primarily for backward compatibility"
  `(progn
     (defclass ,name ,(append subclasses (list 'weblocks-webapp))
       ,slots
       (:default-initargs 
	:name (attributize-name ',name)
	 :description ,description 
	 :public-app-path (let ((path ,public-app-path))
			    (if (or (null path) (eq path :system-default))
				nil
				(compute-public-files-path 
				 (intern (package-name ,*package*) :keyword))))
	 :init-user-session ,init-user-session
	 :prefix ,prefix
	 :application-dependencies 
	 (append ,(when (not ignore-default-dependencies)
			`(build-local-dependencies
			 '((:stylesheet "layout")
			   (:stylesheet "main")
			   (:stylesheet "dialog")
			   (:script "prototype")
			   (:script "scriptaculous")
			   (:script "shortcut")
			   (:script "weblocks")
			   (:script "dialog"))))
		 (build-local-dependencies ,dependencies))))
     (pushnew ',name *registered-webapps*)
     (when ,autostart
       (pushnew ',name *autostarting-webapps*))
     t))

(defun get-webapp (name &optional (error-p t))
  "Get a running web application"
  (let ((app (find (attributize-name name) *active-webapps* 
		   :key #'weblocks-webapp-name :test #'equal)))
    (if app app
	(when error-p
	  (error "Argument ~a is not a running weblocks application." name)))))

(defun get-webapps-for-class (name)
  (let ((class (or (find-class name nil) (find-class (intern name :keyword) nil))))
    (when class
      (loop for app in *active-webapps* 
	 when (eq (class-of app) class)
	 collect app))))


(defun check-webapp (name)
  "Ensure that the we have a valid webapp class"
  (unless (find-class name nil)
    (error "~a is not a valid weblocks application class." name)))

(defun start-webapp (class &rest initargs &key name &allow-other-keys)
  "Starts the web application"
  (check-webapp class)
  (unless name 
    (setq name (attributize-name class)))
  (let ((app (get-webapp name nil)))
    (when app
      (warn "An instance of ~A with name ~A is already running, ignoring start request"
	    class name)
      (return-from start-webapp))
    (setq app (apply #'make-instance class 
		     (append (list :name name)
			     (remove-keyword-parameter initargs :name))))
    (initialize-webapp app)
    (enable-webapp app)
    app))

(defun enable-webapp (app)
  "Make sure the app with the \"\" prefix is always the last one and that there
   is only one!"
  (setf *active-webapps*
	(sort (pushnew app *active-webapps*)
	      #'string>
	      :key #'weblocks-webapp-prefix))
  (when (> (count "" (mapcar #'weblocks-webapp-prefix *active-webapps*) :test #'equal) 1)
    (error "Cannot have two defaults dispatchers with prefix \"\"")))

(defgeneric initialize-webapp (app)
  (:documentation "A protocol for performing any special initialization on the creation of a webapp object.")
  (:method ((app t)) nil))

(defmethod initialize-webapp :before ((app weblocks-webapp))
  "Ensure that all registered stores are open"
  (unless weblocks::*weblocks-server*
    (start-weblocks))
  (open-stores))

(defmethod initialize-webapp :after ((app weblocks-webapp))
  "Setup per-webapp debugging support for toolbar rendering"
  (flet ((compute-debug-dependencies ()
	   (append (list (make-local-dependency :script "weblocks-debug"))
		   (dependencies "debug-toolbar"))))
    (cond ((weblocks-webapp-debug app)
	   (enable-global-debugging)
	   (setf (weblocks-webapp-application-dependencies app)
		 (compact-dependencies 
		  (append 
		   (weblocks-webapp-application-dependencies app)
		   (compute-debug-dependencies)))))
	  (t
	   (let ((debug-deps (compute-debug-dependencies)))
	     (setf (weblocks-webapp-application-dependencies app)
		   (remove-if (lambda (dep)
				(member dep debug-deps :test #'dependencies-equalp))
			      (weblocks-webapp-application-dependencies app))))))))

(defun stop-webapp (name)
  "Stops the web application"
  (let ((app (find-app name)))
    (setf *active-webapps* (delete app *active-webapps*))
    (finalize-webapp app)))

(defgeneric finalize-webapp (app)
  (:documentation "Called when the app has been pulled off the running list to perform any 
   webapp specific cleanup")
  (:method ((app t)) nil))

(defmethod finalize-webapp :after ((app weblocks-webapp))
  "When all webapps are shut down, close any open stores and stop the weblocks server"
  (when (null *active-webapps*)
    (close-stores)
    (stop-weblocks)))

(defun find-app (name)
  (let ((app (get-webapp name nil))
	(apps (get-webapps-for-class name)))
    (when (not app)
      (if (eq (length apps) 1)
	  (setf app (first apps))
	  (error "App name ~A not found or no instances of class ~A found" name name)))
    app))

(defun restart-webapp (name)
  (let* ((app (find-app name))
	 (class (class-of app))
	 (name (weblocks-webapp-name app)))
    (stop-webapp name)
    (start-webapp class :name name)))

;;
;; These procedures are relative to the current request's selected webapp
;;

(defun current-webapp ()
  "Returns the currently invoked instance of a web application."
  (declare (special *current-webapp*))
  *current-webapp*)

(defun reset-webapp-session (&optional (app (current-webapp)))
  "Reset sessions on a per-webapp basis"
  (setf (webapp-session-value app) nil))

(defun webapp-application-dependencies (&optional (app (current-webapp)))
  "Returns a list of dependencies on scripts and/or stylesheets that
   will persist throughout the whole application. See documentation for
   'widget-application-dependencies' for more details."
  (weblocks-webapp-application-dependencies app))

(defun webapp-name (&optional (app (current-webapp)))
  "Returns the name of the web application (also see 'defwebapp'). Please
   note, this name will be used for the composition of the page title
   displayed to the user. See 'page-title' for details."
  (weblocks-webapp-name app))

(defun webapp-description (&optional (app (current-webapp)))
  "Returns the description of the web application. Please note, this
   description will be used for the composition of the page title
   displayed to the user. See 'page-title' for details."
  (weblocks-webapp-description app))

(defun webapp-prefix (&optional (app (current-webapp)))
  "Returns the URL prefix of the application."
  (weblocks-webapp-prefix app))

(defun webapp-init-user-session (&optional (app (current-webapp)))
  "Returns the init function for the user session."
  (symbol-function (weblocks-webapp-init-user-session app)))

(defun make-webapp-uri (uri &optional (app (current-webapp)))
  (concatenate 'string (webapp-prefix app) uri))

(defun webapp-session-value (symbol &optional (session *session*))
  "Get a session value from the currently running webapp"
  (declare (special *current-webapp*))
  (let ((webapp-session (session-value *current-webapp* session)))
    (when webapp-session
      (gethash symbol webapp-session))))

(defun (setf webapp-session-value) (value symbol)
  "Set a session value for the currently runnin webapp"
  (declare (special *current-webapp*))
  (let ((webapp-session (session-value *current-webapp*)))
    (unless webapp-session
      (setf webapp-session (make-hash-table :test 'equal)
	    (session-value *current-webapp*) webapp-session))
    (setf (gethash symbol webapp-session) value)))


;;
;; Permanent actions
;;

;; NOTES: Should lock-protect this table since users may add actions at runtime

(defun webapp-permanent-action (action)
  "Returns the action function associated with this symbol in the current webapp"
  (declare (special *current-webapp*))
  (when *current-webapp*
    (let ((action-table (webapp-permanent-actions *current-webapp*)))
      (when action-table
	(gethash (if (symbolp action) (symbol-name action) action)
		 action-table)))))

(defun webapp-permanent-actions (webapp)
  (gethash (if (symbolp webapp) webapp
	       (type-of webapp))
	   *webapp-permanent-actions*))

(defun add-webapp-permanent-action (webapp-name action-name function-or-name)
  "Remove an action from a webapp.  action-name should be a string, or it
   will be converted to one (to work with the macro).  function-or-name is
   a symbol or a function object (valid object for funcall)"
  (assert (symbolp webapp-name))
  (assert (or (symbolp action-name) (stringp action-name)))
  (macrolet ((action-table (appname)
	       `(gethash ,appname *webapp-permanent-actions*)))
    (unless (action-table webapp-name)
      (setf (action-table webapp-name) (make-hash-table :test 'equal)))
    (setf (gethash (if (symbolp action-name)
		       (string-downcase (symbol-name action-name))
		       (string-downcase action-name))
		   (action-table webapp-name)) 
	  function-or-name)))
      
(defun remove-webapp-permanent-action (webapp-name action-name)
  "Remove a permanent action from a webapp"
  (macrolet ((action-table (appname)
	       `(gethash ,appname *webapp-permanent-actions*)))
    (let ((table (action-table webapp-name)))
      (when table
	(remhash action-name table)))))

(defmacro define-permanent-action (name webapp-class action-params &body body)
  "Adds a permanent action to the class's set of permanent actions"
  (assert (and (find-class webapp-class)
	       (subtypep webapp-class 'weblocks-webapp)))
  `(add-webapp-permanent-action ',webapp-class ',name
				(lambda ,action-params
				  ,@body)))

(defmacro define-permanent-action/cc (name webapp-class action-params &body body)
  "Adds a permanent action to the class's set of permanent actions"
  (assert (and (find-class webapp-class)
	       (subtypep webapp-class 'weblocks-webapp)))
  `(add-webapp-permanent-action ',webapp-class ',name
				(lambda/cc ,action-params
				  ,@body)))
