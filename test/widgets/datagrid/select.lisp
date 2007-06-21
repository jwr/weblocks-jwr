
(in-package :weblocks-test)

;;; test render-select-bar
(deftest-html render-select-bar-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data-class 'employee)))
	(weblocks::render-select-bar grid)))
  (:p :class "datagrid-select-bar"
      (:strong "Select: ")
      (:a :href "?action=abc123"
	  :onclick "initiateAction(\"abc123\", \"weblocks-session=1%3Atest\"); return false;"
	  "All")
      ", "
      (:a :href "?action=abc124"
	  :onclick "initiateAction(\"abc124\", \"weblocks-session=1%3Atest\"); return false;" "None")))

;;; test datagrid-item-selected-p
(deftest datagrid-item-selected-p-1
    (let ((grid (make-instance 'datagrid
			       :data-class 'employee
			       :selection '(:none . (1 2 3)))))
      (not (null (datagrid-item-selected-p grid 1))))
  t)

(deftest datagrid-item-selected-p-2
    (let ((grid (make-instance 'datagrid
			       :data-class 'employee
			       :selection '(:none . (1 2 3)))))
      (not (null (datagrid-item-selected-p grid 4))))
  nil)

;;; test datagrid-select-item
(deftest datagrid-select-item-1
    (let ((grid (make-instance 'datagrid
			       :data-class 'employee
			       :selection '(:none . ()))))
      (datagrid-select-item grid 2)
      (not (null (datagrid-item-selected-p grid 2))))
  t)

;;; test datagrid-clear-selection
(deftest datagrid-clear-selection-1
    (let ((grid (make-instance 'datagrid
			       :data-class 'employee
			       :selection '(:none . (1 2 3)))))
      (datagrid-clear-selection grid)
      (not (null (datagrid-item-selected-p grid 2))))
  nil)

;;; test datagrid-render-select-body-cell
(deftest-html datagrid-render-select-body-cell-1
    (let ((grid (make-instance 'datagrid
			       :data-class 'employee)))
      (weblocks::datagrid-render-select-body-cell grid *joe* 'name "Joe"))
  (:td :class "select"
       (:input :type "checkbox" :name "item-1")))

(deftest-html datagrid-render-select-body-cell-2
    (let ((grid (make-instance 'datagrid
			       :data-class 'employee
			       :selection '(:none . (1)))))
      (weblocks::datagrid-render-select-body-cell grid *joe* 'name "Joe"))
  (:td :class "select"
       (:input :type "checkbox" :name "item-1" :checked "checked")))

;;; test render-table-header-cell-select
(deftest-html render-table-header-cell-select-1
    (render-table-header-cell *joe* 'weblocks::select nil)
  (:th :class "select" ""))

;;; test selection process
(deftest-html render-widget-body-datagrid-select-1
    (with-request :get nil
      (let ((grid (make-instance 'datagrid
				 :data (list *joe* *bob*)
				 :data-class 'employee
				 :allow-select-p t))
	    (*on-ajax-complete-scripts* nil))
	(declare (special *on-ajax-complete-scripts*))
	;; render datagrid
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; select all
	(do-request `((,weblocks::*action-string* . "abc124")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")
	;; select none
	(do-request `((,weblocks::*action-string* . "abc131")))
	(render-widget-body grid :form-id "I1" :input-id "I2" :search-id "I3")))
  (htm
   ;; initial
   #.(searchbar-template "I1" "I2" "I3" "abc123")
   (:p :class "datagrid-select-bar"
      (:strong "Select: ")
      (:a :href "?action=abc124"
	  :onclick "initiateAction(\"abc124\", \"weblocks-session=1%3Atest\"); return false;"
	  "All")
      ", "
      (:a :href "?action=abc125"
	  :onclick "initiateAction(\"abc125\", \"weblocks-session=1%3Atest\"); return false;" "None"))
   (:form :action "" :method "get" :onsubmit "initiateFormAction(\"abc126\", $(this), \"weblocks-session=1%3Atest\"); return false;"
	  (:fieldset
	   (:div :class "datagrid-body"
		 #.(table-header-template
		    '((:th :class "select" "")
		      (:th :class "name sort-ascending" (:span #.(link-action-template "abc127" "Name")))
		      (:th :class "manager" (:span #.(link-action-template "abc128" "Manager"))))
		    '((:tr
		       (:td :class "select"
			(:input :type "checkbox" :name "item-2"))
		       (:td :class "name" (:span :class "value" "Bob"))
		       (:td :class "manager" (:span :class "value" "Jim")))
		      (:tr :class "altern"
		       (:td :class "select"
			(:input :type "checkbox" :name "item-1"))
		       (:td :class "name" (:span :class "value" "Joe"))
		       (:td :class "manager" (:span :class "value" "Jim"))))
		    :summary "Ordered by name, ascending."))
	   (:input :name "action" :type "hidden" :value "abc126")))
   ;; select all clicked
   #.(searchbar-template "I1" "I2" "I3" "abc129")
   (:p :class "datagrid-select-bar"
      (:strong "Select: ")
      (:a :href "?action=abc130"
	  :onclick "initiateAction(\"abc130\", \"weblocks-session=1%3Atest\"); return false;"
	  "All")
      ", "
      (:a :href "?action=abc131"
	  :onclick "initiateAction(\"abc131\", \"weblocks-session=1%3Atest\"); return false;" "None"))
   (:form :action "" :method "get" :onsubmit "initiateFormAction(\"abc132\", $(this), \"weblocks-session=1%3Atest\"); return false;"
	  (:fieldset
	   (:div :class "datagrid-body"
		 #.(table-header-template
		    '((:th :class "select" "")
		      (:th :class "name sort-ascending" (:span #.(link-action-template "abc133" "Name")))
		      (:th :class "manager" (:span #.(link-action-template "abc134" "Manager"))))
		    '((:tr
		       (:td :class "select"
			(:input :type "checkbox" :name "item-2" :checked "checked"))
		       (:td :class "name" (:span :class "value" "Bob"))
		       (:td :class "manager" (:span :class "value" "Jim")))
		      (:tr :class "altern"
		       (:td :class "select"
			(:input :type "checkbox" :name "item-1" :checked "checked"))
		       (:td :class "name" (:span :class "value" "Joe"))
		       (:td :class "manager" (:span :class "value" "Jim"))))
		    :summary "Ordered by name, ascending."))
	   (:input :name "action" :type "hidden" :value "abc132")))
   ;; select none clicked
   #.(searchbar-template "I1" "I2" "I3" "abc135")
   (:p :class "datagrid-select-bar"
      (:strong "Select: ")
      (:a :href "?action=abc136"
	  :onclick "initiateAction(\"abc136\", \"weblocks-session=1%3Atest\"); return false;"
	  "All")
      ", "
      (:a :href "?action=abc137"
	  :onclick "initiateAction(\"abc137\", \"weblocks-session=1%3Atest\"); return false;" "None"))
   (:form :action "" :method "get" :onsubmit "initiateFormAction(\"abc138\", $(this), \"weblocks-session=1%3Atest\"); return false;"
	  (:fieldset
	   (:div :class "datagrid-body"
		 #.(table-header-template
		    '((:th :class "select" "")
		      (:th :class "name sort-ascending" (:span #.(link-action-template "abc139" "Name")))
		      (:th :class "manager" (:span #.(link-action-template "abc140" "Manager"))))
		    '((:tr
		       (:td :class "select"
			(:input :type "checkbox" :name "item-2"))
		       (:td :class "name" (:span :class "value" "Bob"))
		       (:td :class "manager" (:span :class "value" "Jim")))
		      (:tr :class "altern"
		       (:td :class "select"
			(:input :type "checkbox" :name "item-1"))
		       (:td :class "name" (:span :class "value" "Joe"))
		       (:td :class "manager" (:span :class "value" "Jim"))))
		    :summary "Ordered by name, ascending."))
	   (:input :name "action" :type "hidden" :value "abc138")))))

