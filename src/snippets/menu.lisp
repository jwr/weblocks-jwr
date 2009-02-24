
(in-package :weblocks)

(export '(*menu-empty-message* render-menu))

(defparameter *menu-empty-message* "No menu entires."
  "A default message shown by 'render-menu' if no entries are
  available.")

(defun render-menu (options &key selected-pane header (container-id (gen-id)) (base "")
                    ordered-list-p (empty-message *menu-empty-message*)
                    disabled-panes)
  "Renders a menu snippet based on given options and selected option. An
option may be a dotted pair of a label and uri-token to link to, or a
name (which will be converted to a label and a URL via humanize-name and
attributize-name, respectively). The selected-pane will be compared to
an option's URI token via equalp.  If CONTAINER-ID is provided, it is
used as the basis of DOM IDs for the menu and each menu item generated
with `unattributized-name'. If a given pane token is found in
`disabled-panes', it's rendered in the navigation as disabled."
  (declare (special *current-navigation-url*))
  (flet ((render-menu-items (&optional orderedp)
           (loop
              for option in options
              for item-number from 1
              do (progn
                   (unless (consp option)
                     (setf option
                           (cons (humanize-name option)
                                 (attributize-name option))))
                   (unless selected-pane
                     (setf selected-pane (car option)))
                   (let* ((label (car option))
                          (target (cdr option))
			  (pane-selected-p (equalp target (or selected-pane "")))
                          (pane-disabled-p (member target disabled-panes :test #'string-equal))
                          (pane-class (cond
                                        (pane-selected-p "selected-item")
                                        (pane-disabled-p "disabled-item"))))
                     (with-html
                       (:li :id (unattributized-name (format nil "~A-~A" container-id label)
                                                     'menu-item)
                            :class pane-class
                            (:span :class (concatenate 'string
                                                       "item-wrapper"
                                                       (when orderedp
                                                         (format nil " item-number-~A" item-number)))
                                   (etypecase target
                                     (string
                                      (if (or pane-selected-p pane-disabled-p)
                                        (htm (:span :class "label" (str label)))
                                        (htm (:a :href 
						 (concatenate 'string
							      (string-right-trim "/" base)
							      "/"
							      (string-left-trim "/" target))
                                                   (str label)))))
                                     (function
                                      (render-link target label)))))))))))
    (with-html
      (:div :class "view menu"
            :id (unattributized-name container-id 'menu)
            (with-extra-tags
              (when header
                (htm (:h1 (str header))))
              (if (null options)
                  (htm
                   (:div :class "empty-menu" (str empty-message)))
                  (if ordered-list-p
                      (htm (:ol (render-menu-items t)))
                      (htm (:ul (render-menu-items))))))))))
