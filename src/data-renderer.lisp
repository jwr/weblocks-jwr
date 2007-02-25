;;;; Generic data renderer
(in-package :weblocks)

(defmethod render-data-header (obj)
  (format *weblocks-output-stream* "<div class=\"~A\">~%"
	  (attributize-name (object-class-name obj)))
  (format *weblocks-output-stream* "<p><h1>Viewing ~A</h1></p>~%"
	  (humanize-name (object-class-name obj))))

(defmethod render-data-slot-object-inline (obj slot-name (slot-value standard-object))
  (format *weblocks-output-stream* "~%<!-- Rendering ~A -->~%" (attributize-name slot-name))
  (render-data slot-value :inlinep t)
  (format *weblocks-output-stream* "~%"))

(defmethod render-data-slot-object-reference (obj slot-name (slot-value standard-object))
  (let ((obj-name (object-name slot-value)))
    (if (not (null obj-name))
	(render-data-slot obj slot-name obj-name))))

(defmethod render-data-slot (obj slot-name (slot-value standard-object))
  (if (render-slot-inline-p obj slot-name)
      (render-data-slot-object-inline obj slot-name slot-value)
      (render-data-slot-object-reference obj slot-name slot-value)))

(defmethod render-data-slot (obj slot-name slot-value)
  (format *weblocks-output-stream* "<li><h2>~A:</h2>"
	  (humanize-name slot-name))
  (render-data slot-value)
  (format *weblocks-output-stream* "</li>~%"))

(defmethod render-data-pre-slots (obj)
  (format *weblocks-output-stream* "<ul>~%"))

(defmethod render-data-post-slots (obj)
  (format *weblocks-output-stream* "</ul>~%"))

(defmethod render-data-footer (obj)
  (format *weblocks-output-stream* "</div>"))

; slot-names is a list of slots. If hidep is t, only the slots in
; slot-names will be displayed, otherwise the slots in slot-names will
; be hidden and all other slots will be displayed. If observe-order-p
; is true, slots will be displayed in the order they appear in the
; list. This option has effect only with hidep is true. If
; slot-names are an association list, slots will be renamed according
; to the values in the list. This option takes effect only if hidep
; is true.
(defmethod render-data ((obj standard-object) &key
			inlinep slot-names hidep observe-order-p)
  (if (not inlinep) (render-data-header obj))
  (if (not inlinep) (render-data-pre-slots obj))
  (mapc (lambda (slot)
	  (render-data-slot obj (cdr slot) (get-slot-value obj (car slot))))
	(object-visible-slots obj
			      :slot-names slot-names
			      :hidep hidep
			      :observe-order-p observe-order-p))
  (if (not inlinep) (render-data-post-slots obj))
  (if (not inlinep) (render-data-footer obj))
  *weblocks-output-stream*)

(defmethod render-data (obj &key
			inlinep slot-names hidep observe-order-p)
  (format *weblocks-output-stream* "<span>~A</span>" obj)
  *weblocks-output-stream*)
