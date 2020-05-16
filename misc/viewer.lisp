(in-package :similar-images-viewer)

(defun prepare-tree-view (tree-view)
  (let* ((renderer (gtk-cell-renderer-text-new))
         (column (gtk-tree-view-column-new-with-attributes
                  "Name" renderer "text" 0)))
    (gtk-tree-view-append-column tree-view column)))

(defun add-entries (tree-view sections)
  (let ((model (gtk-tree-view-model tree-view)))
    (loop
       for section in sections
       for parent = (gtk-tree-store-set
                     model
                     (gtk-tree-store-append model nil)
                     (namestring (first section)))
       do
         (loop for file in section do
              (gtk-tree-store-set
               model
               (gtk-tree-store-append model parent)
               (namestring file))))))

#+nil
(defun set-image (image pixbuf status)
  (let ((widget-width (gtk-widget-get-allocated-width image))
        (image-height (gdk-pixbuf:gdk-pixbuf-get-height pixbuf))
        (image-width (gdk-pixbuf:gdk-pixbuf-get-width pixbuf)))

    (gtk-image-set-from-pixbuf
     image
     (if (<= image-width widget-width)
         (gdk-pixbuf:gdk-pixbuf-copy pixbuf)
         (gdk-pixbuf:gdk-pixbuf-scale-simple
          pixbuf
          widget-width
          (floor (* image-height widget-width) image-width)
          :nearest)))
    (gtk-label-set-text
     status
     (format nil "~dx~d~%" image-width image-height))))

(defun view (similar)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Similar pictures browser"
                                 :default-width 800
                                 :default-heigh 600))
          (paned (make-instance 'gtk-paned
                                :orientation :horizontal))
          (tree-view (make-instance 'gtk-tree-view
                                    :model (make-instance 'gtk-tree-store
                                                          :column-types '("gchararray"))))
          (status (make-instance 'gtk-label))
          (box (make-instance 'gtk-box :orientation :vertical))
          (scrolled-window (make-instance 'gtk-scrolled-window))
          (scrolled-window2 (make-instance 'gtk-scrolled-window))
          (image (make-instance 'gtk-image)))

      (prepare-tree-view tree-view)
      (add-entries tree-view similar)

      (gobject:g-signal-connect
       window "destroy"
       (lambda (widget)
         (declare (ignore widget))
         (leave-gtk-main)))

      #+nil
      (gobject:g-signal-connect
       tree-view1 "cursor-changed"
       (lambda (widget)
         (declare (ignore widget))
         (let ((iter (gtk-tree-selection-get-selected
                      (gtk-tree-view-get-selection tree-view1))))
           (when iter
             (add-entries
              tree-view2
              (nth  
               (first
                (gtk-tree-path-get-indices
                 (gtk-tree-model-get-path
                  (gtk-tree-view-get-model tree-view1)
                  iter)))
               similar))))))

      (gobject:g-signal-connect
       tree-view "cursor-changed"
       (lambda (widget)
         (declare (ignore widget))
         (let ((iter (gtk-tree-selection-get-selected
                      (gtk-tree-view-get-selection tree-view))))
           (if iter
               (let ((filename 
                      (gtk-tree-model-get-value
                       (gtk-tree-view-get-model tree-view)
                       iter 0)))
                 (when (probe-file filename)
                   (gtk-image-set-from-file image filename)
                   (let ((pixbuf (gtk-image-pixbuf image)))
                     (gtk-label-set-text
                      status
                      (format nil "~dx~d"
                              (gdk-pixbuf:gdk-pixbuf-width pixbuf)
                              (gdk-pixbuf:gdk-pixbuf-height pixbuf))))))))))

      (gobject:g-signal-connect
       tree-view "key-press-event"
       (lambda (widget event)
         (declare (ignore widget))
         (if (= (gdk:gdk-event-key-keyval event)
                (gdk:gdk-keyval-from-name "Delete"))
             (let ((iter (gtk-tree-selection-get-selected
                          (gtk-tree-view-get-selection tree-view)))
                   (model (gtk-tree-view-get-model tree-view)))
               (when iter
                 (let ((filename (gtk-tree-model-get-value
                                  model iter 0)))
                   (if (probe-file filename)
                       (delete-file filename))
                   (gtk-tree-store-remove
                    model iter)))))))

      (gtk-container-add scrolled-window tree-view)
      (gtk-container-add scrolled-window2 image)
      (gtk-paned-pack1 paned scrolled-window :resize t)
      (gtk-paned-pack2 paned scrolled-window2 :resize t)
      (gtk-box-pack-start box paned)
      (gtk-box-pack-end box status :expand nil)
      (gtk-container-add window box)
      (gtk-widget-show-all window))))
