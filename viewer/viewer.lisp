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
       for counter from 1 by 1
       for parent = (gtk-tree-store-set
                     model
                     (gtk-tree-store-append model nil)
                     (format nil "Match ~d (~d images)"
                             counter
                             (length section)))
       do
         (loop for file in section do
              (gtk-tree-store-set
               model
               (gtk-tree-store-append model parent)
               (namestring file))))))

(defun set-image (image pixbuf)
  (if pixbuf
      (let* ((widget-width  (gtk-widget-get-allocated-width image))
             (widget-height (gtk-widget-get-allocated-height image))
             (image-height (gdk-pixbuf:gdk-pixbuf-get-height pixbuf))
             (image-width  (gdk-pixbuf:gdk-pixbuf-get-width pixbuf))
             (scale-x (/ image-width widget-width))
             (scale-y (/ image-height widget-height))
             (scale (max scale-x scale-y)))
        (gtk-image-set-from-pixbuf
         image
         (if (< scale 1)
             (gdk-pixbuf:gdk-pixbuf-copy pixbuf)
             (gdk-pixbuf:gdk-pixbuf-scale-simple
              pixbuf
              (floor image-width scale)
              (floor image-height scale)
              :nearest))))
      (gtk:gtk-image-clear image)))

(defun get-selected-name (tree-view)
  (let ((iter (gtk-tree-selection-get-selected
               (gtk-tree-view-get-selection tree-view)))
        (model (gtk-tree-view-get-model tree-view)))
    (if (and iter
             (= (gtk-tree-path-get-depth
                 (gtk-tree-model-get-path model iter))
                2))
        (values
         (gtk-tree-model-get-value model iter 0)
         iter))))

(defun expand-or-collapse-at-selection (tree-view action)
  (declare (type (member :expand :collapse) action))
  (let* ((iter (gtk-tree-selection-get-selected
                (gtk-tree-view-get-selection tree-view)))
         (model (gtk-tree-view-get-model tree-view))
         (path (if iter (gtk-tree-model-get-path model iter))))
    (if (and path (= (gtk-tree-path-get-depth path) 1))
        (ecase action
          (:expand
           (gtk-tree-view-expand-row tree-view path nil))
          (:collapse
           (gtk-tree-view-collapse-row tree-view path))))))

(defun view (similar)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Similar pictures browser"
                                 :default-width 800
                                 :default-heigh 600))
          (paned (make-instance 'gtk-paned
                                :orientation  :horizontal
                                :position     150))
          (tree-view (make-instance 'gtk-tree-view
                                    :model (make-instance 'gtk-tree-store
                                                          :column-types '("gchararray"))))
          (status (make-instance 'gtk-label))
          (box (make-instance 'gtk-box :orientation :vertical))
          (scrolled-window (make-instance 'gtk-scrolled-window))
          (scrolled-window2 (make-instance 'gtk-scrolled-window))
          (image (make-instance 'gtk-image))
          current-pixbuf)

      (prepare-tree-view tree-view)
      (add-entries tree-view similar)

      (gobject:g-signal-connect
       window "destroy"
       (lambda (widget)
         (declare (ignore widget))
         (leave-gtk-main)))

      (gobject:g-signal-connect
       tree-view "cursor-changed"
       (lambda (widget)
         (declare (ignore widget))
         (let ((filename (get-selected-name tree-view)))
           (cond
             ((and filename (probe-file filename))
              (setq current-pixbuf (gdk-pixbuf:gdk-pixbuf-new-from-file filename))
              (gtk-label-set-text
               status
               (format nil "~dx~d"
                       (gdk-pixbuf:gdk-pixbuf-width  current-pixbuf)
                       (gdk-pixbuf:gdk-pixbuf-height current-pixbuf))))
             (t
              (setq current-pixbuf nil)))
           (set-image image current-pixbuf))))

      (gobject:g-signal-connect
       image "size-allocate"
       (lambda (widget allocation)
         (declare (ignore widget allocation))
         (set-image image current-pixbuf)))

      (gobject:g-signal-connect
       tree-view "key-press-event"
       (lambda (widget event)
         (declare (ignore widget))
         (cond
           ((= (gdk:gdk-event-key-keyval event)
               (gdk:gdk-keyval-from-name "Delete"))
            (multiple-value-bind (filename iter)
                (get-selected-name tree-view)
              (when iter
                (when (probe-file filename)
                  (delete-file filename))
                (gtk-tree-store-remove
                 (gtk-tree-view-get-model tree-view)
                 iter))))
           ((= (gdk:gdk-event-key-keyval event)
               (gdk:gdk-keyval-from-name "Right"))
            (expand-or-collapse-at-selection
             tree-view :expand))
           ((= (gdk:gdk-event-key-keyval event)
               (gdk:gdk-keyval-from-name "Left"))
            (expand-or-collapse-at-selection
             tree-view :collapse)))))

      (gtk-container-add scrolled-window tree-view)
      (gtk-container-add scrolled-window2 image)
      (gtk-paned-pack1 paned scrolled-window :resize t)
      (gtk-paned-pack2 paned scrolled-window2 :resize t)
      (gtk-box-pack-start box paned)
      (gtk-box-pack-end box status :expand nil)
      (gtk-container-add window box)
      (gtk-widget-show-all window))))
