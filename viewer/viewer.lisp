(in-package :similar-images-viewer)

(defun prepare-tree-view (tree-view)
  (let* ((renderer (gtk:gtk-cell-renderer-text-new))
         (column (gtk:gtk-tree-view-column-new-with-attributes
                  "Name" renderer "text" 0)))
    (gtk:gtk-tree-view-append-column tree-view column)))

(defun add-entries (tree-view sections)
  (let ((model (gtk:gtk-tree-view-model tree-view)))
    (loop
       for section in sections
       for counter from 1 by 1
       for parent = (gtk:gtk-tree-store-set
                     model
                     (gtk:gtk-tree-store-append model nil)
                     (format nil "Match ~d (~d images)"
                             counter
                             (length section)))
       do
         (loop for file in section do
              (gtk:gtk-tree-store-set
               model
               (gtk:gtk-tree-store-append model parent)
               (namestring file))))))

;; KLUDGE: Using gdk-pixbuf on lisp side results in memory leaks. I do
;; not know who to blame: gdk-pixbuf, cl-cffi-gtk or myself. Basically
;; all operations with pixbufs result in leaks. Only creating images
;; with GTK-IMAGE-NEW-FROM-FILE works fine, but this way you cannot
;; scale an image. Earlier I did GDK-PIXBUF-NEW-FROM-FILE and
;; GDK-PIXBUF-SCALE-SIMPLE for that purpose. Now use a pixbuf loader
;; to create one pixbuf instead of two and reduce leak a bit.
;;
;; Also disable "size_allocate" handler for the image.
(defun load-image (image status filename)
  (let ((loader (gdk-pixbuf:gdk-pixbuf-loader-new))
        (buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (gobject:g-signal-connect
     loader "size-prepared"
     (lambda (loader width height)
       (gtk:gtk-label-set-text
        status
        (format nil "~dx~d" width height))
       (let* ((widget-width  (gtk:gtk-widget-get-allocated-width image))
              (widget-height (gtk:gtk-widget-get-allocated-height image))
              (scale-x (/ width widget-width))
              (scale-y (/ height widget-height))
              (scale (max scale-x scale-y)))
         (when (> scale 1)
           (gdk-pixbuf:gdk-pixbuf-loader-set-size
            loader
            (floor width scale)
            (floor height scale))))))

    (gobject:g-signal-connect
     loader "area-prepared"
     (lambda (loader)
       (let ((pixbuf (gdk-pixbuf:gdk-pixbuf-loader-get-pixbuf loader)))
         (gtk:gtk-image-set-from-pixbuf image pixbuf))))
    (gobject:g-signal-connect
     loader "area-updated"
     (lambda (loader x y width height)
       (declare (ignore loader x y width height))
       (gtk:gtk-widget-queue-draw image)))
    (with-open-file (input filename :element-type '(unsigned-byte 8))
      (loop for bytes = (read-sequence buffer input)
            until (zerop bytes)
            do (gdk-pixbuf-loader-write-no-copy loader buffer bytes)
            finally (gdk-pixbuf:gdk-pixbuf-loader-close loader)))))

(defun get-selected-name (tree-view)
  (let ((iter (gtk:gtk-tree-selection-get-selected
               (gtk:gtk-tree-view-get-selection tree-view)))
        (model (gtk:gtk-tree-view-get-model tree-view)))
    (if (and iter
             (= (gtk:gtk-tree-path-get-depth
                 (gtk:gtk-tree-model-get-path model iter))
                2))
        (values
         (gtk:gtk-tree-model-get-value model iter 0)
         iter))))

(defun expand-or-collapse-at-selection (tree-view action)
  (declare (type (member :expand :collapse) action))
  (let* ((iter (gtk:gtk-tree-selection-get-selected
                (gtk:gtk-tree-view-get-selection tree-view)))
         (model (gtk:gtk-tree-view-get-model tree-view))
         (path (if iter (gtk:gtk-tree-model-get-path model iter))))
    (if (and path (= (gtk:gtk-tree-path-get-depth path) 1))
        (ecase action
          (:expand
           (gtk:gtk-tree-view-expand-row tree-view path nil))
          (:collapse
           (gtk:gtk-tree-view-collapse-row tree-view path))))))

(defun view (similar)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:gtk-window
                                 :type :toplevel
                                 :title "Similar pictures browser"
                                 :default-width 800
                                 :default-heigh 600))
          (paned (make-instance 'gtk:gtk-paned
                                :orientation  :horizontal
                                :position     150))
          (tree-view (make-instance 'gtk:gtk-tree-view
                                    :model (make-instance 'gtk:gtk-tree-store
                                                          :column-types '("gchararray"))))
          (status (make-instance 'gtk:gtk-label))
          (box (make-instance 'gtk:gtk-box :orientation :vertical))
          (scrolled-window (make-instance 'gtk:gtk-scrolled-window))
          (image (make-instance 'gtk:gtk-image)))

      (prepare-tree-view tree-view)
      (add-entries tree-view similar)

      (gobject:g-signal-connect
       window "destroy"
       (lambda (widget)
         (declare (ignore widget))
         (gtk:leave-gtk-main)))

      (gobject:g-signal-connect
       tree-view "cursor-changed"
       (lambda (widget)
         (declare (ignore widget))
         (let ((filename (get-selected-name tree-view)))
           (cond
             ((and filename (probe-file filename))
              (load-image image status filename))
             (t
              (gtk:gtk-label-set-text status "")
              (gtk:gtk-image-clear image))))))

      #+nil
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
                (gtk:gtk-tree-store-remove
                 (gtk:gtk-tree-view-get-model tree-view)
                 iter))))
           ((= (gdk:gdk-event-key-keyval event)
               (gdk:gdk-keyval-from-name "Right"))
            (expand-or-collapse-at-selection
             tree-view :expand))
           ((= (gdk:gdk-event-key-keyval event)
               (gdk:gdk-keyval-from-name "Left"))
            (expand-or-collapse-at-selection
             tree-view :collapse)))))

      (gtk:gtk-container-add scrolled-window tree-view)
      (gtk:gtk-paned-pack1 paned scrolled-window :resize t)
      (gtk:gtk-paned-pack2 paned image :resize t)
      (gtk:gtk-box-pack-start box paned)
      (gtk:gtk-box-pack-end box status :expand nil)
      (gtk:gtk-container-add window box)
      (gtk:gtk-widget-show-all window))))
