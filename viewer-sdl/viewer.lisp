(in-package :similar-images-viewer-sdl)

(define-condition font-error (error)
  ((family :initarg :family
           :reader  font-error-family))
  (:documentation "Signalled when no font is found"))

(sera:-> dist-rect
         (unsigned-byte unsigned-byte unsigned-byte unsigned-byte)
         (values sdl2-ffi:sdl-rect &optional))
(defun dist-rect (image-width    image-height
                  renderer-width renderer-height)
  (flet ((rect% (w h)
           (sdl2:make-rect
            (floor (- renderer-width  w) 2)
            (floor (- renderer-height h) 2)
            (floor w)
            (floor h))))
  (let ((scale (max (/ image-width  renderer-width)
                    (/ image-height renderer-height)
                    1)))
    (rect% (/ image-width  scale)
           (/ image-height scale)))))

(sera:-> find-font (string) (values pathname &optional))
(defun find-font (family)
  (let ((font (find family (fd:list-fonts)
                    :key  #'fd:family
                    :test #'string=)))
    (unless font
      (error 'font-error :family family))
    (fd:file font)))

(sera:-> load-image-with-fallback
         (pathname)
         (values sdl2-ffi:sdl-surface &optional))
(defun load-image-with-fallback (pathname)
  (restart-case
      (sdl2-image:load-image (truename pathname))
    (gray-square ()
      :report "Create a red square instead of a picture"
      (let ((surface (sdl2:create-rgb-surface 600 600 32)))
        (sdl2:fill-rect surface nil
                        (sdl2:map-rgb
                         (sdl2:surface-format surface) 100 100 100))
        surface))))

(sera:-> render-image
         (sdl2-ffi:sdl-renderer (or pathname null))
         (values &optional))
(defun render-image (renderer filename)
  (when filename
    (with-font (font (find-font "DejaVu Sans"))
      (with-surfaces ((img-surf (load-image-with-fallback filename))
                      (text-surf (sdl2-ttf:render-utf8-solid
                                  font
                                  (format nil "~a ~dx~d" filename
                                          (sdl2:surface-width  img-surf)
                                          (sdl2:surface-height img-surf))
                                  255 255 255 0)))
        (with-textures-from-surface ((img-text renderer img-surf)
                                     (text-text renderer text-surf))
          (sdl2:render-copy renderer img-text
                            :dest-rect (multiple-value-call #'dist-rect
                                         (sdl2:surface-width  img-surf)
                                         (sdl2:surface-height img-surf)
                                         (sdl2:get-renderer-output-size renderer)))
          (sdl2:render-copy renderer text-text
                            :dest-rect (sdl2:make-rect
                                        0 0
                                        (sdl2:surface-width  text-surf)
                                        (sdl2:surface-height text-surf)))))))
  (values))

(defun handle-conditions (c)
  (declare (ignore c))
  (when (find-restart 'gray-square)
    (invoke-restart 'gray-square))
  (continue))

(sera:-> view-images
         ((cons pathname))
         (values &optional))
(defun view-images (filenames)
  (multiple-value-bind (current next previous remove)
      (sera:deconstruct (make-image-walker filenames))
    (sdl2:with-init (:everything)
      (with-sdl2-image (:jpg :png)
        (with-sdl2-ttf
          (sdl2:with-window (win :title "Viewer" :flags '(:shown))
            (sdl2:with-renderer (renderer win :flags '(:accelerated))
              (flet ((show-image (filename)
                       (handler-bind
                           (((or file-error sdl2-image:sdl-image-error) #'handle-conditions))
                         (sdl2:set-render-draw-color renderer 0 0 0 255)
                         (sdl2:render-clear renderer)
                         (render-image renderer filename)
                         (sdl2:render-present renderer))))
                (sdl2:with-event-loop (:method :wait)
                  (:keydown
                   (:keysym keysym)
                   (case-scancode keysym
                     (:scancode-escape
                      (sdl2:push-event :quit))
                     (:scancode-delete
                      (show-image (funcall remove)))
                     (:scancode-left
                      (show-image (funcall previous)))
                     (:scancode-right
                      (show-image (funcall next)))))
                  (:windowevent
                   (:event event)
                   (when (member event '(6 1 3))
                     (show-image (funcall current))))
                  (:quit () t)))))))))
  (values))
