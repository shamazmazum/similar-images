(in-package :similar-images-viewer-sdl)

(defmacro case-scancode (scancode &body clauses)
  `(cond
     ,@(loop for (code . rest) in clauses collect
             `((sdl2:scancode= (sdl2:scancode-value ,scancode) ,code)
               ,@rest))))

(defmacro with-surface ((var init-form) &body body)
  `(let ((,var ,init-form))
     (unwind-protect
          (progn ,@body)
       (tg:cancel-finalization ,var)
       (sdl2:free-surface ,var))))

(defmacro with-surfaces (bindings &body body)
  (reduce
   (lambda (binding acc)
     `(with-surface ,binding ,acc))
   bindings
   :from-end t
   :initial-value `(progn ,@body)))

(defmacro with-texture-from-surface ((var renderer surface) &body body)
  `(let ((,var (sdl2:create-texture-from-surface ,renderer ,surface)))
     (unwind-protect
          (progn ,@body)
       (sdl2:destroy-texture ,var))))

(defmacro with-textures-from-surface (bindings &body body)
  (reduce
   (lambda (binding acc)
     `(with-texture-from-surface ,binding ,acc))
   bindings
   :from-end t
   :initial-value `(progn ,@body)))

(defmacro with-font ((var font-pathname) &body body)
  `(let ((,var (sdl2-ttf:open-font ,font-pathname 24)))
     (unwind-protect
          (progn ,@body)
       (sdl2-ttf:close-font ,var))))

(defmacro with-sdl2-image (flags &body body)
  `(progn
     (sdl2-image:init ',flags)
     (unwind-protect
          (progn ,@body)
       (sdl2-image:quit))))

(defmacro with-sdl2-ttf (&body body)
  `(progn
     (sdl2-ttf:init)
     (unwind-protect
          (progn ,@body)
       (sdl2-ttf:quit))))
