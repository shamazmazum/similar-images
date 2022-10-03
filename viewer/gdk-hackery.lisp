(in-package :similar-images-viewer)

(defun gdk-pixbuf-loader-write-no-copy (loader buffer count)
  (glib:with-g-error (err)
    (cffi:with-pointer-to-vector-data (ptr buffer)
      (gdk-pixbuf::%gdk-pixbuf-loader-write loader ptr count err))))
