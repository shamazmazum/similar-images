(in-package :similar-images-viewer-sdl)

(sera:-> list->dlist (list)
         (values dlist:dlist &optional))
(defun list->dlist (list)
  (loop with dlist = nil
        for elt in (reverse list) do
        (dlist:dlist-push elt dlist)
        finally (return dlist)))

(sera:-> remove-this-item! (dlist:dcons)
         (values (or dlist:dcons null) &optional))
(defun remove-this-item! (dcons)
  "Remove DCONS from a chain of conses and return a next cons in a chain"
  (let ((prev (dlist:prev dcons))
        (next (dlist:next dcons)))
    (when prev
      (setf (dlist::dcons-next prev) next))
    (when next
      (setf (dlist::dcons-prev next) prev))
    (or next prev)))

(deftype walker-function ()
  '(sera:-> () (values (or pathname null) &optional)))

(sera:defconstructor image-walker
  (current walker-function)
  (next    walker-function)
  (prev    walker-function)
  (remove  walker-function))

(sera:-> make-image-walker ((cons pathname))
         (values image-walker &optional))
(defun make-image-walker (filenames)
  (let ((dcons (dlist:dlist-first (list->dlist filenames))))
    (labels ((current-data () (dlist:data dcons))
             (next ()
               (let ((next (dlist:next dcons)))
                 (when next (setq dcons next)))
               (current-data))
             (prev ()
               (let ((prev (dlist:prev dcons)))
                 (when prev (setq dcons prev)))
               (current-data))
             (remove-current ()
               (setq dcons (remove-this-item! dcons))
               (current-data)))
      (image-walker #'current-data #'next #'prev #'remove-current))))
