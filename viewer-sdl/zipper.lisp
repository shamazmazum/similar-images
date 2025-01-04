(in-package :similar-images-viewer-sdl)

(sera:defconstructor list-zipper
  (rest list)
  (seen list))

(sera:-> zipper-current (list-zipper)
         (values t &optional))
(defun zipper-current (zipper)
  (or (car (list-zipper-rest zipper))
      ;; If there is no more items left, give the previous item
      (car (list-zipper-seen zipper))))

(sera:-> zipper-next (list-zipper)
         (values list-zipper &optional))
(defun zipper-next (zipper)
  (if (list-zipper-rest zipper)
      (list-zipper (cdr (list-zipper-rest zipper))
                   (cons (car (list-zipper-rest zipper))
                         (list-zipper-seen zipper)))
      zipper))

(sera:-> zipper-prev (list-zipper)
         (values list-zipper &optional))
(defun zipper-prev (zipper)
  (if (list-zipper-seen zipper)
      (list-zipper (cons (car (list-zipper-seen zipper))
                         (list-zipper-rest zipper))
                   (cdr (list-zipper-seen zipper)))
      zipper))


(sera:-> zipper-remove-head (list-zipper)
         (values list-zipper &optional))
(defun zipper-remove-head (zipper)
  (if (list-zipper-rest zipper)
      (list-zipper (cdr (list-zipper-rest zipper))
                   (list-zipper-seen zipper))
      zipper))

(sera:-> list->zipper (list)
         (values list-zipper &optional))
(defun list->zipper (list)
  (list-zipper list nil))

(deftype walker-function ()
  '(sera:-> () (values t &optional)))

(sera:defconstructor image-walker
  (current walker-function)
  (next    walker-function)
  (prev    walker-function)
  (remove  walker-function))

(sera:-> make-image-walker (list)
         (values image-walker &optional))
(defun make-image-walker (filenames)
  (let ((zipper (list->zipper filenames)))
    (labels ((current-data () (zipper-current zipper))
             (next ()
               (setq zipper (zipper-next zipper))
               (zipper-current zipper))
             (prev ()
               (setq zipper (zipper-prev zipper))
               (zipper-current zipper))
             (remove-current ()
               (setq zipper (zipper-remove-head zipper))
               (zipper-current zipper)))
      (image-walker #'current-data #'next #'prev #'remove-current))))
