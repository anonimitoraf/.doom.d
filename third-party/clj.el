;;; clj.el --- Clojure-like utilities               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Rafael Nicdao

;; Author: Rafael Nicdao <nicdaoraf@gmail.com>

;;; Commentary:

;;; Code:

(defmacro clj/comment (&rest _))

(defun clj/juxt (&rest fns)
  (lambda (&rest xs)
    (mapcar (lambda (f) (apply f xs)) fns)))

(clj/comment
  (funcall (clj/juxt '+ '-) 10 9) ;; => (19 1)
  (funcall (clj/juxt '+ '-) 1) ;; => (1 -1)

  (let ((g (clj/juxt '(1+ 1-))))
    (funcall g 1))
  )

(provide 'clj)
;;; clj.el ends here
