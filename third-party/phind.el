(require 'webdriver)

(defclass webdriver-service-chrome (webdriver-service)
  ((executable
    :initform "chromedriver"
    :documentation "")
    (port
      :initform 9515
      :documentation ""))
  "")

(setq webdriver-default-service 'webdriver-service-chrome)

(defvar phind--session nil)
(defun phind--make-session () (make-instance 'webdriver-session))

(defun phind--start ()
  (setq phind--session (phind--make-session))
  (webdriver-session-start phind--session)
  (webdriver-goto-url phind--session "https://www.phind.com"))

(defun phind--restart ()
  (condition-case nil
    (webdriver-session-stop phind--session)
    (error nil))
  (phind--start))

(phind--restart)

;;;###autoload
(defun phind-search (query)
  (interactive)
  (let* ((search-box (webdriver-find-element phind--session
                       (make-instance 'webdriver-by
                         :strategy "tag name"
                         :selector "textarea")))
          (search-button (webdriver-find-element phind--session
                           (make-instance 'webdriver-by
                             :strategy "css selector"
                             :selector "button[type='submit']"))))
    (webdriver-element-clear phind--session search-box)
    (webdriver-element-send-keys phind--session search-box query)
    (webdriver-element-click phind--session search-button)))

(phind--restart)
(phind-search "Python sum odds")

(provide 'phind)
