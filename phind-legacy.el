;;; phind.el --- Integration for phind               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Rafael Nicdao

;; Author: Rafael Nicdao <nicdaoraf@gmail.com>

;;; Commentary:

;;; Code:

(require 'shell-maker)
(require 'dash)

(defvar phind--user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/112.0")

(defun phind--make-request-answer (query)
  (json-serialize `(:question ,query
                    :codeContext ""
                     :options (:skill "expert"
                                :date "05/05/2023"
                                :language "en-GB"
                                :detailed :false
                                :creative :false
                                ;; :customLinks nil
                                ))))

(defun phind--request-answer (query)
  (let* ((curl (string-join
                 `("curl https://www.phind.com/api/infer/creative"
                    "-X POST"
                    ,(format "-H 'User-Agent: %s'" phind--user-agent)
                    "-H 'Accept: */*'"
                    "-H 'Accept-Language: en-GB,en;q=0.5'"
                    "-H 'Accept-Encoding: gzip, deflate, br'"
                    "-H 'Content-Type: application/json'"
                    "-H 'Origin: https://www.phind.com'"
                    "-H 'Sec-Fetch-Dest: empty'"
                    "-H 'Sec-Fetch-Mode: cors'"
                    "-H 'Sec-Fetch-Site: same-origin'"
                    "-H 'Connection: keep-alive'"
                    "-H 'Pragma: no-cache'"
                    "-H 'Cache-Control: no-cache'"
                    "-H 'TE: trailers'"
                    ,(format "--data-raw %s" (phind--make-request-answer query))
                    "--compressed"
                    )
                 " ")))
    (shell-command-to-string curl)))

(defun phind--stringify-entry (entry)
  "Pretty-prints ENTRY."
  (string-join
    (mapcar (lambda (prop)
              (plist-get entry prop)
              '(:name :snippet :url)))
    "\n"))

(defvar phind--config
  (make-shell-maker-config
    :name "Phind"
    :execute-command
    (lambda (command _history callback _error-callback)
      (let* ((result (phind--request command))
              (entries (->> result
                         (-map (lambda (entry)
                                 (let ((name (gethash "name" entry))
                                        (url (gethash "url" entry))
                                        (snippet (gethash "snippet" entry)))
                                   `(:name ,name
                                      :url ,url
                                      :snippet ,snippet))))
                         (-map #'phind--stringify-entry)
                         (apply #'concat))))
        (funcall callback entries nil)))))

(defun phind-shell ()
  "Start a phind shell that you can query."
  (interactive)
  (shell-maker-start phind--config))

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

(provide 'phind)
;;; phind.el ends here
