;;; phind.el --- Integration for phind               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Rafael Nicdao

;; Author: Rafael Nicdao <nicdaoraf@gmail.com>

;;; Commentary:

;;; Code:

(require 'shell-maker)
(require 'dash)

(defvar phind--user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/112.0")

(defun phind--request (query)
  "Construct request using QUERY."
  (let* ((curl (string-join
                 `("curl https://www.phind.com/api/bing/search"
                    "-X POST"
                    ,(format "-H 'User-Agent: %s'" phind--user-agent)
                    "-H 'Accept: */*'"
                    "-H 'Accept-Language: en-GB,en;q=0.5'"
                    "-H 'Accept-Encoding: gzip, deflate, br'"
                    "-H 'Content-Type: application/json'"
                    "-H 'Pragma: no-cache'"
                    "-H 'Cache-Control: no-cache'"
                    ,(format "--data-raw '{\"q\":\"%s\",\"userRankList\":{},\"browserLanguage\":\"en-GB\"}'" query)
                    "--compressed"
                    "--silent")
                 " "))
          (jq "jq -M .processedBingResults.webPages.value")
          (cmd (concat curl " | " jq)))
    (json-parse-string (shell-command-to-string cmd))))

(defun phind--stringify-entry (entry)
  (format "NAME: %s\nSNIPPET: %s\nURL: %s\n\n"
    (plist-get entry :name)
    (plist-get entry :snippet)
    (plist-get entry :url)))

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

(provide 'phind)
;;; phind.el ends here
