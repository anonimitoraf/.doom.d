;;; chatgpt.el --- Integration for phind               -*- lexical-binding: t; -*-
(require 'shell-maker)

(defun ++chatgpt--request (question)
  (let* ((curl (string-join
                 `("curl http://localhost:4444/ask"
                    "-s"
                    "-X POST"
                    "-H 'Content-Type: application/json'"
                    ,(format "--data '%s'" (json-serialize `(:question ,question))))
                 " ")))
    (shell-command-to-string curl)))

(defvar ++chatgpt--config)
(setq ++chatgpt--config
  (make-shell-maker-config
    :name "ChatGPT"
    :execute-command
    (lambda (question _history callback _error-callback)
      (make-thread
        (lambda ()
          (let* ((result (++chatgpt--request question)))
            (funcall callback result nil)))))))

(defun ++chatgpt-ask ()
  (interactive)
  (shell-maker-start ++chatgpt--config))
