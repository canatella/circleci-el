;;; circleci.el -- Integrate Circleci.   -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Damien Merenne <dam@cosinux.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'auth-source)
(require 'subr-x)
(require 'request)

(defcustom circleci-url "https://circleci.com/api/v1.1/"
  "The address of the circleci host."
  :type 'string
  :group 'circleci)

(defcustom circleci-debug nil "Debug the circleci calls." :type 'boolean :group 'circleci)

(defvar circleci-query-data '() "Query parameters as an alist.")

(defun circleci-log (level format-string &rest args)
  "Log message at LEVEL passing FORMAT-STRING and ARGS to `message'."
  (when circleci-debug (apply #'message format-string args)))

(defun circleci-debug (format-string &rest args)
  "Log debug message passing FORMAT-STRING and ARGS to `message'."
  (apply #'circleci-log :debug format-string args))

(defun circleci-info (format-string &rest args)
  "Log info message passing FORMAT-STRING and ARGS to `message'."
  (apply #'circleci-log :info format-string args))

(defun circleci-warn (format-string &rest args)
  "Log warn message passing FORMAT-STRING and ARGS to `message'."
  (apply #'circleci-log :warn format-string args))

(defun circleci-error (format-string &rest args)
  "Log error message passing FORMAT-STRING and ARGS to `message'."
  (apply #'circleci-log :error format-string args))

(defun circleci-credentials ()
  "Return username and password for circleci."
  (let ((found
         (nth 0
              (auth-source-search :max 1
                                  :host (url-host (url-generic-parse-url circleci-url))
                                  ;; secrets.el wouldn’t accept a number.
                                  :port (number-to-string (url-port (url-generic-parse-url circleci-url)))
                                  :require '(:user :secret)
                                  :create t)))
        user secret)
    (when found
      (setq user
            (plist-get found :user)
            secret
            (let ((sec (plist-get found :secret)))
              (if (functionp sec) (funcall sec) sec)))
      (list user secret))))

(defun circleci-authorization-token ()
  "Return authorization token for circleci."
  (cl-destructuring-bind
      (username password)
      (circleci-credentials)
    `("Authorization" . ,(format "Basic %s" (base64-encode-string (concat password ":") t)))))

(defun circleci-url-query-string (data)
  "Convert alist DATA to url query parameters."
  (concat "?"
          (string-join
           (seq-map
            (lambda (item)
              (format "%s=%s"
                      (url-hexify-string (car item))
                      (url-hexify-string (cdr item))))
            data)
           "&")))

(defun circleci-request-url (&rest paths)
  "Build a circleci rest url form PATHS."
  (concat circleci-url (string-join paths "/")))

(defun circleci--parse ()
  "Parse response data."
  (let ((json-array-type 'list)) (json-read)))

(defun circleci-request (method paths &rest settings &allow-other-keys)
  "Send a METHOD rest request to PATHS with request SETTINGS."
  (let ((headers (list (circleci-authorization-token) '("Accept" . "application/json")))
        (circleci-url (apply #'circleci-request-url paths)))
    (apply #'request circleci-url :type method :headers headers :parser #'circleci--parse settings)))


(defun circleci-get (paths &rest settings &allow-other-keys)
  "Execute a get request to PATHS using request SETTINGS."
  (apply #'circleci-request "GET" paths settings))

(defun circleci-request-callback-dispatch (settings data args)
  "Dispatch request to SETTINGS using DATA and ARGS."
  (condition-case nil
      (let* ((success (plist-get settings :success))
             (error (plist-get settings :error))
             (complete (plist-get settings :complete))
             (status-code (plist-get settings :status-code))
             (args (plist-put args :data data))
             (symbol-status (plist-get args :symbol-status))
             (response (plist-get args :response)))
        (setf (request-response-data response) data)
        (let* ((success-p (eq symbol-status 'success))
               (cb (if success-p success error)))
          (when cb (apply cb args)))
        (let ((cb (cdr (assq (request-response-status-code response) status-code))))
          (when cb (apply cb args)))

        (when complete (apply complete args)))
    ((debug error))))

(cl-defun circleci-request-success (settings filter &rest args &allow-other-keys)
  "Run FILTER on successful request and dispatch the result in SETTING callbacks with ARGS and DATA."
  (let ((symbol-status (plist-get args :symbol-status))
        (data (plist-get args :data)))
    (when (eq symbol-status 'success)
      (setq data (funcall (or filter #'identity) data)))
    (circleci-request-callback-dispatch settings data args)))

(defun circleci-last-workflow (vcs username project &optional branch &rest settings &allow-other-keys)
  "Fetch project status for VCS USERNAME and PROJECT possibly limiting to BRANCH."
  (let ((params `(("limit" . "16") ("shallow" . "true")))
        (paths `("project" ,vcs ,username ,project)))
    (when branch (setq paths (append paths `("tree" ,branch))))
    (circleci-get paths :params params
           :complete (apply-partially #'circleci-request-success
                                      settings
                                      (lambda (data)
                                        (let ((workflow-id (json-path '(workflows workflow_id) (elt data 0))))
                                          (seq-filter
                                           (lambda (build)
                                             (string= workflow-id (json-path '(workflows workflow_id) build)))
                                           data)))))))

(provide 'circleci)

;;; circleci.el ends here
