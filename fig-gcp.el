;;; fig-gcp --- Global consciousness project integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)
(require 'request)
(require 'dom)

(defcustom fig//gcp-server "https://global-mind.org"
  "Server URL for GCP project."
  :type '(string)
  :group 'fig)

(defvar fig//gcp-last-response nil)

(defun fig//gcp-get (loc k)
  "Get LOC from GCP, passing the returned HTML to K."
  (setf request-message-level -1)
  (request
    (s-concat fig//gcp-server loc)
    :type "GET"
    :parser (lambda () (libxml-parse-xml-region (point-min) (point-max)))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq fig//gcp-last-response data)
       (funcall k data))))
  t)

(defun fig//gcp-dot (k)
  "Pass the current GCP index (as a number between 0 and 1) to K."
  (fig//gcp-get
   "/gcpdot/gcpindex.php"
   (lambda (d)
     (funcall k (string-to-number (caddar (last (cddr (cadddr d)))))))))

(defun fig//gcp-describe (n)
  "Describe GCP index N."
  (cond
   ((< n 0.05) "Red dot. Significantly large network variance. Suggests broadly shared coherence of thought and emotion.")
   ((< n 0.10) "Orange dot. Strongly increased network variance. May be chance fluctuation.")
   ((< n 0.40) "Yellow dot. Slightly increased network variance. Probably chance fluctuation.")
   ((< n 0.90) "Green dot. Normally random network variance. This is average or expected behavior.")
   ((< n 0.95) "Light blue dot. Small network variance. Probably chance fluctuation.")
   (t "Blue dot. Significantly small network variance. Suggestive of deeply shared, internally motivated group focus.")))

(provide 'fig-gcp)
;;; fig-gcp.el ends here
