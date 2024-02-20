;;; fig-wikipedia --- Wikipedia integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'json)
(require 'request)
(require 'dom)

(defcustom fig//wikipedia-query-base "https://en.wikipedia.org/w/api.php?action=query&prop=extracts&format=json&exintro=1&titles="
  "Base URL for Wikipedia query."
  :type '(string)
  :group 'fig)

(defvar fig//wikipedia-last-response nil)

(defun fig//fetch-wikipedia (page k)
  "Retrieve PAGE from Wikipedia.
Pass the resulting article summary to K."
  (let ((pagename (if (s-contains? "://" page) (url-file-nondirectory page) page)))
    (request
      (s-concat fig//wikipedia-query-base (url-encode-url pagename))
      :type "GET"
      :parser #'json-parse-buffer
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)

         (setq fig//wikipedia-last-response data)
         (if-let ((pages (car (ht-values (ht-get (ht-get fig//wikipedia-last-response "query") "pages"))))
                  (ext (ht-get pages "extract"))
                  (dom (with-temp-buffer (insert ext) (libxml-parse-html-region (point-min) (point-max))))
                  )
             (funcall k (s-trim (dom-texts dom)))
           (fig//write-chat-event (format "Could not find Wikipedia page: %s" pagename))))))
    nil))

(defcustom fig/wiki-buffer "*fig-wiki*"
  "Name of buffer used to display wiki."
  :type '(string)
  :group 'fig)

(define-derived-mode fig/wiki-mode special-mode "Wikipedia"
  "Major mode for displaying wiki."
  :group 'fig)

(defun fig//get-wiki-buffer ()
  "Return the wiki buffer."
  (unless (get-buffer fig/wiki-buffer)
    (with-current-buffer (get-buffer-create fig/wiki-buffer)
      (fig/wiki-mode)))
  (get-buffer fig/wiki-buffer))

(defun fig//wikipedia-summary (page)
  "Display a summary of PAGE from Wikipedia."
  (fig//fetch-wikipedia
   page
   (lambda (sum)
     (with-current-buffer (fig//get-wiki-buffer)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (fig//write-line sum)))))
  )

(provide 'fig-wikipedia)
;;; fig-wikipedia.el ends here
