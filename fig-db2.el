;;; fig-db2 --- Redis client -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)

(defcustom fig/db2-buffer " *fig-db2*"
  "Name of buffer used to store intermediate Redis connection data."
  :type '(string)
  :group 'fig)

(defconst fig/db2-host "shiro")
(defconst fig/db2-port 6379)

(defvar fig//db2-partial-result nil)
(defvar fig//db2-state nil)
(defvar fig//db2-callback nil)

(defun fig//db2-run-callback (x)
  "Pass X to `fig//db2-callback' and reset the state."
  (let ((cb fig//db2-callback))
    (setf fig//db2-state nil)
    (setf fig//db2-partial-result nil)
    (setf fig//db2-callback nil)
    (when cb (funcall cb x))))

(defun fig//db2-handle-response (msg)
  "Handle the Redis response MSG."
  (let* ((trimmed (s-chop-suffix "\r" (s-chop-suffix "\r\n" msg)))
         (sigil (seq-elt trimmed 0)))
    (fig//write-log (s-concat "Received from Redis: " trimmed))
    (cl-case sigil
      (?+ (fig//db2-run-callback (seq-drop trimmed 1)))
      (?* (setf fig//db2-state (list 'array (string-to-number (seq-drop trimmed 1)))))
      (?$
       (let ((len (string-to-number (seq-drop trimmed 1))))
         (if (= -1 len)
             (fig//db2-run-callback nil)
           (setf fig//db2-state `(string ,len)))))
      (otherwise
       (cl-case (car fig//db2-state)
         (array nil)
         (string
          (setf fig//db2-partial-result (s-concat fig//db2-partial-result trimmed))
          (when (>= (length fig//db2-partial-result) (cadr fig//db2-state))
            (fig//db2-run-callback fig//db2-partial-result)))
         (otherwise (fig//write-log (s-concat "Unknown sigil response in unknown Redis state"))))))))

(defun fig//db2-handle-lines ()
  "Call `fig//db2-handle-response' on every complete line of the current buffer."
  (let ((l (fig//get-complete-line)))
    (when (and l (not (s-blank? l)))
      (fig//db2-handle-response l)
      (fig//db2-handle-lines))))
(defun fig//db2-process-filter (proc data)
  "Process filter for pub/sub bus connection on PROC and DATA."
  (with-current-buffer (get-buffer-create fig/db2-buffer)
    (when (not (marker-position (process-mark proc)))
      (set-marker (process-mark proc) (point-max)))
    (goto-char (process-mark proc))
    (insert data)
    (set-marker (process-mark proc) (point))
    (goto-char (point-min))
    (fig//db2-handle-lines)))

(defun fig/db2-disconnect ()
  "Disconnect from the pub/sub bus."
  (when (process-live-p (get-process "fig-db2"))
    (delete-process "fig-db2")))

(defun fig/db2-connect ()
  "Connect to the pub/sub bus."
  (fig/db2-disconnect)
  (make-network-process
   :name "fig-db2"
   :buffer nil
   :host fig/db2-host
   :service fig/db2-port
   :filter #'fig//db2-process-filter))

(defun fig/db2-encode (x)
  "Encode X for Redis."
  (cond
   ((listp x) (format "*%d\r\n%s\r\n" (length x) (apply #'s-concat (-map #'fig/db2-encode x))))
   ((stringp x) (format "$%d\r\n%s\r\n" (length x) x))))

(defun fig/db2-send (msg)
  "Send MSG to Redis."
  (process-send-string
   "fig-db2"
   msg))

(defun fig/db2-cmd (cmd k)
  "Run CMD in Redis and pass the result to K."
  (setf fig//db2-callback k)
  (fig/db2-send (fig/db2-encode cmd)))

(defun fig/db2-set (key val)
  "Set KEY to VAL in Redis."
  (fig/db2-cmd `("SET" ,key ,val) (lambda (_) nil)))

(defun fig/db2-get (key k)
  "Get KEY from Redis and pass the corresponding value to K."
  (fig/db2-cmd `("GET" ,key) k))

(defun fig/db2-hset (key hkey val &rest vals)
  "Set HKEY in hash KEY to VAL in Redis."
  (fig/db2-cmd `("HSET" ,key ,hkey ,val ,@vals) (lambda (_) nil)))

(defun fig/db2-hget (key hkey k)
  "Get HKEY in hash KEY from Redis and pass the corresponding value to K."
  (fig/db2-cmd `("HGET" ,key ,hkey) k))

(provide 'fig-db2)
;;; fig-db2.el ends here
