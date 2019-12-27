;;;; tenhou-dl.lisp

(in-package :tenhou-dl)

(defvar *lock* (bt:make-lock))

(unless lparallel:*kernel*
  (setf lparallel:*kernel* (lparallel:make-kernel 4)))

(defun -main ()
  (let ((args (get-command-line-args)))
    (cond ((/= (length args) 3)
           (format t "Usage: tenhou-dl <Tenhou ID> <Log path>
Example: tenhou-dl ID12345678-6fnB8AoP \"C:\\tenhou\\logs\\\"~%"))
          (t (setf lparallel:*kernel*
                   (lparallel:make-kernel (cl-cpus:get-number-of-processors)))
             (format t "~%Downloaded ~a replay~:p~%"
                     (length (download-replays (second args)
                                               (third args))))))))

(defun get-command-line-args ()
  #+sbcl sb-ext:*posix-argv*
  #+lispworks system:*line-arguments-list*
  )

(defun download-replays (tenhou-id log-dir)
  "Download all Tenhou replays for games played by user with ID `tenhou-id'
in the last 10 days to `log-dir'.  Returns a list of paths of saved replays.
Skips any replays that already exist in `log-dir'."
  (declare (string tenhou-id)
           ((or pathname string) log-dir))
  (remove-if #'null
             (lparallel:pmapcar (lambda (url) (download-replay url log-dir))
                                (get-replay-urls tenhou-id))))

(defun download-replay (url log-dir)
  "Download Tenhou replay from `url' to `log-dir'.  Skips if replay already in `log-dir'"
  (declare (string url)
           ((or pathname string) log-dir))
  (let* ((full-url (if (search "tenhou.net" url)
                       url
                       (format nil "https://tenhou.net~a" url)))
         (file-name (format nil "~a.mjlog" (subseq url (1+ (position #\= url)))))
         (subdir (subseq file-name 0 6))
         (destination-path (format nil "~a/~a/~a" log-dir subdir file-name)))
    (unless (cl-fad:file-exists-p destination-path)
      (when (trivial-download:download full-url destination-path :quiet t)
        (bt:with-lock-held (*lock*)
          (format t "~a ==>~%~t~t~a~%" full-url destination-path))
        destination-path))))

(defun get-replay-urls (tenhou-id)
  (declare (string tenhou-id))
  (parse-replay-response (get-replay-response tenhou-id)))

(defun get-replay-response (tenhou-id)
  (declare (string tenhou-id))
  (dex:get (format nil "https://tenhou.net/0/log/find.cgi?un=~a" tenhou-id)))

(defun parse-replay-response (response)
  (declare (string response))
  (loop for el in (plump:get-elements-by-tag-name (plump:parse response) "a")
        when (equal (plump:get-attribute el "href") "DOWNLOAD")
          collect (third (first el))))
