;;;; tenhou-dl.lisp

(in-package :tenhou-dl)

(defun -main ()
  (cond ((/= (length sb-ext:*posix-argv*) 3)
         (format t "Usage: tenhou-dl <Tenhou ID> <Log path>
Example: tenhou-dl ID12345678-6fnB8AoP \"C:\\tenhou\\logs\\\"~%"))
        (t (setf lparallel:*kernel* (lparallel:make-kernel 8))
           (format t "~%Downloaded ~a replay~:p~%"
                   (length (download-replays (second sb-ext:*posix-argv*)
                                             (third sb-ext:*posix-argv*)))))))

(defun download-replays (tenhou-id log-dir)
  "Download all Tenhou replays for games played by user with ID `tenhou-id'
in the last 10 days to `log-dir'.  Returns a list of paths of saved replays.
Skips any replays that already exist in `log-dir'."
  (remove-if #'null
             (lparallel:pmap 'list (lambda (url)  (download-replay url log-dir))
                             (get-replay-urls tenhou-id))))

(defun download-replay (url log-dir)
  "Download Tenhou replay from `url' to `log-dir'.  Skips if replay already in `log-dir'"
  (let* ((full-url (if (search "tenhou.net" url)
                       url
                       (format nil "https://tenhou.net~a" url)))
         (file-name (format nil "~a.mjlog" (subseq url (1+ (position #\= url)))))
         (subdir (subseq file-name 0 6))
         (destination-path (format nil "~a/~a/~a" log-dir subdir file-name)))
    (unless (cl-fad:file-exists-p destination-path)
      (when (trivial-download:download full-url destination-path :quiet t)
        (format t "~a ==>~%~t~t~a~%" full-url destination-path)
        destination-path))))

(defun get-replay-urls (tenhou-id)
  (parse-replay-response (get-replay-response tenhou-id)))

(defun get-replay-response (tenhou-id)
  (drakma:http-request "https://tenhou.net/0/log/find.cgi"
                       :parameters (list (cons "un" tenhou-id))))

(defun parse-replay-response (response)
  (loop for el in (cdr (cl-html-parse:parse-html response))
        when (and (listp el) (equalp (second el) "DOWNLOAD"))
          collect (third (first el))))
