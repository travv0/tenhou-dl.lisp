;;;; tenhou-dl.lisp

(in-package :tenhou-dl)

(defun -main ()
  (if (< (length sb-ext:*posix-argv*) 3)
      (format t "Usage: tenhou-dl <Tenhou ID> <Log path>
Example: tenhou-dl ID12345678-6fnB8AoP \"C:\\tenhou\\logs\\\"~%")
      (format t "~%Downloaded ~a replay~:p~%"
              (count t (download-replays (second sb-ext:*posix-argv*)
                                         (third sb-ext:*posix-argv*))))))

(defun download-replays (tenhou-id log-dir)
  (map 'list (lambda (url) (download-replay url log-dir))
       (get-replay-urls tenhou-id)))

(defun download-replay (url log-dir)
  (let* ((full-url (format nil "https://tenhou.net~a" url))
         (file-name (format nil "~a.mjlog" (subseq url (+ (position #\= url) 1))))
         (subdir (subseq file-name 0 6))
         (destination-path (format nil "~a/~a/~a" log-dir subdir file-name)))
    (unless (cl-fad:file-exists-p destination-path)
      (format t "~a ==>~%~t~t~a~%" full-url destination-path)
      (trivial-download:download full-url destination-path :quiet t))))

(defun get-replay-urls (tenhou-id)
  (parse-replay-response (get-replay-response tenhou-id)))

(defun get-replay-response (tenhou-id)
  (drakma:http-request "https://tenhou.net/0/log/find.cgi"
                       :parameters (list (cons "un" tenhou-id))))

(defun parse-replay-response (response)
  (loop for el in (cdr (cl-html-parse:parse-html response))
        when (and (listp el) (equalp (second el) "DOWNLOAD"))
          collect (third (first el))))
