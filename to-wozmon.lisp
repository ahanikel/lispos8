(ql:quickload :uiop)

(defun to-wozmon (file)
  (let ((addr #x0400))
    (uiop:with-input-file (f file :element-type '(unsigned-byte 8))
      (loop
            for b = (read-byte f nil :eof)
            until (eq :eof b)
            do (progn
                 (if (eq 0 (mod addr 8))
                     (format t "~%~4,'0x: ~2,'0x" addr b)
                     (format t " ~2,'0x" b))
                 (incf addr)))
      (terpri))))
