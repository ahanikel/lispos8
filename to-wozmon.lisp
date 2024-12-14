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

(defun compile-asm (file)
  (let* ((base    (subseq file 0 (position #\. file :from-end t)))
         (infile  (format nil "~a.o" base))
         (outfile (format nil "~a.bin" base)))
    (list
     (uiop:run-program
      `("/opt/homebrew/bin/ca65"
        ,file)
      :output :string :error-output :output)
     (uiop:run-program
      `("/opt/homebrew/bin/ld65"
        "-C" "plain.cfg"
        "-o" ,outfile
        ,infile)
      :output :string
      :error-output :output)
     (uiop:run-program
      `("find" "." "-name" "lisp.*" "-ls")
      :output :string))))

(progn (compile-asm "lisp.s") (to-wozmon "lisp.bin"))
