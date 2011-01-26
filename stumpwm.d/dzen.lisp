(in-package :stumpwm)

(defparameter *dzen-bg* "#000")
(defparameter *dzen-fg* "#fff")
(defparameter *dzen-height* 15)
(defparameter *dzen-font* "fixed")
(defparameter *dzen-alignment* "l") ;; l for left, r for right, c for center
(defparameter *dzen-separator* " | ")
(defparameter *dzen-format*
  '((:exec "date")))

(defun interpret-dzen-format (fmt)
  (case (first fmt)
    (:group (let ((res (mapcar #'interpret-dzen-format (rest fmt))))
              (format nil "~{~a~}^fg(white)" res)))
    (:exec (let* ((res (run-shell-command (second fmt) t))
                  (newline-pos (position #\Newline res)))
             (if newline-pos
                 (subseq res 0 newline-pos)
                 res)))
    (:color (format nil "^fg(~a)" (second fmt)))
    (:fun (apply (second fmt) (cddr fmt)))
    (:string (second fmt))
    (:icon (format nil "^i(~a) " (second fmt)))
    (:sysctl
     (interpret-dzen-format
      `(:exec ,(format nil "sysctl ~a | cut -d' ' -f2"
                       (second fmt)))))))

(defun get-mode-line-str ()
  (format nil (concat "~{~a~#[~:;" *dzen-separator* "~]~}")
          (mapcar #'interpret-dzen-format *dzen-format*)))

(let ((dzen-process nil))
  (defun launch-dzen2 ()
    (sb-thread:make-thread
     (lambda ()
       (resize-head 0 0 *dzen-height*
                    (head-width (current-head))
                    (- (head-height (current-head)) *dzen-height*))
       (let ((process (sb-ext:run-program "dzen2"
                                          (list "-bg" *dzen-bg* "-fg" *dzen-fg*
                                                "-h" (format nil "~d" *dzen-height*)
                                                "-fn" *dzen-font*
                                                "-ta" *dzen-alignment*)
                                          :input :stream
                                          :wait nil
                                          :search t
                                          :output *standard-output*)))
         (setf dzen-process process)
         ;; TODO: when dzen get killed, stop the loop
         (with-open-stream (input (sb-ext:process-input process))
           (handler-case
               (loop while (sb-ext:process-alive-p process) do
                    (princ (get-mode-line-str) input)
                    (terpri input)
                    (force-output input)
                    (sleep 1))
             (sb-int:simple-stream-error () (setf dzen-process nil))))))))

  (defun kill-dzen2 ()
    (when dzen-process
      ;; Send the TERM signal
      (sb-ext:process-kill dzen-process 15)
      (setf dzen-process nil)
      ;; TODO: do something cleaner ?
      (resize-head 0 0 0 1280 1024)))

  (defun toggle-dzen2 ()
    (if dzen-process
        (kill-dzen2)
        (launch-dzen2)))
  (defun dzen-process ()
    dzen-process))

(defcommand dzen () ()
  "Launch or kill dzen"
  (toggle-dzen2))
