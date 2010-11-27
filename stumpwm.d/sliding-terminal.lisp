(in-package :stumpwm)

(defvar *hidden-group* (gnewbg ".sliding-term"))

(defun launch-term (&optional wait-p)
  (run-shell-command "urxvt -name sliding-term")
  (when wait-p
    ;; ensure the terminal is launched before returning
    ;; TODO: do a loop ?
    (sleep 0.5)))

(defun find-term ()
  (find "sliding-term" (screen-windows (current-screen))
        :test (lambda (str w)
                (string= (window-name w) str))))

(defcommand show-term () ()
  ;; Create a new frame for the window
  (dump-group-to-file "/tmp/stumpwm-slide-term-group")
  (vsplit)
  (move-window :down)
  (move-focus :up)
  (let ((term (or (find-term)
                  (progn (launch-term t) (find-term)))))
    ;; Get the window into the current group and focuses it
    (move-window-to-group term (current-group))
    (pull-window term)))

(defcommand hide-term () ()
  (move-window-to-group (find-term) *hidden-group*)
  (restore-from-file "/tmp/stumpwm-slide-term-group"))
