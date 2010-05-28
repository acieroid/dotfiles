(in-package :stumpwm)

;;; TODO: destroy group when last window moved
(defun spawn-group (name &key window
                              (screen (current-screen))
                              (background-p t))
  "Spawn the group named NAME if it doesn't exist yet, and move the
window WINDOW to it, when specified. Move to the this group when
BACKGROUND-P is nil, deleting the current group if there are no more
window in it."
  (let ((old-group (current-group))
        (group (find-group screen name)))
    (if group
        (unless background-p
          (switch-to-group group))
        (setf group (add-group screen name :background background-p)))
    (when window
      (move-window-to-group window group))
    (unless (or background-p (group-windows old-group))
      (kill-group old-group group))))

(defcommand gswitch (name) ((:string "Group name: "))
  "Switch to the group with the name NAME, creating it if it doesn't
exist yet"
  (spawn-group name :background-p nil))

(defcommand gmove-create (to-group) ((:string "To Group: "))
  "Move the current window to the group TO-GROUP, creating it if it
doesn't exist yet"
  (spawn-group to-group :window (current-window)))
