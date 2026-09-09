;;; kingsbury-prefix.el --- Isolated actual prefix-key schedule -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'timer)
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      ring-bell-function #'ignore)
(defvar kingsbury2-prefix--log (getenv "KINGSBURY_LOG"))
(defun kingsbury2-prefix--emit (message)
  (write-region (concat message "\n") nil kingsbury2-prefix--log t 'silent))
(defvar kingsbury2-prefix--root (file-name-as-directory (getenv "KINGSBURY_ROOT")))
(setq agent-shell-sidebar-refresh-timer nil
      agent-shell-sidebar-extra-project-roots (list kingsbury2-prefix--root)
      projectile-known-projects nil)
(fset 'project-known-project-roots (lambda () nil))
(fset 'agent-shell-buffers (lambda () nil))
(switch-to-buffer "*Kingsbury isolated sidebar*")
(agent-shell-sidebar-mode)
(advice-add 'agent-shell-sidebar--parse-tick :before
            (lambda (&rest _) (kingsbury2-prefix--emit "TICK")))
(add-hook 'post-command-hook
          (lambda () (kingsbury2-prefix--emit (format "POST %S" this-command))))
(run-at-time
 0.25 nil
 (lambda ()
   (with-current-buffer "*Kingsbury isolated sidebar*"
     ;; Arrange a pending continuation whose prior idle age was five seconds.
     ;; The real command loop, native idle age, and actual keystrokes are unmocked.
     (cl-letf (((symbol-function 'current-idle-time) (lambda () (seconds-to-time 5))))
       (agent-shell-sidebar-refresh))
     (kingsbury2-prefix--emit
      (format "ARMED threshold=%.3f delay=%.3f"
              (float-time (timer--time agent-shell-sidebar--parse-timer))
              agent-shell-sidebar-parse-idle-delay)))))
