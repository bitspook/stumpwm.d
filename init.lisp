;; -*-lisp-*-
(in-package :stumpwm)

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload '(:slynk :xkeyboard :cl-ppcre))

(defvar spook/init-directory
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname) ".stumpwm.d"))))

(defcommand firefox () ()
  "Run or raise Firefox."
  (run-or-raise "firefox" '(:class "Firefox") t nil))

(defcommand delete-window-and-frame () ()
  "Delete the current window with its frame."
  (delete-window)
  (remove-split))

(defcommand hsplit-and-focus () ()
  "Create a new frame on the right and focus it."
  (hsplit)
  (move-focus :right))

(defcommand vsplit-and-focus () ()
  "Create a new frame below and focus it."
  (vsplit)
  (move-focus :down))

;; change the prefix key to something else
(set-prefix-key (kbd "s-SPC"))
(redirect-all-output (merge-pathnames "log" spook/init-directory))

(slynk:restart-server :dont-close t)

(set-module-dir
 (pathname-as-directory (concat (format nil "~a" (user-homedir-pathname)) "Documents/vendor/stumpwm-contrib-modules")))

;; visual
(set-normal-gravity :bottom-right)
(setf *message-window-gravity* :center
      *input-window-gravity* :center
      *group-format* "%t"
      *window-format* "%10c (%20t)")

(setf *mouse-focus-policy* :sloppy)

(setf *top-level-error-action* :message)

(when *initializing*
  (mode-line))
;; Keybindings

;; quick launch
(define-key *root-map* (kbd "f") "firefox")
(define-key *root-map* (kbd "c") "exec kitty")
(define-key *top-map* (kbd "C-s-l") "exec betterlockscreen -l --off 60")

;; window movement
(define-key *top-map* (kbd "s-k") "move-focus up")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-l") "move-focus right")
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-P") "exec rofi-pass")
(define-key *top-map* (kbd "s-d") "exec rofi -show run")
(define-key *top-map* (kbd "s-D") "exec rofi -show drun")

(define-key *root-map* (kbd "'") "windowlist")
(define-key *root-map* (kbd "C-k") "delete-window-and-frame")

(defun load-file (filename)
  (load (concat spook/init-directory filename ".lisp")))

(load-file "./colors")
(load-file "./modeline")

;; Modules from stumpwm-contrib
;; bring mouse cursor to current window
(load-module "beckon")
;; gracefully end programs when ending session
(load-module "end-session")

;; navigate b/w windows from all workspaces
(load-module "globalwindows")
(define-key *root-map* (kbd "\"") "global-windowlist")

;; get urgent windows
(load-module "urgentwindows")
