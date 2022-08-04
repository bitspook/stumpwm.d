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
      *message-window-padding*   10
      *message-window-y-padding* 10
      *group-format* "%t"
      *window-format* "%10c (%20t)")

(setf *mouse-focus-policy* :sloppy)

(setf *top-level-error-action* :message)

(when *initializing*
  (mode-line))

(which-key-mode)

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

;; familiar workspace/group navigation
(loop :for i :from 1 :to 9
      :do (define-key *top-map* (kbd (format nil "s-~a" i)) (format nil "gselect ~a" i)))

(define-key *root-map* (kbd "'") "windowlist")
(define-key *root-map* (kbd "C-k") "delete-window-and-frame")

(defun load-file (filename)
  (load (concat spook/init-directory filename ".lisp")))

(load-file "./colors")
(load-file "./modeline")

(defvar *spook-end-session-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") "end-session")
    (define-key m (kbd "l") "logout")
    (define-key m (kbd "s") "suspend-computer")
    (define-key m (kbd "S") "shutdown-computer")
    (define-key m (kbd "r") "loadrc")
    (define-key m (kbd "R") "restart-hard")
    (define-key m (kbd "C-r") "restart-computer")
    m))
(define-key *root-map* (kbd "q") '*spook-end-session-keymap*)


;; Modules from stumpwm-contrib
;; bring mouse cursor to current window
(load-module "beckon")
;; gracefully end programs when ending session
(load-module "end-session")

;; navigate b/w windows from all workspaces
(load-module "globalwindows")
(define-key *root-map* (kbd "\"") "global-windowlist")

;; fonts
(ql:quickload :clx-truetype)
(xft:cache-fonts)
(load-module "ttf-fonts")

(set-font `(,(make-instance 'xft:font :family "DejaVu Sans Mono for Powerline" :subfamily "Book" :size 8.5 :antialias t)))

;; get urgent windows
(load-module "urgentwindows")


(when *initializing*
  (grename "read")
  (gnewbg "edit")
  (gnewbg "term")

  (run-shell-command "firefox")
  (run-shell-command "emacs")
  (run-shell-command "kitty"))

(clear-window-placement-rules)
(define-frame-preference "read" (nil t t :class "Evince"))
(define-frame-preference "read" (nil t t :class "firefox"))
(define-frame-preference "read" (nil t t :class "Chromium"))
(define-frame-preference "edit" (nil t t :class "Emacs"))
(define-frame-preference "term" (nil t t :class "kitty"))
