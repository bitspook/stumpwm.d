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

(defvar spook/term-cmd "kitty -o background_opacity=0.7")
(defvar spook/fireword-bin "~/Documents/work/fireword/fireword")

(defcommand fireword (pass len)
    ((:password "Password: ")
     (:password "Length: "))
  (run-shell-command (format nil  "~a ~a ~a | xclip -sel clip" spook/fireword-bin pass len)))

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

(defcommand toggle-float-this () ()
  "Toggle current window to float/unfloat."
  (let ((window (current-window)))
    (if (float-window-p window)
        (unfloat-this)
        (float-this))))

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

(setf *mouse-focus-policy* :click)

(setf *top-level-error-action* :message)

(which-key-mode)

;; Keybindings

;; quick launch
(define-key *root-map* (kbd "f") "firefox")
(define-key *root-map* (kbd "c") (format nil "exec ~a" spook/term-cmd))
(define-key *top-map* (kbd "C-s-l") "exec betterlockscreen -l --off 60")

;; window movement
(define-key *top-map* (kbd "s-k") "move-focus up")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-l") "move-focus right")
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-p") "exec rofi-pass")
(define-key *top-map* (kbd "s-P") "fireword")
(define-key *top-map* (kbd "s-d") "exec rofi -show run")
(define-key *top-map* (kbd "s-D") "exec rofi -show drun")
(define-key *top-map* (kbd "s-S") "exec flameshot gui")

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

(defparameter *spook-toggle-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "f") "toggle-float-this")
    m))
(define-key *root-map* (kbd "t") '*spook-toggle-keymap*)

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
(load-module "ttf-fonts")
;; NixOS specific fix to get the fonts
(setq clx-truetype::*font-dirs* (split-string (getenv "FONT_DIRS") ":"))
(xft:cache-fonts)

(set-font (list (make-instance 'xft:font
                               :family "Noto Sans Mono"
                               :subfamily "Regular"
                               :size 11
                               :antialias t)))

;; get urgent windows
(load-module "urgentwindows")

;; Monitor setup
(defun spook/hdmi-connected-p ()
  (string-trim '(#\Space #\Newline) (run-shell-command "xrandr | grep \"HDMI\" | cut -d \" \" -f 1" t)))

(when *initializing*
  (when (not (spook/hdmi-connected-p))
    (run-shell-command "xrandr --output eDP --mode 1920x1080"))

  (when-let ((hdmi (spook/hdmi-connected-p)))
    (run-shell-command (format nil "xrandr --output ~a --primary --auto --output eDP --off" hdmi) t))

  (loop :for head :in (screen-heads (current-screen))
        :do (enable-mode-line (current-screen) head t)))

;; scratch window
(defun scratch-window ()
  "Find scratch window if one exists."
  (find-if
   (lambda (w) (string= (window-name w) "scratchmacs"))
   (all-windows)))

(defcommand toggle-scratch () ()
  "Toggle showing a scratch window."
  (let* ((win (scratch-window))
         (s-width (screen-width (current-screen)))
         (s-height (screen-height (current-screen)))
         (win-height (floor (/ s-height 2)))
         (win-width (floor (/ s-width 1.8)))
         (win-x (floor (/ s-height 2.4)))
         (win-y (floor (/ s-width 4.4))))
    (when (not win)
      (run-shell-command "emacsclient -c --frame-parameters='(quote (name . \"scratchmacs\"))'")
      (sleep 1)

      (setq win (scratch-window))
      (hide-window win)
      (float-window win (window-group win))
      (float-window-move-resize win :x win-x :y win-y :width win-width :height win-height))

    (if (window-hidden-p win)
        (progn (unhide-window win)
               (focus-window win t))
        (hide-window win))))

(define-key *top-map* (kbd "s-s") "toggle-scratch")

;; group/window placement
(when *initializing*
  (grename "read")
  (gnewbg "edit")
  (gnewbg "term")

  (run-shell-command "firefox")
  (run-shell-command "emacs")
  (run-shell-command spook/term-cmd))

(clear-window-placement-rules)
(define-frame-preference "read" (nil t t :class "Evince"))
(define-frame-preference "read" (nil t t :class "firefox"))
(define-frame-preference "read" (nil t t :class "Chromium"))
(define-frame-preference "edit" (nil t t :class "Emacs" :title-not "scratchmacs"))
(define-frame-preference "term" (nil t t :class "kitty"))
