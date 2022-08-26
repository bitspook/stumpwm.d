(in-package :stumpwm)

(setf *mode-line-timeout* 2
      *time-modeline-string* "%a,%d %b; %r"
      *mode-line-background-color* spook-nord1
      *mode-line-foreground-color* spook-nord5
      *mode-line-border-width* 0
      *mode-line-border-color* spook-nord1)

(load-module "battery-portable")
(load-module "mem")

(setf mem::*mem-modeline-fmt*        "%a%p"
      *hidden-window-color*          "^**"
      *mode-line-highlight-template* "^[^(:fg \"#1d1f21\")^(:bg \"#88c0d0\") ~A ^]")

(setf *screen-mode-line-format*
      (list
       "[%g] | "
       "%W"
       "^>"
       '(:eval (format nil "^[^(:fg \"~a\")^(:bg \"~a\") %M ^]" spook-nord8 spook-nord1))
       '(:eval (format nil "^[^(:fg \"~a\")^(:bg \"~a\") BAT: %B ^]" spook-nord8 spook-nord1))
       '(:eval (format nil "^[^(:fg \"~a\")^(:bg \"~a\") %d ^]" spook-nord8 spook-nord1))))
