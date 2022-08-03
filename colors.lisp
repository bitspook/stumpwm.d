(in-package :stumpwm)

(defvar spook-nord0 "#2e3440")
(defvar spook-nord1 "#3b4252")
(defvar spook-nord2 "#434c5e")
(defvar spook-nord3 "#4c566a")
(defvar spook-nord4 "#d8dee9")
(defvar spook-nord5 "#e5e9f0")
(defvar spook-nord6 "#eceff4")
(defvar spook-nord7 "#8fbcbb")
(defvar spook-nord8 "#88c0d0")
(defvar spook-nord9 "#81a1c1")
(defvar spook-nord10 "#5e81ac")
(defvar spook-nord11 "#bf616a")
(defvar spook-nord12 "#d08770")
(defvar spook-nord13 "#ebcb8b")
(defvar spook-nord14 "#a3be8c")
(defvar spook-nord15 "#b48ead")

(setq *colors*
      `(,spook-nord1   ;; 0 black
        ,spook-nord11  ;; 1 red
        ,spook-nord14  ;; 2 green
        ,spook-nord13  ;; 3 yellow
        ,spook-nord10  ;; 4 blue
        ,spook-nord14  ;; 5 magenta
        ,spook-nord8   ;; 6 cyan
        ,spook-nord5)) ;; 7 white

(when *initializing*
  (update-color-map (current-screen)))
