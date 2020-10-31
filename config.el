(use-package! exwm
  :config
  ;; necessary to configure exwm manually
  (require 'exwm)
  (require 'exwm-config)
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "HDMI-0"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command "xrandr" nil "xrandr --output HDMI-0 --mode 1920x1080 --pos 0x0 --rotate normal")))
  (exwm-randr-enable)
  ;; fringe size, most people prefer 1
  (fringe-mode 3)
  ;; emacs as a daemon, use "emacsclient <filename>" to seamlessly edit files from the terminal directly in the exwm instance
  (server-start)
  ;; this fixes issues with ido mode, if you use helm, get rid of it
  ;;(exwm-config-ido)
  ;; a number between 1 and 9, exwm creates workspaces dynamically so I like starting out with 1
  (setq exwm-workspace-number 1)
  ;; this is a way to declare truly global/always working keybindings
  ;; this is a nifty way to go back from char mode to line mode without using the mouse
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  (exwm-input-set-key (kbd "s-k") #'exwm-workspace-delete)
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-swap)
  (exwm-input-set-key (kbd "\\") #'doom/leader)
  ;; the next loop will bind s-<number> to switch to the corresponding workspace
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
  ;; the simplest launcher, I keep it in only if dmenu eventually stopped working or something
  (exwm-input-set-key (kbd "s-&")
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))
  ;; an easy way to make keybindings work *only* in line mode
  (push ?\C-q exwm-input-prefix-keys)
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)
  ;; simulation keys are keys that exwm will send to the exwm buffer upon inputting a key combination
  (exwm-input-set-simulation-keys
   '(
     ;; movement
     ([?\C-b] . left)
     ([?\M-b] . C-left)
     ([?\C-f] . right)
     ([?\M-f] . C-right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))
     ;; cut/paste
     ([?\C-w] . ?\C-x)
     ([?\M-w] . ?\C-c)
     ([?\C-y] . ?\C-v)
     ;; search
     ([?\C-s] . ?\C-f)))
  ;; this little bit will make sure that XF86 keys work in exwm buffers as well
  (dolist (k '(XF86AudioLowerVolume
               XF86AudioRaiseVolume
               XF86PowerOff
               XF86AudioMute
               XF86AudioPlay
               XF86AudioStop
               XF86AudioPrev
               XF86AudioNext
               XF86ScreenSaver
               XF68Back
               XF86Forward
               Scroll_Lock
               print))
    (cl-pushnew k exwm-input-prefix-keys))
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; This just enables exwm, it started automatically once everything is ready
  (exwm-enable))

(use-package! dmenu
  :bind
    ("s-SPC" . 'dmenu))

(defun exwm-run-async (name)
  (interactive)
  (start-process name nil name))

(defun trigex/launch-firefox ()
  (interactive)
  (exwm-run-async "firefox"))

(defun trigex/launch-ahoviewer ()
  (interactive)
  (exwm-run-async "ahoviewer"))

(defun trigex/lock-screen ()
  (interactive)
  (exwm-async-run "i3lock-fancy"))

(defun trigex/shutdown ()
  (interactive)
  (start-process "halt" nil "sudo" "halt"))

(global-set-key (kbd "s-f") 'trigex/launch-firefox)
(global-set-key (kbd "<print>") 'trigex/store-fullscreen-screenshot)
(global-set-key (kbd "<M-print>") 'trigex/upload-fullscreen-screenshot)
(global-set-key (kbd "<s-print>") 'trigex/store-region-screenshot)
(global-set-key (kbd "<C-print>") 'trigex/upload-region-screenshot)
;; Screenshot keybinds

(defconst volumeModifier "4")

(defun audio/mute ()
  (interactive)
  (start-process "audio-mute" nil "pulsemixer" "--toggle-mute"))

(defun audio/raise-volume ()
  (interactive)
  (start-process "raise-volume" nil "pulsemixer" "--change-volume" (concat "+" volumeModifier)))

(defun audio/lower-volume ()
  (interactive)
  (start-process "lower-volume" nil "pulsemixer" "--change-volume" (concat "-" volumeModifier)))

(global-set-key (kbd "<XF86AudioMute>") 'audio/mute)
(global-set-key (kbd "<XF86AudioRaiseVolume>") 'audio/raise-volume)
(global-set-key (kbd "<XF86AudioLowerVolume>") 'audio/lower-volume)

(defun trigex/take-screenshot (type handler)
  "Takes a screenshot using scapmgr"
  (interactive "sType: \nsHandler: ")
  (when window-system
    (message "Cheese!")
    (sit-for 1)
    (shell-command (concat "scapmgr " type " " handler))
    (message "Screenshot taken!")))

(defun trigex/upload-fullscreen-screenshot ()
  (interactive)
  (trigex/take-screenshot "-f" "-u"))

(defun trigex/store-fullscreen-screenshot ()
  (interactive)
  (trigex/take-screenshot "-f" "-s"))

(defun trigex/upload-region-screenshot ()
  (interactive)
  (trigex/take-screenshot "-r" "-u"))

(defun trigex/store-region-screenshot ()
  (interactive)
  (trigex/take-screenshot "-r" "-s"))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(setq user-full-name "Trigex"
      user-mail-address "trigex@waifu.club")

(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 14))
(setq line-spacing 1.2)

(global-prettify-symbols-mode t)

(setq doom-theme 'doom-nord)

(setq doom-modeline-buffer-file-name-style 'relative-from-project)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-unicode-fallback t)
(display-time-mode 1)
(setq display-time-format "%I:%M %p")

(use-package! symon
  :config
  (symon-mode)
  :bind
  ("s-h" . symon-mode))

(setq fancy-splash-image "~/.doom.d/cirno-dash.png")
(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        trigex/dashboard-widget-loaded))

(defun trigex/dashboard-widget-loaded ()
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (trigex/display-benchmark-h 'return))
    'face 'doom-dashboard-loaded)
   "\n"))

(defun trigex/display-benchmark-h (&optional return-p)
  "Display a benchmark including number of packages and modules loaded.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "%d frogs across %d lakes unfrozen in %.03fs"
           (- (length load-path) (length doom--initial-load-path))
           (if doom-modules (hash-table-count doom-modules) 0)
           (or doom-init-time
               (setq doom-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))

(setq display-line-numbers-type 'relative)

(use-package! beacon
  :diminish beacon-mode
  :init
  (beacon-mode 1))

(setq doom-leader-key "\\")

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun close-and-kill-next-pane ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))
;; "Control-close" kill other
(global-set-key (kbd "C-c k") 'close-and-kill-next-pane)

(defun close-next-pane ()
  (interactive)
  (other-window 1)
  (delete-window))
;; "Control-close" other
(global-set-key (kbd "C-c o") 'close-next-pane)

(global-set-key (kbd "s-C-l") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-h") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-j") 'shrink-window)
(global-set-key (kbd "s-C-k") 'enlarge-window)

(global-set-key (kbd "C-x w 1") '+workspace/switch-to-0)
(global-set-key (kbd "C-x w 2") '+workspace/switch-to-1)
(global-set-key (kbd "C-x w 3") '+workspace/switch-to-2)
(global-set-key (kbd "C-x w 4") '+workspace/switch-to-3)
(global-set-key (kbd "C-x w 5") '+workspace/switch-to-4)

(shell-command "setxkbmap -option ctrl:swapcaps")

(setq org-directory "~/Documents/org")

(use-package! org-bullets
  :init
  (add-hook 'org-mode-hook (org-bullets-mode 1)))

(defalias 'open 'find-file-other-window)
(defalias 'clean 'eshell/clear-scrollback)
(defalias 'suopen 'eshell/sudo-open)

(defun eshell/sudo-open (filename)
  "Open a file as root in Eshell."
  (let ((qual-filename (if (string-match "^/" filename)
                           filename
                         (concat (expand-file-name (eshell/pwd)) "/" filename))))
    (switch-to-buffer
     (find-file-noselect
      (concat "/sudo::" qual-filename)))))

(defun eshell-other-window ()
  "Create or visit an eshell buffer."
  (interactive)
  (if (not (get-buffer "*eshell*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (eshell))
    (switch-to-buffer-other-window "*eshell*")))

(global-set-key (kbd "<s-return>") 'eshell-other-window)

(defvar my-term-shell "/usr/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(after! circe
  ;; Servers
  (set-irc-server! "clan-world"
                   `(:tls t
                     :port 5597
                     :host "znc.termer.net"
                     :server-buffer-name "clan-world"
                     :nick "trigex"
                     :pass ,(+pass-get-secret "IRC/ZNC/clan-world")))
  (set-irc-server! "Rizon"
                   `(:tls t
                     :port 5597
                     :host "znc.termer.net"
                     :server-buffer-name "Rizon"
                     :nick "trigex"
                     :pass ,(+pass-get-secret "IRC/ZNC/Rizon")))
  (set-irc-server! "Freenode"
                   `(:tls t
                     :port 5597
                     :host "znc.termer.net"
                     :server-buffer-name "Freenode"
                     :nick "trigex"
                     :pass ,(+pass-get-secret "IRC/ZNC/Freenode")))
  (circe-lagmon-mode))

;; Defer notifications for 2 minutes
(setq +irc-defer-notification 120)
(setq +irc-notifications-watch-strings
      '("HeXa" "LeJustice" "termer" "HelloMrEdwards" "FM" "tas" "tas_" "sage" "sage8" "Sentaku-san" "zenhead" "tusoud" "starman" "blakkheim"))

(setq eradio-channels '(("HardcorePower" . "https://panel.beheerstream.com:2199/tunein/hardcorep.pls")))
(setq eradio-player '("mpv" "--no-video" "--no-terminal"))

(use-package! emms
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all) ; don't change this to values you see on stackoverflow questions if you expect emms to work
  (setq emms-seek-seconds 5)
  (setq emms-player-list '(emms-player-mpd))
  (setq emms-info-functions '(emms-info-mpd))
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6601")
  :bind
  ("s-m p" . emms)
  ("s-m b" . emms-smart-browse)
  ("s-m r" . emms-player-mpd-update-all-reset-cache)
  ("<XF86AudioPrev>" . emms-previous)
  ("<XF86AudioNext>" . emms-next)
  ("<XF86AudioPlay>" . emms-pause)
  ("<XF86AudioStop>" . emms-stop))

(setq mpc-host "localhost:6601")
(defun mpd/start-music-daemon ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "mpd")
  (mpd/update-database)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD Started!"))
(global-set-key (kbd "s-m c") 'mpd/start-music-daemon)

(defun mpd/kill-music-daemon ()
  "Stops playback and kill the music daemon."
  (interactive)
  (emms-stop)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD Killed!"))
(global-set-key (kbd "s-m k") 'mpd/kill-music-daemon)

(defun mpd/update-database ()
  "Updates the MPD database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD Database Updated!"))
(global-set-key (kbd "s-m u") 'mpd/update-database)

(defun emms/open ()
  "Creates a new workspace for EMMS and opens it's views"
  (interactive)
  ;; start mpd if it's not open
  (if (not (get-process "mpd")) (mpd/start-music-daemon))
  ;; create it's workspace
  (+workspace/new "EMMS" nil)
  ;; create it's layout
  (emms)
  (split-and-follow-vertically)
  (emms-smart-browse)
  (emms-browse-by-album))

;; Uppdate feed on opening elfeed
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

(setq exec-path (append exec-path '("/home/trigex/.local/bin")))
