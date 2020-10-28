(setq exwm-input-global-keys
      `((,(kbd "SPC") . doom/leader)))
(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(require 'exwm-systemtray)
(exwm-config-default)
(setq exwm-randr-workspace-output-plist '(0 "HDMI-0"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command "xrandr" nil "xrandr --output HDMI-0 --mode 1920x1080 --pos 0x0 --rotate normal")))
(exwm-randr-enable)
(exwm-systemtray-enable)

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

(setq fancy-splash-image "~/.doom.d/cirno-dash.png")
(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        trigex-dashboard-widget-loaded))

(defun trigex-dashboard-widget-loaded ()
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (trigex-display-benchmark-h 'return))
    'face 'doom-dashboard-loaded)
   "\n"))

(defun trigex-display-benchmark-h (&optional return-p)
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

;(setq doom-leader-key "\\")

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
  ;; Options
  (circe-lagmon-mode)
  (enable-circe-display-images))

(after! circe-notifications
  (setq circe-notifications-watch-strings
        '("HeXa" "LeJustice" "termer" "HelloMrEdwards" "FM" "tas" "tas_" "sage" "sage8" "Sentaku-san" "zenhead" "tusoud" "starman" "blakkheim"))
  (add-hook 'circe-server-connected-hook 'enable-circe-notifications))

(defun trigex-compile-config ()
  (interactive)
  (shell-command "$HOME/.emacs.d/bin/doom sync"))

(defun trigex-save-screenshot ()
  (interactive)
  (shell-command "$HOME/.local/bin/scapmgr -f -s"))
(defun trigex-upload-screenshot ()
  (interactive)
  (shell-command "$HOME/.local/bin/scapmgr -f -u"))
