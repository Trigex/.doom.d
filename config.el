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

(setq fancy-splash-image "~/.doom.d/cirno-dash.png")

(setq display-line-numbers-type 'relative)

(use-package! beacon
  :diminish beacon-mode
  :init
  (beacon-mode 1))

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

(setq org-directory "~/Documents/org")

(use-package! org-bullets
  :init
  (add-hook 'org-mode-hook (org-bullets-mode 1)))

(use-package! switch-window
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts '("h" "j" "k" "l"))
  :bind
  ([remap other-window] . switch-window))

(after! circe
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

(after! circe-notifications
  (setq circe-notifications-watch-strings
        '("HeXa" "LeJustice" "termer" "HelloMrEdwards" "FM" "tas" "tas_" "sage" "sage8" "Sentaku-san" "zenhead" "tusoud" "starman" "blakkheim"))
  (add-hook 'circe-server-connected-hook 'enable-circe-notifications))

(defun trigex-compile-config ()
  (interactive)
  (shell-command "$HOME/.emacs.d/bin/doom sync"))
