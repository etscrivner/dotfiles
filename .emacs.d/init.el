;; Get rid of bars of any kind, we don't need 'em
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Start the emacsclient server
(server-start)

;; Get rid of the welcome screen
(setq inhibit-startup-message t)

;; One letter yes-or-no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show line numbers next to lines
(global-linum-mode 1)
(global-hl-line-mode 1)

;; Add some space to the right of the line numbers displayed
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
		     (count-lines (point-min) (point-max)))))
	 (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

;; Show line and column numbers in gutter
(setq column-number-mode t)

;; Show how big the file is in the gutter
(size-indication-mode t)

;; Indicate empty lines at the end of a file
(setq-default indicate-empty-lines t)

;; Show trailing whitespace at the end of a line (highlights in red by default)
(setq show-trailing-whitespace t)

;; Set up a decent, basic color scheme (use customize-themes to find new one)
(custom-set-variables
 '(custom-enabled-themes (quote (wombat))))

;; Bump up the font size a bit and use a prettier face
(cond ((member "Inconsolata" (font-family-list))
       (set-face-attribute 'default nil :height 180 :font "Inconsolata"))
      (else (set-face-attribute 'default nil :height 180)))

;; Customize hl-line-mode coloring. This needs to come after
;; configuring theming as some themes override these settings.
(set-face-foreground 'highlight nil)
(set-face-background 'highlight "#333")
(set-face-underline 'highlight nil)
