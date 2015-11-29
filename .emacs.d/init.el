;; Get rid of bars of any kind, we don't need 'em
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Start the emacsclient server
(server-start)

;; Get rid of the welcome screen
(setq inhibit-startup-message t)

;; One letter yes-or-no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show line numbers next to lines
(global-linum-mode 1)

;; Highlight the current line
(global-hl-line-mode 1)

;; Hide the fringes
(fringe-mode -1)

;; Show us the time
(display-time)

;; Don't blink that cursor
(blink-cursor-mode -1)

;; Add some space to the right of the line numbers displayed
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
		     (count-lines (point-min) (point-max)))))
	 (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

;; Show line and column numbers in gutter
(setq line-number-mode t)
(setq column-number-mode t)

;; Show how big the file is in the gutter
(size-indication-mode t)

;; Show trailing whitespace at the end of a line (highlights in red by
;; default)
(setq show-trailing-whitespace t)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show matching parens
(show-paren-mode 1)

;; Uniquify buffer names if we have two files with same name open
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Electric pair mode
(electric-pair-mode 1)

;; UTF-8 everywhere
(setq locale-encoding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Set up a decent, basic color scheme (use customize-themes to find
;; new one)
(custom-set-variables
 '(custom-enabled-themes (quote (wombat))))

;; Bump up the font size a bit and use a prettier face
(cond ((member "Inconsolata" (font-family-list))
       (set-face-attribute 'default nil :height 180 :font "Inconsolata"))
      (else (set-face-attribute 'default nil :height 180)))

;; Customize hl-line-mode coloring. This needs to come after
;; configuring theming as some themes override these settings.
(set-face-foreground 'highlight nil)
(set-face-background 'highlight "dim grey")
(set-face-underline 'highlight nil)
