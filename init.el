;; * Config helpers

(defun kd-font-available-p (font)
  "Check if FONT is available on the system."
  (find-font (font-spec :name font)))

(defun kd-use-font (font)
  "Use FONT if available."
  (when (kd-font-available-p font)
    (add-to-list 'default-frame-alist `(font . ,font))
    (set-frame-font font nil t)))

;; * Initialization

(require 'package)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq package-enable-at-startup nil)  ; don't initialize twice!

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; `use-package' is included in Emacs 29. When not present in older
;; versions, install it.
(when (and (version< emacs-version "29")
	   (not (package-installed-p 'use-package)))
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-defer t)

(eval-when-compile
  (require 'use-package))

;; * Basic configuration

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(global-unset-key (kbd "C-z"))

(use-package delight
  ;; Enables you to customise the mode names displayed in the mode line.
  ;; https://elpa.gnu.org/packages/delight.html
  :ensure t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(use-package eldoc
  :delight)

;; * TODO Files and directories

;; (let ((default-directory "~/.emacs.d/lisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))

;; * Appearance

(setq-default cursor-type 'bar)
(blink-cursor-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(when (display-graphic-p)
  (scroll-bar-mode 0))

;; Change font and font size

(set-face-attribute 'default (selected-frame) :height 100)
(kd-use-font "Consolas")

;; ** Themes

(use-package doom-themes
  ;; A megapack of themes for GNU Emacs, from the Doom Emacs configuration
  ;; framework.
  ;; https://github.com/doomemacs/themes
  :ensure t
  :init
  (load-theme 'doom-one t)
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; * Text editing

;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (show-paren-mode 1)
					; TODO Move that line

;; * File/project management

;; ** Default directory
(when (eq system-type 'windows-nt)
  (setq default-directory "C:/Users/Karine-PC-portable/emacs"))

;; ** Shortcut to init file
(defun kd-find-user-init-file ()
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c i") 'kd-find-user-init-file)

;; ** Projectile
;; TODO check if this correct
(use-package projectile
  :ensure t
  :init
  (setq projectile-track-known-projects-automatically nil)
  :config
  (projectile-global-mode))

;; ** Recentf
(setq recentf-max-saved-items 200)

;; * Which-key
(use-package which-key
  :ensure t
  :delight
  :defer 2
  :config
  (which-key-mode))

;; * Company
(use-package company
  :ensure t
  :demand
  :delight company-mode
  :config
  (global-company-mode))

;; * Rainbow-mode (Karine)
(use-package rainbow-mode
  :ensure t
  :delight
  :hook prog-mode)

;; * Org-mode

;; ** Open text files with Org-mode
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

;; ** Add task file to agenda files on my main PC
;; (when (and (equal system-name "PC-KARINE")
;; 	   (eq system-type 'windows-nt))
;;   (setq org-agenda-files '("C:/Users/Karine/Documents/Organisation/TODO.org")))

;; ** Ajoute un raccourci pour accéder à l'agenda Org
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

;; ** Raccourcis pour les blocs de code Org Babel
;; (with-eval-after-load "org"
;;   (add-to-list 'org-structure-template-alist '("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC")))

;; ** TODO keywords
(setq org-log-into-drawer t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-keywords
      '((sequence "TODO(t)"
                  "WAIT(w@)"
		  "URGE(u)"
		  "PROG(p)"
                  "|"
                  "DONE(d)"
		  "CNCL(c@)")))

(setq org-todo-keyword-faces
      '(("TODO" . "#cc0000")
	("PROG" . "#ffcc33")
	("WAIT" . "#ef7e49")
	("DONE" . "#009800")
	("CNCL" . "grey50")))

;; ** Custom priorities
(setq org-priority-faces '((65 . "wheat")
			   (66 . "light salmon")
			   (67 . "tomato")))

;; ** Org-emphasis
(setq org-emphasis-alist
  '(("*" (bold :background "#0097a0" :foreground "white"))
    ("/" (italic :foreground "aquamarine3"))
    ("_" underline)
    ("=" (:background "maroon" :foreground "white"))
    ("~" (:background "#b0d8fc" :foreground "#343434"))
    ("+" (:strike-through t))))

;; ** Speed commands
(setq org-use-speed-commands t)

;; ** Enable org-indent-mode by default
(setq org-startup-indented t)

;; * TODO Python
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;; * Popwin
(use-package popwin
  :ensure t
  :defer 1
  :config
  (popwin-mode 1))

;; * Emacs dashboard
(use-package dashboard
  ;; An extensible emacs startup screen showing you what's most important.
  ;; https://github.com/emacs-dashboard/emacs-dashboard
  :ensure t
  :demand t
  :config
  (setq dashboard-items '((recents  . 5)
                          (projects . 5))
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-page-separator "\n\f\n")
  (dashboard-setup-startup-hook))

(use-package all-the-icons
  ;; A utility package to collect various Icon Fonts and propertize them within
  ;; Emacs.
  ;; https://github.com/domtronn/all-the-icons.el
  :ensure t)

(use-package page-break-lines
  ;; This Emacs library provides a global mode which displays ugly form feed
  ;; characters as tidy horizontal rules.
  ;; https://github.com/purcell/page-break-lines
  :ensure t
  :delight
  :hook (dashboard-mode . page-break-lines-mode))

;; * Shortcut for inserting today's date
;; (defun insert-todays-date (arg)
;;   (interactive "P")
;;   (insert (if arg
;; 	      (format-time-string "%d-%m-%Y")
;; 	    (format-time-string "%Y-%m-%d"))))

(put 'upcase-region 'disabled nil)

;; exec path from shell path (for exporting org files to pdf)

(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; * Vertico & friends

(use-package vertico
  :ensure t
  :init
  (setq vertico-cycle t
        vertico-count 20)
  (vertico-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless basic)))

(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)
   ("M-g o" . consult-outline)
   ("M-s g" . consult-grep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)))
