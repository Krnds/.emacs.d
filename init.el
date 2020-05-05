;; * Initialization

(require 'package)

(setq package-enable-at-startup nil)  ; don't initialize twice!

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-defer t)

(eval-when-compile
  (require 'use-package))

;; * Basic configuration

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(global-unset-key (kbd "C-z"))

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

;; Change font size globally

(set-face-attribute 'default (selected-frame) :height 100)

;; ** Themes

(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai t))

;; * Text editing

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(show-paren-mode 1)			; TODO Move that line

;; * File/project management

;; ** Default directory
(when (eq system-type 'windows-nt)
  (setq default-directory "C:/Users/Karine/Documents/Emacs/"))

;; ** Shortcut to init file
(defun kd-find-user-init-file ()
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c i") 'kd-find-user-init-file)

;; ** Projectile
;; TODO check if this correct
(use-package projectile
  :ensure t
  :after (helm)
  :init
  (when (require 'helm nil 'noerror)
    (setq projectile-completion-system 'helm))
  (setq projectile-track-known-projects-automatically nil)
  :config
  (projectile-global-mode))

;; ** Recentf
(setq recentf-max-saved-items 200)

;; * Helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :config
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (helm-mode 1))

;; * Which-key
(use-package which-key
  :ensure t
  :defer 2
  :config
  (which-key-mode))

;; * Company
(use-package company
  :ensure t
  :demand
  :config
  (global-company-mode))

;; * Rainbow-mode (Karine)
(use-package rainbow-mode
  :ensure t
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
(with-eval-after-load "org"
  (add-to-list 'org-structure-template-alist '("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC")))

;; ** TODO keywords
(setq org-log-into-drawer t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-keywords
      '((sequence "TODO(t)"
                  "WAIT(w@)"
		  "URGENT(u)"
                  "|"
                  "DONE(d)"
		  "CNCL(c@)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-todo)
	("URGENT" . "red4")
	("WAIT" . "orange")
	("DONE" . "lime green")
	("CNCL" . "grey50")))

;; ** Custom priorities
(setq org-priority-faces '((65 . "wheat")
			   (66 . "light salmon")
			   (67 . "tomato")))

;; ** Org-emphasis
(with-eval-after-load 'org
  (add-to-list 'org-emphasis-alist			   ;;
	       '("*" (:foreground "brown1"))		   ;;
	       '("/" (:foreground "aquamarine3"))))	   ;;

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

;; * Shortcut for inserting today's date
;(defun insert-todays-date (arg)
;  (interactive "P")
;  (insert (if arg
;	      (format-time-string "%d-%m-%Y")
;	    (format-time-string "%Y-%m-%d"))))


;; * TODO Bigloo
;; (require 'bigloo)
;; (require 'bmacs)

;; (when (eq system-type 'gnu/linux)
;;   (setq bigloo-name "/opt/bigloo/bin/bigloo"
;; 	ude-repl "/opt/bigloo/bin/bigloo"))

;; (with-eval-after-load 'bee-mode
;;   (define-key bee-mode-map (kbd "<f7>") 'ude-repl-send-buffer)
;;   (define-key bee-mode-map (kbd "<f6>") 'bee-repl-send-last-sexp))

;; * File-local variables
;; Local Variables:
;; eval: (orgstruct-mode)
;; eval: (setq-local orgstruct-heading-prefix-regexp ";; ")
;; End:

;; * Custom-set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (exec-path-from-shell use-package)))
 '(safe-local-variable-values
   (quote
    ((eval setq-local orgstruct-heading-prefix-regexp ";; ")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)

;; packages
(when (>= emacs-major-version 25)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

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
