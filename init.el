(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("marmalade" . "https://marmalade-repo.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; List of packages to use
(defvar my-packages '(better-defaults
                      magit
                      idle-highlight-mode
                      markdown-mode
                      projectile
                      autopair
                      exec-path-from-shell
                      company
                      company-go
                      go-mode
                      php-mode
                      ws-butler
                      flycheck
                      helm
                      el-get
                      go-eldoc
                      gotest
                      sx
                      multiple-cursors
                      helm-projectile
                      helm-ag
                      helm-open-github
                      vagrant
                      js2-mode
                      monochrome-theme
                      powerline
                      php-extras
                      evil
                      org)
  "A list of packages that should be installed at launch")

;; Make sure the packages are up to date
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Load My Custom Config
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq user-specific-config (concat dotfiles-dir "mcos.el"))

(if (file-exists-p user-specific-config) (load user-specific-config))
