(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;;(add-to-list 'package-archives
;;             '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

;; List of packages to use
(defvar my-packages '(better-defaults
                      magit
                      idle-highlight-mode
                      markdown-mode
                      projectile
                      color-theme-sanityinc-tomorrow
                      autopair
                      exec-path-from-shell
                      company
                      company-go
                      go-mode
                      php-mode
                      ws-butler
                      flycheck
                      helm)
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

