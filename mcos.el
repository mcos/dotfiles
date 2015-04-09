;; Set up our .emacs.d as a variable
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path (concat dotfiles-dir "custom"))

;; Need some Common Lisp Here
(require 'cl)

;; Projectile for Project Stuff
(require 'projectile)
(projectile-global-mode)

;; RET Behaves as LFD
;; Do this so that return will also indent. Very cool.
(defun RET-behaves-as-LFD ()
  (let ((x (key-binding "\C-j")))
    (local-set-key "\C-m" x)))

;; Company Mode - Autocompletion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay .2)           ; Set the popup delay to 0.25 seconds
(setq company-minimum-prefix-length 1)
(setq company-tooltip-limit 20)

;; Auto Pair everything
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; Require Final Newline
(setq mode-require-final-newline t)

;; Reload All Changes From Disk
(global-auto-revert-mode t)

;; Turn off bells
(setq ring-bell-function 'ignore)

;; Color Theme
(load-theme 'sanityinc-tomorrow-night t)

;; Turn off the silly startup message
(setq inhibit-startup-message t)

;; Winner mode
(winner-mode 1)

;; Show whitespace by default
(require 'ws-butler)
(ws-butler-global-mode)

;; Save and Restore Desktop Sessions
(desktop-save-mode 1)

;; GRAB THE PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Turn on the zshell properly
(setenv "ESHELL" (concat dotfiles-dir "eshell"))

;; Flycheck Syntax Checking
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Rename a file and a buffer
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
 (filename (buffer-file-name)))
    (if (not filename)
 (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
   (message "A buffer named '%s' already exists!" new-name)
 (progn
   (rename-file name new-name 1)
   (rename-buffer new-name)
   (set-visited-file-name new-name)
   (set-buffer-modified-p nil))))))

;;;;;;;;;;;;;;;;;
;; Go Specific ;;
;;;;;;;;;;;;;;;;;
(require 'go-mode)
(require 'company-go)
(require 'go-eldoc)

;; Set the GOPATH to $HOME/.go
;; That's what I use as my global GOPATH, because I'm a monster
;; In the future, maybe I'll get around to running emacs in server mode, then
;; I won't have to do things like this all the time

(setenv "GOPATH" "$HOME/.go")

;; Go-oracle
(load-file (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/oracle/oracle.el"))

;; Custom go-mode hook
(add-hook 'go-mode-hook (lambda ()
  "Custom Go Mode Hook"
  
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  
  ;; Imenu & Speedbar
  (setq imenu-generic-expression
        '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
          ("func" "^func *\\(.*\\) {" 1)))
  (imenu-add-to-menubar "Index")
  (set (make-local-variable 'company-backends) '(company-go))

  ;; Go-Oracle-Mode
  (go-oracle-mode)

  ;; Go-eldoc
  (go-eldoc-setup)

  ;; Key Bindings
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c t .") 'go-test-current-test)
  (local-set-key (kbd "C-c t f") 'go-test-current-file)
  (local-set-key (kbd "C-c t p") 'go-test-current-project)))

;;;;;;;;;;;;;;;;;
;;;;;  /go  ;;;;;
;;;;;;;;;;;;;;;;;

;; Load all projects into magit-repo-dirs
(eval-after-load "projectile" 
  '(progn (setq magit-repo-dirs (mapcar (lambda (dir)
                                         (substring dir 0 -1))
                                       (remove-if-not (lambda (project)
                                                        (file-directory-p (concat project "/.git/"))) 
                                                      (projectile-relevant-known-projects))))

         (setq magit-repo-dirs-depth 1)))

;; Line Numbers
(global-linum-mode t)

;; Column Numbers
(column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;
;; New Key Bindings ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Map C-x C-m to M-x - So we don't need to searching for Alt
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; C-w is now delete the current word. So we don't have to go and find BACKSPACE
(global-set-key "\C-w" 'backward-kill-word)

;; Remap Kill-Region to C-x C-k
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Magit Status
(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;;;;;;;
;; PHP-Mode ;;
;;;;;;;;;;;;;;
(autoload 'php-mode "php-mode" "Major mode for PHP work." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-hook 'php-mode-hook
          'php-enable-psr2-coding-style)
;;           '(flycheck-phpcs-standard "PSR2")

;; Doc block comments

(define-skeleton class-doc-block
        "Inserting doc block for class"
        "This is ignoring"
        "/**\n"
        " * Short description for class\n"
        " *\n"
        " * Long description for class (if any)...\n"
        " */"
        "class"
        '(indent-region (point-min) (point-max))
        '(indent-according-to-mode)
)

(define-skeleton function-doc-block
        "Inserting doc block for function/method"
        "This is ignoring"
        "/**\n"
        " * Description for method\n"
        " *\n"
        " * @param data_type $parameterName Parameter description\n"
        " * @return data_type Return value description\n"
        " */"
        '(indent-region (point-min) (point-max))
        '(indent-according-to-mode)
)

(define-skeleton variable-doc-block
        "Inserting doc block for variable"
        nil
        "/**\n"
        " * Description for variable - class member\n"
        " * \n"
        " * @type data_type \n"
        " */"
        '(indent-region (point-min) (point-max))
        '(indent-according-to-mode)
)

(add-hook 'php-mode-hook
        (lambda ()
        (define-key php-mode-map (kbd "C-c M-c") 'class-doc-block)
        (define-key php-mode-map (kbd "C-c M-f") 'function-doc-block)
        (define-key php-mode-map (kbd "C-c M-v") 'variable-doc-block)
        (RET-behaves-as-LFD))
        )
