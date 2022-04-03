;;; init.el ---                                      -*- lexical-binding: t; eval: (jerm-emacs-init-settings); -*-

;; Copyright (C) 2022  Jerm

;; Author: Jerm <jerm@pop-os>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
;;

;; * [[https://github.com/raxod502/straight.el][straight.el]]

;; ** Bootstrap =straight.el=

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ** Setup [[https://github.com/jwiegley/use-package][use-package]] for use with =straight.el=

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;; * My info


(setq prettify-symbols-mode t)
(setq user-full-name "Jerm"
      user-mail-address "glechisma.k1cs5@aleeas.com")
(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil)


;; * Install Scripts

;;
;; Set system vars
;; (defconst on-linux   (eq system-type 'gnu/linux))
;; (defconst on-mac     (eq system-type 'darwin))
;; (defconst on-bsd     (or on-mac (eq system-type 'berkeley-unix)))
;; (defconst on-windows (memq system-type '(cygwin windows-nt ms-dos)))
;; (defconst apt-installed (executable-find "apt"))
;; (defconst yum-installe (executable-find "yum"))
;; (defconst dnf-installed (executable-find "dnf"))
;; (defconst zypper-installed (executable-find "zypper"))
;; (defconst pacman-installed (executable-find "pacman"))
;; (defconst snap-installed (executable-find "snap"))
;; (defconst flatpak-installed (executable-find "flatpak"))
;; (defconst nix-installed (executable-find "nix"))
;; (defconst aptitude-installed (executable-find "aptitude"))
;; (defun ensure-guix ()
;;   "If guix isn't installed install it."
;;   (when (not (executable-find "guix"))
;;     (shell-command (concat user-emacs-directory " ./guix-install.sh"))))
;; (ensure-guix)
;; (defconst guix-installed (executable-find "guix"))
;; (when (not guix-installed)
;;     (run-with-timer 30 t (message "Gnu Guix failed to install, Please ensure it is installed.")))

;;
(require 'password-cache)
(setq password-cache-expiry nil)
(password-cache-add "passwd" "Nb&n5")
;; extend to verify that passwd is correct
(defun sudo-passwd-avail-p ()
  "Check if password is stored, if not promptuser for password.
store password in hashed variable."
  (when (not (password-in-cache-p "passwd"))
    (password-cache-add "passwd" (read-passwd "Password? "))))
(sudo-passwd-avail-p)
(defun sudo-shell-command (command)
  "Execute shell-command COMMAND as sudo."
  (interactive "MCommand: ")
  (shell-command (concat "echo " (gethash "passwd" password-data) " | sudo -S " command)))
(defconst user-home-dir (concat (getenv "HOME") "/"))
(defconst user-docs-dir (concat user-home-dir "Documents/"))
(defconst user-photos-dir (concat user-home-dir "Pictures/"))

(defun ensure-tmp-folder ()
  "Check that tmp folder exists, if not create it."
  (file-directory-p (concat user-home-dir  ".tmp"))
  (shell-command "if [ ! -d \"${tmp_dir}\" ]; then
    echo \"mkdir -p $tmp_dir\"
    mkdir -p \"${tmp_dir}\"
else
    echo \"Found temp dir $tmp_dir\"
fi"))
(ensure-tmp-folder)

;; (defun ensure-ruby ()
;;   "Check to see if ruby is installed, and install if needed."
;;   (shell-command "sudo apt install -y ruby"))

;; (defconst ruby-found (executable-find "ruby"))

;; (defun install-asciidoctor ()
;;   "Install asciidoctor."
;;   (shell-command "sudo apt install -y asciidoctor"))
;; (defun install-curl ()
;;   "Install curl"
;;   (shell-command "sudo apt install -y curl"))
;; (defun ensure-gitman-info ()
;;   "Check to see if gitman is available as info, if not install it.
;; But first check for asciidoc as it is a dependency."
;;   (when (not (executable-find "asciidoctor"))
;;     (install-asciidoctor))
;;   (when (not (executable-find "curl"))
;;     (install-curl))
;;   (when (not (executable-find "makeinfo"))
;;     )
;;   (when (not (executable-find "docbook2X"))
;;     )
;;   (when (not (Info-index-nodes "Git"))
;;   ;;   (shell-command "tmp_dir=\"${HOME}/.tmp\"
;; ;; git clone https://github.com/git/git ${tmp_dir}/git
;; ;; cd ${tmp_dir}/git/
;; ;; make install info install-info USE_ASCIIDOCTOR=YesPlease
;; ;; cd
;; ;; rm -rf ${tmp_dir}/git")
;;     ))
;; (ensure-gitman-info)


;; (defun check-for-keepass-cli ()
;;   "Check to see if KeePassXC is intalled, and installit if needed."
;;   (when (not (executable-find "keepassxc"))
;;     (message "KeePassXC not found, installing...")
;;     (not (executable-find "keepassxc"))
;;     (shell-command "guix package -i keepassxc")))
;; (check-for-keepass-cli)

;;
;; Check for and if missing install FiraCode

(defun check-for-font (font)
  "Check to see if FONT is available."
  (interactive "*sWhat font to check for: ")
  (if (font-info font)
      (message (concat font " available in Emacs."))
    (message  (concat font " not available in Emacs."))))
(when (member "FiraCode" (font-family-list))
   (set-face-attribute 'default nil :font "NotoSans"))
(defun check-for-minimap-font ()
  "Check to see if Minimap Font is available, if not install it."
  (when (not (font-info "Minimap"))
    (shell-command "fonts_dir=\"${HOME}/.fonts\"
    if [ ! -d \"${fonts_dir}\" ]; then
    echo \"mkdir -p $fonts_dir\"
    mkdir -p \"${fonts_dir}\"
else
    echo \"Found fonts dir $fonts_dir\"
fi
git clone https://github.com/davestewart/minimap-font/ ${fonts_dir}/minimap-font

echo \"fc-cache -f\"
fc-cache -f")))
(check-for-minimap-font)
(defun check-for-fira-code ()
  "Check to see if FiraCode is available, if not install it."
  (when (not (font-info "FiraCode"))
    (shell-command "fonts_dir=\"${HOME}/.fonts\"
    if [ ! -d \"${fonts_dir}\" ]; then
    echo \"mkdir -p $fonts_dir\"
    mkdir -p \"${fonts_dir}\"
else
    echo \"Found fonts dir $fonts_dir\"
fi
wget https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip
unzip -d ${fonts_dir} Fira_Code_v6.2.zip
rm Fira_Code_v6.2.zip

echo \"fc-cache -f\"
fc-cache -f")))
(check-for-fira-code)

(defun install-gitman-info ()
  "Do stuff"
  (when (not (executable-find "asciidoc"))
    (shell-command "guix package -i asciidoc"))
  (when (not (executable-find "docbook2x"))
    (sudo-shell-command " apt install -y docbook2x"))
  

;; Doesn't work
;;   (shell-command "wget https://github.com/git/git/archive/master.tar.gz
;; tar xf master.tar.gz
;; cd git-master/Documentation
;; make git.info gitman.info
;; mv git.info gitman.info ~/.local/share/info
;; cd ~/.local/share/info
;; install-info git.info dir
;; install-info gitman.info dir")
)


(defadvice Info-follow-nearest-node (around gitman activate)
  "When encountering a cross reference to the `gitman' info
manual, then instead of following that cross reference show
the actual manpage using the function `man'."
  (let ((node (Info-get-token
	       (point) "\\*note[ \n\t]+"
	       "\\*note[ \n\t]+\\([^:]*\\):\\(:\\|[ \n\t]*(\\)?")))
    (if (and node (string-match "^(gitman)\\(.+\\)" node))
	(progn (require 'man)
	       (man (match-string 1 node)))
      ad-do-it)))

;; * Splash

;;
;; Custom dual-splash

(straight-use-package 'dash)
(straight-use-package '(emacs-splash :type git :host github :repo "rougier/emacs-splash"
				     :fork (:host github :repo "glechismi/emacs-splash")))
(require 'kisses)
(add-hook 'kisses-mode-hook 'page-break-mode -1)
(setq initial-buffer-choice 'kisses-initial-buffer)
(require 'splash-screen)
(run-with-timer 0.0 nil 'splash-screen)
(put-text-property 0 (length kisses-banner) 'face 'font-lock-keyword-face ;;outline-1
		   kisses-banner)
(run-with-timer 5.0 nil 'fade-out-kill-splash)


;; * Theme

;; ** [[https://gitlab.com/protesilaos/modus-themes][modus-themes]]

(use-package modus-themes
    :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-syntax '(green-strings alt-syntax yellow-comments)
        modus-themes-mixed-fonts t
        modus-themes-links '(neutral-underline)
        modus-themes-prompts '(background bold)
        modus-themes-mode-line '(3d accented borderless 1.2)
        modus-themes-tabs-accented t
        modus-themes-completions
        '((matches . (extrabold background underline))
          (selection . (semibold text-also underline))
          (popup . (semibold accented text-also underline)))
        modus-themes-fringes '(intense)
        modus-themes-lang-checkers '(text-also)
        modus-themes-hl-line '()
        modus-themes-subtle-line-numbers t
        modus-themes-intense-mouseovers nil
        modus-themes-markup '(background)
        modus-themes-paren-match '(bold intense)
        modus-themes-diffs '(bg-only)
        ;; modus-themes-org-blocks '(tinted-background)
        modus-themes-headings
        '((1 . ( (height . 1.0) variable-pitch rainbow ultrabold))
          (2 . ( (height . 1.0) variable-pitch rainbow ultrabold))
          (3 . ( (height . 1.0) variable-pitch rainbow ultrabold))
          (4 . ( (height . 1.0) variable-pitch rainbow ultrabold))
          (5 . ( (height . 1.0) variable-pitch rainbow ultrabold))
          (6 . ( (height . 1.0) variable-pitch rainbow ultrabold))
          (7 . ( (height . 1.0) variable-pitch rainbow ultrabold))
          (8 . ( (height . 1.0) variable-pitch rainbow ultrabold)))
        modus-themes-variable-pitch-ui t
        modus-themes-region '(bg-only no-extend))
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi) ;; OR (modus-themes-load-operandi) 
  :bind ("<f5>" . modus-themes-toggle)
  )


;; * Misc UI

;; ** Setup path

(add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))

;; ** [[https://github.com/alphapapa/topsy.el][topsy.el]]

(use-package topsy
  :hook
  (prog-mode . topsy-mode))

;; ** [[https://github.com/alphapapa/unpackaged.el][lorem-ipsum-hide-text]]

(defcustom unpackaged/lorem-ipsum-overlay-exclude nil
  "List of regexps to exclude from `unpackaged/lorem-ipsum-overlay'."
  :type '(repeat regexp))

(defun lorem-ipsum-overlay (&optional replace-p)
  "Overlay all text in current buffer with \"lorem ipsum\" text.
When called again, remove overlays.  Useful for taking
screenshots without revealing buffer contents.

If REPLACE-P is non-nil (interactively, with prefix), replace
buffer contents rather than overlaying them.  When a buffer is
very large and would have so many overlays that performance would
be prohibitively slow, you may replace the buffer contents
instead.  (Of course, be careful about saving the buffer after
replacing its contents.)

Each piece of non-whitespace text in the buffer is compared with
regexps in `unpackaged/lorem-ipsum-overlay-exclude', and ones
that match are not overlaid.  Note that the regexps are compared
against the entire non-whitespace token, up-to and including the
preceding whitespace, but only the alphabetic part of the token
is overlaid.  For example, in an Org buffer, a line that starts
with:

  #+TITLE: unpackaged.el

could be matched against the exclude regexp (in `rx' syntax):

  (rx (or bol bos blank) \"#+\" (1+ alnum) \":\" (or eol eos blank))

And the line would be overlaid like:

  #+TITLE: parturient.et"
  (interactive "P")
  (require 'lorem-ipsum)
  (let ((ovs (overlays-in (point-min) (point-max))))
    (if (cl-loop for ov in ovs
                 thereis (overlay-get ov :lorem-ipsum-overlay))
        ;; Remove overlays.
        (dolist (ov ovs)
          (when (overlay-get ov :lorem-ipsum-overlay)
            (delete-overlay ov)))
      ;; Add overlays.
      (let ((lorem-ipsum-words (--> lorem-ipsum-text
                                    (-flatten it) (apply #'concat it)
                                    (split-string it (rx (or space punct)) 'omit-nulls)))
            (case-fold-search nil))
        (cl-labels ((overlay-group (group)
                                   (let* ((beg (match-beginning group))
                                          (end (match-end group))
                                          (replacement-word (lorem-word (match-string group)))
                                          (ov (make-overlay beg end)))
                                     (when replacement-word
                                       (overlay-put ov :lorem-ipsum-overlay t)
                                       (overlay-put ov 'display replacement-word))))
                    (replace-group (group)
                                   (let* ((beg (match-beginning group))
                                          (end (match-end group))
                                          (replacement-word (lorem-word (match-string group))))
                                     (when replacement-word
                                       (setf (buffer-substring beg end) replacement-word))))
                    (lorem-word (word)
                                (if-let* ((matches (lorem-matches (length word))))
                                    (apply-case word (downcase (seq-random-elt matches)))
                                  ;; Word too long: compose one.
                                  (apply-case word (downcase (compose-word (length word))))))
                    (lorem-matches (length &optional (comparator #'=))
                                   (cl-loop for liw in lorem-ipsum-words
                                            when (funcall comparator (length liw) length)
                                            collect liw))
                    (apply-case (source target)
                                (cl-loop for sc across-ref source
                                         for tc across-ref target
                                         when (not (string-match-p (rx lower) (char-to-string sc)))
                                         do (setf tc (string-to-char (upcase (char-to-string tc)))))
                                target)
                    (compose-word (length)
                                  (cl-loop while (> length 0)
                                           for word = (seq-random-elt (lorem-matches length #'<=))
                                           concat word
                                           do (cl-decf length (length word)))))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward (rx (group (1+ (or bol bos blank (not alpha)))
                                                 (0+ (not (any alpha blank)))
                                                 (group (1+ alpha))
                                                 (0+ (not (any alpha blank)))))
                                      nil t)
              (unless (cl-member (match-string 0) unpackaged/lorem-ipsum-overlay-exclude
                                 :test (lambda (string regexp)
                                         (string-match-p regexp string)))
                (if replace-p
                    (replace-group 2)
                  (overlay-group 2)))
              (goto-char (match-end 2)))))))))

;; ** Scroll near edge

(setq scroll-margin 2)

;; ** [[https://github.com/Fuco1/dired-hacks][dired-subtree]]

(use-package dired-subtree)

;; ** [[https://github.com/jojojames/dired-sidebar][dired-sidebar]]

(use-package dired-sidebar
  :bind (("C-c C-d" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init 
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config 
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;; ** [[https://github.com/rougier/nano-sidebar][nano-sidebar]]

(use-package nano-sidebar
  :straight (nano-sidebar :type git :host github
                                     :repo "rougier/nano-sidebar"))

;; (defun nano-sidebar-mu4e-init (frame sidebar)
;;   (select-frame sidebar)
;;   (do-some-stuff...))

;; (add-to-list 'nano-sidebar-properties
;;    `("mu4e"    36 dark ,nano-dark-background nano-sidebar-mu4e-init))

;; (set-frame-parameter nil 'name "mu4e")
;; (nano-sidebar-toggle)

;; ** Time in modeline

(display-time-mode 1)

;; ** Battery in modeline


(unless (string-match-p "^Power N/A" (battery))
  (display-battery-mode 1))

;; ** Cool ellipsis

(setq truncate-string-ellipsis "…")

;; ** [[https://github.com/tarsius/minions][minions]]

(use-package minions
  :bind
  ("C-c %" . minions-minor-modes-menu)
  :init
  (minions-mode +1))

;; ** [[https://gitlab.com/protesilaos/lin][lin]]

(use-package lin
  :straight '(:type git :host gitlab
                    :repo "protesilaos/lin")
  :config
  (setq lin-mode-hooks
      '(bongo-mode-hook
        dired-mode-hook
        elfeed-search-mode-hook
        git-rebase-mode-hook
        ibuffer-mode-hook
        ilist-mode-hook
        ledger-report-mode-hook
        log-view-mode-hook
        magit-log-mode-hook
        mu4e-headers-mode
        notmuch-search-mode-hook
        notmuch-tree-mode-hook
        occur-mode-hook
        org-agenda-mode-hook
        tabulated-list-mode-hook)
      lin-face 'lin-green)
(lin-global-mode +1))

;; ** [[https://gitlab.com/protesilaos/pulsar][pulsar]]

(use-package pulsar
  :config
  (setq pulsar-pulse-functions
      '(isearch-repeat-forward
        isearch-repeat-backward
        recenter-top-bottom
        move-to-window-line-top-bottom
        reposition-window
        bookmark-jump
        other-window
        delete-window
        delete-other-windows
        forward-page
        backward-page
        scroll-up-command
        scroll-down-command
        windmove-right
        windmove-left
        windmove-up
        windmove-down
        windmove-swap-states-right
        windmove-swap-states-left
        windmove-swap-states-up
        windmove-swap-states-down
        tab-new
        tab-close
        tab-next
        org-next-visible-heading
        org-previous-visible-heading
        org-forward-heading-same-level
        org-backward-heading-same-level
        outline-backward-same-level
        outline-forward-same-level
        outline-next-visible-heading
        outline-previous-visible-heading
        outline-up-heading)
      pulsar-pulse t
      pulsar-delay 0.055
      pulsar-iterations 10
      pulsar-face 'pulsar-magenta
      pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1)
  :bind
  ("C-x l" . pulsar-pulse-line)
  ("C-x L" . pulsar-highlight-line))

;; ** =mode-line-bell=

(require'mode-line-bell)
(mode-line-visual-bell)

;; ** [[https://github.com/abo-abo/avy][avy]]

(use-package avy 
  :config
  (avy-setup-default)
  :init
  (setq avy-style 'pre
  avy-timout-seconds 1.2)
  :bind
  ("M-]" . avy-goto-char-timer)
  ("C-c ]" . avy-org-goto-heading-timer))

;; ** [[https://github.com/abo-abo/ace-window][ace-window]]

(use-package ace-window
  :bind
  ("M-o" . ace-window))
(setq aw-ignored-buffers
      '("*Minimap*" "*Calc Trail*" "*LV*"))
(setq aw-ignore-on t)

;; ** [[https://github.com/noctuid/link-hint.el][link-hint]]

(use-package link-hint
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

;; ** [[https://github.com/ab0abo/define-word][define-word]]

(use-package define-word)
;; ** [[https://github.com/bkaestner/redacted.el][redacted.el]]

(use-package redacted)

(define-key global-map (kbd "<f12>") 'redacted-mode)

;; ** [[https://github.com/rougier/emacs-svg-icon][emacs-svg-icon]]

;; (use-package emacs-sgv-icon)

;; ** [[https://github.com/rougier/svg-lib][svg-lib]]

(use-package svg-lib)

;; ** [[https://github.com/rougier/svg-tags][svg-tags]]
(use-package svg-tag-mode)
(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}")

(defun svg-progress-percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0)) :ascent 'center))

(defun svg-progress-count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ count total) nil
                                    :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag value nil
                           :stroke 0 :margin 0)) :ascent 'center)))
(setq svg-tag-tags
      `(
         ("|[0-9a-zA-Z- ]+?|" . ((lambda (tag)
                                  (svg-tag-make tag :face 'holiday
                                                :margin 0 :beg 1 :end -1))))
        ("\([0-9a-zA-Z]\)" . ((lambda (tag)
                                (svg-tag-make tag :beg 1 :end -1 :radius 12))))
        ("\([0-9a-zA-Z][0-9a-zA-Z]\)" . ((lambda (tag)
                                           (svg-tag-make tag :beg 1 :end -1 :radius 8))))
        ;; Org tags
        (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
        (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))
        
        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :face 'org-priority 
                                            :beg 2 :end -1 :margin 0))))

        ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                            (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                          (svg-progress-count (substring tag 1 -1)))))
        
        ;; TODO / DONE
        ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
        ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


        ;; Citation of the form [cite:@Knuth:1984] 
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))

        
        ;; Active date (without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0))))
        (,(format "\\(<%s *\\)%s>" date-re time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
        (,(format "<%s *\\(%s>\\)" date-re time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

        ;; Inactive date  (without day name, with or without time)
         (,(format "\\(\\[%s\\]\\)" date-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
         (,(format "\\(\\[%s *\\)%s\\]" date-re time-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
         (,(format "\\[%s *\\(%s\\]\\)" date-re time-re) .
          ((lambda (tag)
             (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))

;; (global-svg-tag-mode)

;; ** [[https://github.com/alphapapa/prism.el][prism.el]]

(use-package prism
  :bind
  ("C-c C-c p" . prism-mode)
  ("C-c C-c P" . prism-randomize-colors))

;; ** =page-break-lines=

(use-package page-break-lines)
(global-page-break-lines-mode)

;; ** [[][transient]]

(use-package transient)

;; ** [[https://gitlab.com/sawyerjgardner/demap.el][demap.el]]

(use-package demap
  :config
  (face-spec-set 'demap-minimap-font-face
                   `((t :inherit    unspecified
                        :family     "minimap")))

  :bind (("C-c m" . demap-toggle)))

;; ** [[https://github.com/trevorpogue/topspace][topspace]]

(use-package topspace
  :init
  (global-topspace-mode 1))
;; ** Line Numbers

(defcustom jerm-emacs-line-numbers-enabled-modes
  '(conf-mode prog-mode)
  "Modes which should display line numbers."
  :type 'list
  :group 'my-ui)
(defcustom jerm-emacs-line-numbers-disabled-modes
  '(org-mode)
  "Modes which should not display line numbers.
Modes derived from the modes defined in
`jerm-emacs-line-number-enabled-modes', but should not display line numbers."
  :type 'list
  :group 'my-ui)
(defun jerm-emacs--enable-line-numbers-mode ()
  "Turn on line numbers mode.
Used as hook for modes which should display line numbers."
  (display-line-numbers-mode 1))
(defun jerm-emacs--disable-line-numbers-mode ()
  "Turn off line numbers mode.
Used as hook for modes which should not display line numebrs."
  (display-line-numbers-mode -1))
(defun jerm-emacs--update-line-numbers-display ()
  "Update configuration for line numbers display."
  (if jerm-emacs-display-line-numbers
      (progn
        (dolist (mode jerm-emacs-line-numbers-enabled-modes)
          (add-hook (intern (format "%s-hook" mode))
                    #'jerm-emacs--enable-line-numbers-mode))
        (dolist (mode jerm-emacs-line-numbers-disabled-modes)
          (add-hook (intern (format "%s-hook" mode))
                    #'jerm-emacs--disable-line-numbers-mode))
        (setq-default
         display-line-numbers-grow-only t
         display-line-numbers-type t
         display-line-numbers-width 2))
     (progn
       (dolist (mode jerm-emacs-line-numbers-enabled-modes)
         (remove-hook (intern (format "%s-hook" mode))
                      #'jerm-emacs--enable-line-numbers-mode))
       (dolist (mode jerm-emacs-line-numbers-disabled-modes)
         (remove-hook (intern (format "%s-hook" mode))
                      #'jerm-emacs--disable-line-numbers-mode)))))
(defcustom jerm-emacs-display-line-numbers nil
  "Whether line numbers should be enabled."
  :type 'boolean
  :group 'my-ui
  :set (lambda (sym val)
         (set-default sym val)
         (jerm-emacs--update-line-numbers-display)))

;; ** Transparency

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; ** wallpaper

(defconst user-wallpaper-dir (concat user-photos-dir "wallpapers/"))
(defun set-wallpaper ()
  (shell-command (concat "gsettings set org.gnome.desktop.background picture-uri file://" user-wallpaper-dir "hexFadeMoon.jpg")))
(set-wallpaper)


;; * Completion

;; ** [[https://github.com/minad/cape][cape]]

;; setup later with super-capfs

;; ** [[https://github.com/minad/vertico][vertico]]

(use-package vertico
  :init
  (vertico-mode))

;; Cycle back to top/bottom result when the edge is reached
(customize-set-variable 'vertico-cycle t)
(define-key vertico-map "?" #'minibuffer-completion-help)
(define-key vertico-map (kbd "M-RET") #'minibuffer-force-complete-and-exit)
(define-key vertico-map (kbd "M-TAB") #'minibuffer-complete)
(setq completion-styles '(substring orderless))
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))
(advice-add #'completing-read-multiple
            :override #'consult-completing-read-multiple)
(global-set-key (kbd "TAB") 'completion-at-point)

;; ** [[https://github.com/oantolin/orderless][orderless]]

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; ** Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; ** [[https://github.com/abo-abo/swiper#counsel][counsel]]

(use-package counsel)
;; (counsel-mode -1)

;; ** [[https://github.com/minad/consult][consult]]

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ("C-s" . consult-line)                    ;; orig. isearch-foreward
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
 
  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

;; ** [[https://github.com/minad/marginalia][marginalia]]
;;
;; Enable richer annotations using the Marginalia package

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (;("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; ** [[https://github.com/oantolin/embark][embark]]

(use-package embark

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: C-;
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;;
;; Consult users will also want the embark-consult package.

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; * MISC

;; ** [[https://github.com/bbatsov/crux][crux]]

(use-package crux
  :bind (("C-c s" . crux-cleanup-buffer-or-region)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c C-x k" . crux-kill-other-buffers)
         ("C-c C-o" . crux-other-window-or-switch-buffer)))

;; ** Increase undo limit

(setq undo-limit 80000000)

;; ** [[https://github.com/jschaf/emacs-lorem-ipsum][emacs-lorem-ipsum]]

(use-package lorem-ipsum
  :bind ("C-c C-c l" . lorem-ipsum-transient))

(transient-define-prefix lorem-ipsum-transient ()
  "Org-remark options."
  [("p" "Insert Paragraph of Lorem Ipsum" lorem-ipsum-insert-paragraphs)
   ("s" "Insert Sentance of Lorem Ipsum" lorem-ipsum-insert-sentences)
   ("l" "Insert Lorem Ipsum List item" lorem-ipsum-insert-list)])

;; ** [[https://github.com/alphapapa/unpackaged.el][font-compare]]

(require 'seq)

(defvar lorem-ipsum-text)

;; Code by alphapapa
(defun font-compare (text fonts)
  "Compare TEXT displayed in FONTS.
If TEXT is nil, use `lorem-ipsum' text.  FONTS is a list of font
family strings and/or font specs.

Interactively, prompt for TEXT, using `lorem-ipsum' if left
empty, and select FONTS with `x-select-font', pressing Cancel to
stop selecting fonts."
  (interactive (list (pcase (read-string "Text: ")
                       ("" nil)
                       (else else))
                     ;; `x-select-font' calls quit() when Cancel is pressed, so we use
                     ;; `inhibit-quit', `with-local-quit', and `quit-flag' to avoid that.
                     (let ((inhibit-quit t))
                       (cl-loop for font = (with-local-quit
                                             (x-select-font))
                                while font
                                collect font into fonts
                                finally do (setf quit-flag nil)
                                finally return fonts))))
  (setq text (or text (s-word-wrap 80 (s-join " " (progn
                                                    (require 'lorem-ipsum)
                                                    (seq-random-elt lorem-ipsum-text))))))
  (with-current-buffer (get-buffer-create "*Font Compare*")
    (erase-buffer)
    (--each fonts
      (let ((family (cl-typecase it
                      (font (symbol-name (font-get it :family)))
                      (string it))))
        (insert family ": "
                (propertize text
                            'face (list :family family))
                "\n\n")))
    (pop-to-buffer (current-buffer))))

;; ** Auto save

(setq auto-save-default t)

;; ** Transient mark mode

(transient-mark-mode t)

;; ** Syntax everywhere

(global-font-lock-mode t)

;; ** Overwrite region on yank

(delete-selection-mode t)

;; ** Confirm on closing emacs

(setq confirm-kill-emacs 'y-or-n-p)

;; ** [[https://github.com/alphapapa/ox-elisp][ox-elisp]]

(use-package ox-elisp
  :straight (ox-elisp :host github :type git :repo "alphapapa/ox-elisp"))

;; ** Create dir on save if needed

;; When saving a file in a directory that doesn't exist, offer
;; to (recursively) create the file's parent directories.
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;; ** Revert Dired and other buffers

(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; ** Revert buffers when the underlying file has changed

(global-auto-revert-mode 1)

;; ** Don't use tabs

(setq-default indent-tabs-mode nil)

;; ** Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"

(fset 'yes-or-no-p 'y-or-n-p)

;; ** Turn on recentf mode

(add-hook 'after-init-hook #'recentf-mode)
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))

;; ** Do not saves duplicates in kill-ring

(customize-set-variable 'kill-do-not-save-duplicates t)

;; ** Parentheses

(electric-pair-mode 1)

;; ** Make scrolling less stuttered

(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; ** Better support for files with long lines

(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; ** Make shebang (#!) file executable when saved

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; ** Comment dwim

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end))) 
(define-key global-map (kbd "M-[") 'comment-dwim)
(define-key global-map (kbd "M-;") 'comment-or-uncomment-region-or-line)
;; ** kill buffer

(defun kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(define-key global-map (kbd "C-x k") 'kill-current-buffer)
(define-key global-map (kbd "C-x K") 'kill-buffer)

;; ** switch window on creation

(defun split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

;; (defadvice prompt-for-buffer (&rest _)
;;   :after '(split-window-right-and-switch split-window-below-and-switch)
;;   (consult-buffer))


(define-key global-map (kbd "C-x 2") 'split-window-below-and-switch)
(define-key global-map (kbd "C-x 3") 'split-window-right-and-switch)


;; ** Add keymap to open init file

(defun open-user-init ()
  "Open the user init file."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))
(global-set-key (kbd "C-c q") 'open-user-init)
(defun jerm-emacs-init-settings ()
  "Activate a couple minor modes upon opening my init file."
  (demap-open)
  (outshine-mode))

;; ** Allow my custom variables to be evaluated in my init

(add-to-list 'safe-local-variable-values '(eval jerm-emacs-init-settings))

;; (add-hook 'find-file-hook #'init-olmm)  
;; (defun init-olmm ()
;;   "Enable `org-link-minor-mode' in the user-init-file."
;;   (when (string-equal user-init-file buffer-file-name))
;;     (org-link-minor-mode))

;; ** [[https://github.com/iqbalansari/restart-emacs][restart-emacs]]

(use-package restart-emacs
  :config
  :bind (("C-c M-r" . restart-emacs)
         ("C-c M-n" . restart-emacs-start-new-emacs)))
;; ** [[https://github.com/MaximeWack/seriestracker][seriestracker]]

(use-package seriestracker
  :demand                                                   ;;To force loading seriestracker
  :config                                                   ;;These are the default
  (setq seriestracker-file (concat user-emacs-directory "seriestracker.el")
        seriestracker--fold-cycle 'seriestracker-all-folded ;; can also be 'seriestracker-all-unfolded or 'seriestracker-series-folded. Will deternine the folding at startup
        seriestracker-show-watched "show"                   ;; whether to hide or "show" the watched episodes
        seriestracker-sorting-type "next"))                 ;; or "alpha" for alphabetic sort

;; ** [[https://github.com/ifosch/keepass-mode][keepass-mode]]

(use-package keepass-mode)

;; ** Zone

(require 'zone)
(zone-when-idle 120)


;; * Editing

;; ** Treat camelCase & lisp-case as separate words.
(use-package diredfl)
(add-hook 'prog-mode-hook 'subword-mode)

;; ** [[https://github.com/purcell/package-lint][package-lint]]

(use-package package-lint)

;; ** Don't use tabs

(setq-default indent-tabs-mode nil)

;; ** [[https://github.com/jaotavora/yasnippet][yasnippet]]

(use-package yasnippet)

;; [[http://github.com/AndreaCrotti/yasnippet-snippets][yasnippet-snippets]]

(use-package yasnippet-snippets)
(defconst yasnippet-snippets-dir (concat user-emacs-directory "straight/repos/yasnippet-snippets/snippets"))
(defconst my-snippets-dir (concat user-emacs-directory "my-snippets"))
(setq yas-snippet-dirs
      '(yasnippet-snippets-dir
        my-snippets-dir))


;; (yas-reload-all)
;; (add-hook 'prog-mode-hook #'yas-minor-mode)

(yas-global-mode 1 )

;; 
;; [[https://github.com/abo-abo/auto-yasnippet][auto-yasnippet]]

(use-package auto-yasnippet
  :bind
  ("C-c y" . #'aya-expand)
  ("C-c t" . #'aya-create))


;; ** Guile

;; *** [[https://gitlab.com/emacs-geiser/geiser][geiser]]

(use-package geiser)

;; *** [[https://gitlab.com/emacs-geiser/guile][geiser-guile]]

(use-package geiser-guile)

;; * Fonts

;; ** Defaults

(defun jerm-emacs--set-default-font (spec)
  "Set the default font based on SPEC.

SPEC is expected to be a plist with the same key names
as accepted by `set-face-attribute'."
  (when spec
    (apply 'set-face-attribute 'default nil spec)))
(defcustom jerm-emacs-default-font nil
  "The configuration of the `default' face.
Use a plist with the same key names as accepted by `set-face-attribute'."
  :group 'rational
  :type '(plist :key-type: symbol)
  :tag "Default font"
  :set (lambda (sym val)
         (let ((prev-val (if (boundp 'jerm-emacs-default-font)
                             jerm-emacs-default-font
                         nil)))
         (set-default sym val)
         (when (and val (not (eq val prev-val)))
           (jerm-emacs--set-default-font val)))))


;;
;; Setup FiraCode as default font
(custom-set-variables
 '(jerm-emacs-default-font
   '(:font "FiraCode" :weight regular :height 110)))
(use-package ligature
  :straight (ligature :type git :host github
                       :repo "mickeynp/ligature.el")
  ;; :load-path
  :config
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  
  ;; Enable ligatures in programming modes                                           
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%")))
(global-ligature-mode 't)

;; ** [[https://github.com/duckwork/titlecase.el][titlecase.el]]

(use-package titlecase)


;; * Help Buffers 

;; ** [[https://github.com/Wilfred/helpful][helpful]]

(use-package helpful)

;; Make `describe-*' screens more helpful
(require 'helpful)
(define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
(global-set-key [remap describe-command] #'helpful-command)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-key] #'helpful-key)
(global-set-key [remap describe-symbol] #'helpful-symbol)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key (kbd "C-h F") #'helpful-function)

;; Bind extra `describe-*' commands
(global-set-key (kbd "C-h K") #'describe-keymap)

;; ** [[https://github.com/xuchunyang/elisp-demos][elisp-demos]]

(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; ** [[https://github.com/karthink/popper][popper]]

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Warnings\\*"
          "\\*straight-process\\*"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints



;; * Org

;; ** Org

(use-package org)
(setq org-directory (concat user-docs-dir "Org"))

;; ** [[https://github.com/rougier/org-outer-indent][org-outer-indent]]

(use-package org-outer-indent
  :straight (org-outer-indent :type git :host github
                              :repo "rougier/org-outer-indent"))

;; ** [[https://github.com/nobiot/org-transclusion][org-transclusion]]

(use-package org-transclusion
  :after org
  :bind (("<f8>" . org-transclusion-add)
         ("C-<f8>" . org-transclusion-mode)))

;; ** org-capture-templates

;; TODO setup a transient for =org-capture=

(define-key global-map (kbd "C-c c") 'org-capture)

(defvar org-capture-templates '())

(add-to-list 'org-capture-templates
             '("c" "Contact"
               entry
               (file "~/Documents/org/contacts.org")
               "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{123 Fake St., City, ST 12345}
:PHONE: %^{555-555-5555}
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{note}
:END:"))

(add-to-list 'org-capture-templates
             '("f" "Finished book"
               entry
               (file+headline "~/Documents/org/notes/books-read.org" "Books")
               "* %^{Title} -- %^{Author}\n%t\n"))

(add-to-list 'org-capture-templates
             '("r" "RPG prompt"
               entry
               (file "~/Documents/org/RPG-ideas.org")
               "* %?\n   %t\n"))

(add-to-list 'org-capture-templates
             '("s" "Subscribe to an RSS feed"
               plain
               (file "~/rss-feeds.org")
               "*** [[%^{Feed URL}][%^{Feed name}]]"))

;; ** Tags

(defun org-add-tag (new-tag)
  (org-set-tags (cons new-tag
                      (seq-remove (lambda (tag)
                                    (get-text-property 0 'inherited tag))
                                  (org-get-tags)))))
;; ** Ensure org-links show up as must description

(setq org-descriptive-links t)

;; ** [[https://github.com/minad/org-modern][org-modern]]

(use-package org-modern
  :hook
  (org-mode-hook . org-modern-mode)
  (org-agenda-finalize-hook . org-modern-agenda))

;; ** Hide emphasis markers

(setq org-hide-emphasis-markers t)

;; ** [[https://github.com/awth13/org-appear][org-appear]]

(use-package org-appear
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))
  
;; ** [[https://github.com/alphapapa/outshine][outshine]]

(use-package outshine
  :config
  (defvar outline-minor-mode-prefix "C-c @")
  (add-hook 'emacs-lisp-mode-hook 'outshine-mode))

;; ** [[https://github.com/cadadr/elisp#org-variable-pitchel][org-variable-pitch]]

(use-package org-variable-pitch)
(add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)

;; ** [[https://github.com/alphapapa/org-ql][org-ql]]

(use-package org-ql)

;; ** [[][zk]]

(use-package zk
  :custom
  (zk-directory (concat user-docs-dir "zk/"))
  (zk-file-extension "org")
  (zk-enable-link-buttons nil)
  :config
  (zk-setup-auto-link-buttons)
  (zk-setup-embark))

(defun zk-org-try-to-follow-link (fn &optional arg)
  "When 'org-open-at-point' FN fails, try 'zk-follow-link-at-point'.
Optional ARG."
  (let ((org-link-search-must-match-exact-headline t))
    (condition-case nil
	(apply fn arg)
      (error (zk-follow-link-at-point)))))

(advice-add 'org-open-at-point :around #'zk-org-try-to-follow-link)

;; ** [[][org-download]]

(use-package org-download)

;; ** [[][el2org]]

(use-package el2org)

;;
;; [[https://github.com.com/larstvel/ox-gfm][ox-gfm]]

(use-package ox-gfm)

;; ** [[https://github.com/alphapapa/outorg][outorg]]

(use-package outorg)

(define-key outorg-edit-minor-mode-map (kbd "C-c o") #'outorg-transient)
(global-set-key (kbd "C-c o") 'outorg-edit-as-org)

(transient-define-prefix outorg-transient ()
  "Outorg options."
  [("t" "Save to temp" (lambda () (interactive) (outorg-save-edits-to-tmp-file)))
   ("s" "Outorg save to buffer" (lambda () (interactive) (outorg-copy-edits-and-exit)))])

;; ** [[https://github.com/nobiot/org-remark][org-remark]]

(use-package org-remark
  :config
  (setq org-remark-notes-file-name 'org-remark-notes-file-name-function)
  :bind
  ("C-c n" . org-remark-transient))

(defun org-remark-new (beg end &optional id mode POINT)
  "Create new org-remark mark and open it."
  (interactive (org-remark-region-or-word))
  (org-remark-mark beg end id mode)
  (org-remark-open beg end)
  (switch-to-buffer-other-window "\*marginal notes\*"))

(transient-define-prefix org-remark-transient ()
  "Org-remark options."
  [("m" "Mark for org-remark" org-remark-mark)
   ("n" "Mark and open new remark" org-remark-new)
   ("o" "Open org-remark" org-remark-open)
   ("]" "View next remark" org-remark-view-next)
   ("[" "View prevoius remark" org-remark-view-prev)
   ("r" "Remove mark" org-remark-remove)
   ("D" "Delete mark and notes" org-remark-delete)])

(require 'org-remark-global-tracking)
(org-remark-global-tracking-mode +1)

;; ** [[https://github.com/alphapapa/org-sticky-header][org-sticky-header]]

(use-package org-sticky-header)
(add-hook 'org-mode-hook 'org-sticky-header-mode)
(add-hook 'outshine-mode-hook 'org-sticky-header-mode)

;; ** Org pretty entities 

(setq org-pretty-entities t)

;; ** Inline Images

(setq org-startup-with-inline-images t
      org-image-actual-width '(600))

;; ** org src blocks

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; ** Org eLisp

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))
(defun org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content))
           (message clipboard-url))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))
(define-key global-map (kbd "C-c l") 'org-insert-link-dwim)


;; * Agenda


(define-key global-map (kbd "C-c a") 'org-agenda)

;; ** Custom functions

(defun mark-as-done-and-archive ()
  "Mark the state of an org-mode item as DONE, archive it, and
save the Org buffers."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree)
  (org-save-all-org-buffers))

(defun schedule-today ()
  "Tag current item as `daily'."
  (interactive)
  (org-add-tag "daily")
  (save-buffer))
(defun open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

;; TODO setup agenda transient.
;; (define-key global-map (kbd "C-c a") 'agenda-transient)
(define-key global-map (kbd "C-c i") 'open-index-file)

;; ** TODO settings

(setq org-enforce-todo-dependencies t)
(setq org-log-done 'time)

;; ** Agenda settings

(setq org-agenda-start-on-weekday nil)
(setq org-deadline-warning-days 0)

;; ** Agenda org-capture-templates

(add-to-list 'org-capture-templates
             '("t" "Task"
               entry
               (file+headline org-index-file "Inbox")
               "* TODO %?\n"))
;; ** Agenda PATH

(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
      (concat
       (org-file-path (format "archive/archive-%s.org" (format-time-string "%Y")))
       "::* From %s"))
(setq org-agenda-files (list org-index-file
                             (org-file-path "calendars")
                             (org-file-path "events.org")
                             (org-file-path "goals.org")
                             (org-file-path "habits.org")
                             (org-file-path "recurring-events.org")
                             (org-file-path "recurring-tasks.org")))


;; * PDF's

;; ** [[https://github.com/politza/pdf-tools][pdf-tools]]

(use-package pdf-tools)

;; ** [[https://github.com/007kevin/pdf-view-restore][pdf-view-restore]]

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))


;; * Web


;; ** [[https://github.com/alphapapa/ement.el][ement]]

;; Install `plz' HTTP library (not on MELPA yet).
(use-package plz
  :straight (plz :host github :repo "alphapapa/plz.el"))

;; ;; Install Ement.
;; (use-package ement
;;   :straight (ement :host github :repo "alphapapa/ement.el"))

;; ** [[https://github.com/akirakyle/emacs-webkit][emacs-webkit]]

(straight-use-package
 '(webkit :type git :host github :repo "akirakyle/emacs-webkit"
          :branch "main"
          :files (:defaults "*.js" "*.css" "*.so")
          :pre-build ("make")))

(require 'webkit) 
(global-set-key (kbd "s-b") 'webkit) ;; Bind to whatever global key binding you want if you want
(require 'webkit-ace) ;; If you want link hinting
(require 'webkit-dark) ;; If you want to use the simple dark mode

;; If you want history saved in a different place or
;; Set to `nil' to if you don't want history saved to file (will stay in memory)
(setq webkit-history-file (concat user-docs-dir "/webkit/webkit-history")) 

;; If you want cookies saved in a different place or
;; Set to `nil' to if you don't want cookies saved
(setq webkit-cookie-file (concat user-docs-dir "/webkit/cookies"))

;; Set webkit as the default browse-url browser
(setq browse-url-browser-function 'webkit-browse-url)

;; Force webkit to always open a new session instead of reusing a current one
(setq webkit-browse-url-force-new t)

;; Globally disable javascript
(add-hook 'webkit-new-hook #'webkit-enable-javascript)

;; ;; Override the "loading:" mode line indicator with an icon from `all-the-icons.el'
;; ;; You could also use a unicode icon like ↺
;; (defun webkit--display-progress (progress)
;;   (setq webkit--progress-formatted
;;         (if (equal progress 100.0)
;;             ""
;;           (format "%s%.0f%%  " (all-the-icons-faicon "spinner") progress)))
;;   (force-mode-line-update))

;; Set action to be taken on a download request. Predefined actions are
;; `webkit-download-default', `webkit-download-save', and `webkit-download-open'
;; where the save function saves to the download directory, the open function
;; opens in a temp buffer and the default function interactively prompts.
(setq webkit-download-action-alist '(("\\.pdf\\'" . webkit-download-open)
                                     ("\\.png\\'" . webkit-download-save)
                                     (".*" . webkit-download-default)))

;; Globally use a proxy
;; (add-hook 'webkit-new-hook (lambda () (webkit-set-proxy "socks://localhost:8000")))

;; Globally use the simple dark mode
(setq webkit-dark-mode t)

;; ** [[https://github.com/mrmekon/snitch-el][snitch]]

;; (use-package snitch
;;   :config
;;   (setq snitch-network-policy 'allow
;;         snitch-process-policy 'allow
;;         snitch-log-policy '(allowed blocked whitelisted blacklisted)
;;         snitch-log-verbose t)
;;   (snitch-mode +1))


;; ** [[https://github.com/hrs/engine-mode][engine-mode]]

(use-package engine-mode)

(defengine duckduckgo
           "https://duckduckgo.com/?q=%s"
           :keybinding "d")

(defengine emacs-stackexchange
           "https://emacs.stackexchange.com/search?q="
           :keybinding "e")

(defengine github
           "https://github.com/search?ref=simplesearch&q=%s"
           :keybinding "g")

(defengine stack-overflow
           "https://stackoverflow.com/search?q=%s"
           :keybinding "s")

(defengine wikipedia
           "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
           :keybinding "w")

(defengine youtube
           "https://www.youtube.com/results?search_query=%s"
           :browser 'browse-url-firefox
           :keybinding "y")

(engine-mode t)


;; * Version Control

;; ** [[https://magit.vc/][magit]]

(use-package magit)
(setq magit-view-git-manual-method 'man)


;; * Dired

;; ** Default dir

(setq default-directory user-home-dir)

;; ** [[https://github.com/purcell/diredfl][diredfl]]

(use-package diredfl)
(diredfl-global-mode)

;; ** [[https://github.com/mattiasb/dired-hide-dotfiles][dired-hide-dotfiles]]

(use-package dired-hide-dotfiles)

(defun my-dired-mode-hook ()
  "My `dired' mode hook."
  ;; To hide dot-files by default
  (dired-hide-dotfiles-mode))

;; To toggle hiding
(define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
(add-hook 'dired-mode-hook #'my-dired-mode-hook)

;; ** dired don't open tons of buffers

(setq dired-kill-when-opening-new-dired-buffer t)

;; ** [[https://github.com/Fuco1/dired-hacks][dired-open]]

(use-package dired-open
  :config
  (setq dired-open-extensions
        '(("avi" . "vlc"))))

;; ** dired dwim files

(setq dired-dwim-target t)

;; ** kill buffers when deleted in dired

(setq dired-clean-up-buffers-too t)

;; ** dired deletion settings

(setq dired-recursive-copies 'always)

(setq dired-recursive-deletes 'top)

;; ** [[https://github.com/jwiegley/emacs-async][dired-async]]

(use-package async
  :init
  (dired-async-mode 1))
;; ** Always follow symlinks.

(setq vc-follow-symlinks t)

;; ** Use human readable sizes

(setq-default dired-listing-switches "-alh")



;; * presentation


;; ** [[https://gitlab.com/protesilaos/logos][logos]]

(use-package logos
  :init
  (setq-default logos-hide-mode-line nil
              logos-scroll-lock nil
              logos-variable-pitch nil
              logos-indicate-buffer-boundaries nil
              logos-buffer-read-only nil
              logos-olivetti nil)
  :bind
  ([remap narrow-to-region] . logos-narrow-dwim)
  ([remap forward-page] . logos-forward-page-dwim)
  ([remap backward-page] . logos-backward-page-dwim)
  ("<f9>".  logos-focus-mode))


;; * Frames

;; TODO setup stuff like [[https://github.com/alphapapa/frame-purpose.el][frame-purpose]] and my emacs launcher and eshell-term here
;; 
;;; init.el ends here
