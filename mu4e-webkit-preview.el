;;; mu4e-webkit-preview.el --- Read mu4e HTML mail with WebKit -*- lexical-binding: t; -*-

;; Author: Dzming Li
;; URL: https://github.com/DzmingLi/mu4e-webkit-preview
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mu4e "1.14") (webkit "0"))
;; Keywords: mail, hypermedia
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Enable `mu4e-webkit-preview-mode' to replace graphical HTML message views
;; with akirakyle/emacs-webkit.  Plain text stays in the native mu4e reader.
;; Each browser owns its hidden mu4e view; closing either retires both, so
;; asynchronous mail updates cannot reopen a dismissed message.
;; This package has no dependency on a window manager or attachment UI.

;;; Code:

(require 'cl-lib)
(require 'mu4e)
(require 'seq)
(require 'url-util)

(declare-function webkit-new "webkit" (&optional url buffer-name noquery))
(declare-function webkit--load-uri "webkit-module" (id uri))
(defvar webkit--id)
(defvar webkit-own-window)

(defgroup mu4e-webkit-preview nil
  "Embedded WebKit rendering for mu4e messages."
  :group 'mu4e)

(defcustom mu4e-webkit-preview-created-functions nil
  "Functions called with SOURCE, PREVIEW and WINDOW after replacing a view.
Use this hook to transfer an optional attachment pane to PREVIEW."
  :type 'hook)

(defcustom mu4e-webkit-preview-close-hook nil
  "Hook run in the preview buffer before its windows and source are closed."
  :type 'hook)

(defvar mu4e-webkit-preview--buffers nil)
(defvar-local mu4e-webkit-preview--source nil)
(defvar-local mu4e-webkit-preview--headers nil)
(defvar-local mu4e-webkit-preview--docid nil)
(defvar-local mu4e-webkit-preview--preview nil)
(defvar-local mu4e-webkit-preview--export-directory nil)
(defvar-local mu4e-webkit-preview-mail-mode nil)

(defun mu4e-webkit-preview--new-view (original uri)
  "Follow URI in this mail preview, or call ORIGINAL for other browsers."
  (if mu4e-webkit-preview-mail-mode
      (webkit--load-uri webkit--id uri)
    (funcall original uri)))

(defun mu4e-webkit-preview--source-killed ()
  "Retire the browser when mu4e replaces or kills its native view."
  (let ((preview mu4e-webkit-preview--preview))
    (setq mu4e-webkit-preview--preview nil)
    (when (buffer-live-p preview)
      (with-current-buffer preview
        ;; The source is already being killed.  Do not kill it recursively.
        (setq mu4e-webkit-preview--source nil)
        (kill-buffer preview)))))

(defun mu4e-webkit-preview--close ()
  "Retire this preview, its hidden mu4e view, windows and exported HTML."
  (let ((preview (current-buffer))
        (source mu4e-webkit-preview--source)
        (headers mu4e-webkit-preview--headers)
        (directory mu4e-webkit-preview--export-directory))
    (setq mu4e-webkit-preview--source nil)
    (run-hooks 'mu4e-webkit-preview-close-hook)
    (when (buffer-live-p source)
      (with-current-buffer source
        (setq mu4e-webkit-preview--preview nil)
        (remove-hook 'kill-buffer-hook #'mu4e-webkit-preview--source-killed t))
      (kill-buffer source))
    (dolist (window (get-buffer-window-list preview nil t))
      (if (one-window-p t (window-frame window))
          (set-window-buffer window (if (buffer-live-p headers) headers
                                      (other-buffer preview t)))
        (delete-window window)))
    (dolist (frame (frame-list))
      (dolist (window (window-list frame 'no-minibuffer))
        (unrecord-window-buffer window preview)))
    (when (and directory (file-directory-p directory))
      (delete-directory directory t))
    (setq mu4e-webkit-preview--buffers
          (delq preview mu4e-webkit-preview--buffers))
    (unless mu4e-webkit-preview--buffers
      (remove-hook 'mu4e-mark-execute-pre-hook #'mu4e-webkit-preview--before-execute)
      (advice-remove 'webkit--callback-new-view #'mu4e-webkit-preview--new-view))))

(defun mu4e-webkit-preview--before-execute (mark message)
  "Close the matching preview before a confirmed destructive MARK on MESSAGE.
This runs after mu4e's confirmation, before sending the action to its server.
Retiring the native view prevents a subsequent move update from reopening it."
  (when (memq mark '(trash refile move delete))
    (let ((docid (mu4e-message-field message :docid)))
      (dolist (preview (copy-sequence mu4e-webkit-preview--buffers))
        (when (and (buffer-live-p preview)
                   (equal docid (buffer-local-value
                                 'mu4e-webkit-preview--docid preview)))
          (kill-buffer preview))))))

(defun mu4e-webkit-preview--command (command)
  "Run COMMAND against the native view belonging to this preview."
  (let ((source mu4e-webkit-preview--source)
        (docid mu4e-webkit-preview--docid))
    (unless (and (buffer-live-p source) docid)
      (user-error "This preview no longer has a live message"))
    (with-current-buffer source
      (unless (equal (mu4e-message-field (mu4e-message-at-point t) :docid) docid)
        (user-error "The message behind this preview has changed"))
      (call-interactively command))))

(defun mu4e-webkit-preview-trash ()
  "Mark this message for moving to Trash."
  (interactive)
  (mu4e-webkit-preview--command #'mu4e-headers-mark-for-trash))

(defun mu4e-webkit-preview-refile ()
  "Mark this message for refiling."
  (interactive)
  (mu4e-webkit-preview--command #'mu4e-headers-mark-for-refile))

(defun mu4e-webkit-preview-execute ()
  "Execute marks in the associated headers buffer, with mu4e confirmation."
  (interactive)
  (unless (buffer-live-p mu4e-webkit-preview--headers)
    (user-error "This preview no longer has a live headers buffer"))
  (with-current-buffer mu4e-webkit-preview--headers
    (call-interactively #'mu4e-mark-execute-all)))

(defun mu4e-webkit-preview-quit ()
  "Close this preview and its native view, returning to the headers."
  (interactive)
  (kill-buffer (current-buffer)))

(defvar mu4e-webkit-preview-mail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'mu4e-webkit-preview-trash)
    (define-key map "r" #'mu4e-webkit-preview-refile)
    (define-key map "x" #'mu4e-webkit-preview-execute)
    (define-key map "q" #'mu4e-webkit-preview-quit)
    map)
  "Mail commands in a WebKit preview; marking follows native mu4e keys.")

(define-minor-mode mu4e-webkit-preview-mail-mode
  "Provide mail commands in a WebKit message preview."
  :lighter " Mail"
  :keymap mu4e-webkit-preview-mail-mode-map
  :group 'mu4e-webkit-preview)

;;;###autoload
(defun mu4e-webkit-preview-open (&optional message)
  "Export MESSAGE's CID-resolved HTML and replace its visible native view.
Interactively, use the message in the current mu4e view buffer."
  (interactive)
  (unless (and (derived-mode-p 'mu4e-view-mode)
               (display-graphic-p) (eq (window-buffer) (current-buffer)))
    (user-error "Select a graphical mu4e message view first"))
  (require 'webkit)
  (let* ((message (or message (mu4e-message-at-point)))
         (source (current-buffer))
         (window (selected-window))
         (headers (mu4e-get-headers-buffer))
         (directory (make-temp-file "mu4e-webkit-preview-" t))
         (file (expand-file-name "index.html" directory))
         (coding-system-for-write 'utf-8-unix)
         (webkit-own-window nil)
         preview)
    (condition-case err
        (progn
          (write-region (mu4e-view-message-html message) nil file nil 'silent)
          (setq preview (webkit-new (concat "file://" (url-encode-url file))
                                    "*mu4e-webkit-preview*" t))
          (with-current-buffer preview
            (setq mu4e-webkit-preview--source source
                  mu4e-webkit-preview--headers headers
                  mu4e-webkit-preview--docid (mu4e-message-field message :docid)
                  mu4e-webkit-preview--export-directory directory)
            (mu4e-webkit-preview-mail-mode 1)
            (add-hook 'kill-buffer-hook #'mu4e-webkit-preview--close nil t))
          (with-current-buffer source
            (setq mu4e-webkit-preview--preview preview)
            (add-hook 'kill-buffer-hook #'mu4e-webkit-preview--source-killed nil t))
          (push preview mu4e-webkit-preview--buffers)
          (add-hook 'mu4e-mark-execute-pre-hook #'mu4e-webkit-preview--before-execute)
          (advice-add 'webkit--callback-new-view :around #'mu4e-webkit-preview--new-view)
          (unrecord-window-buffer window source)
          (bury-buffer source)
          (run-hook-with-args 'mu4e-webkit-preview-created-functions source preview window)
          preview)
      (error
       ;; Preserve the native reader if creating the browser fails.
       (when (buffer-live-p preview)
         (with-current-buffer preview
           (setq mu4e-webkit-preview--source nil))
         (kill-buffer preview))
       (when (file-directory-p directory) (delete-directory directory t))
       (when (and (window-live-p window) (buffer-live-p source))
         (set-window-buffer window source))
       (signal (car err) (cdr err))))))

(defun mu4e-webkit-preview--render ()
  "Automatically preview HTML, preserving the native rendering context."
  (save-current-buffer
    (when (and (display-graphic-p)
               (seq-some (lambda (part)
                           (equal (plist-get part :mime-type) "text/html"))
                         (mu4e-view-mime-parts)))
      (when-let* ((window (get-buffer-window (current-buffer) t)))
        (with-selected-window window
          (mu4e-webkit-preview-open))))))

;;;###autoload
(define-minor-mode mu4e-webkit-preview-mode
  "Automatically use embedded WebKit for graphical mu4e HTML messages."
  :global t
  :group 'mu4e-webkit-preview
  (if mu4e-webkit-preview-mode
      (add-hook 'mu4e-view-rendered-hook #'mu4e-webkit-preview--render t)
    (remove-hook 'mu4e-view-rendered-hook #'mu4e-webkit-preview--render)
    (mapc #'kill-buffer (seq-filter #'buffer-live-p
                                   (copy-sequence mu4e-webkit-preview--buffers)))))

(provide 'mu4e-webkit-preview)
;;; mu4e-webkit-preview.el ends here
