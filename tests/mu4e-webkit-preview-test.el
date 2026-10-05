;;; mu4e-webkit-preview-test.el --- Preview lifetime regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'mu4e-webkit-preview)
;; Only the GTK renderer is replaced.  Message identity, view detection and
;; mark execution use the installed mu4e implementation.
(provide 'webkit)

(defmacro mu4e-webkit-test--with-preview (&rest body)
  (declare (indent 0))
  `(save-window-excursion
     (delete-other-windows)
     (let* ((headers (generate-new-buffer " *test-headers*"))
            (source (generate-new-buffer " *test-source*"))
            (message '(:docid 42 :subject "Fixture"))
            (view (split-window-right))
            (mu4e-webkit-preview--buffers nil)
            (mu4e-webkit-preview-created-functions nil)
            (mu4e-webkit-preview-close-hook nil)
            (mu4e-mark-execute-pre-hook nil)
            preview export-directory)
       (unwind-protect
           (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                     ((symbol-function 'webkit-new)
                      (lambda (&rest _)
                        (let ((buffer (generate-new-buffer " *test-webkit*")))
                          (with-current-buffer buffer
                            (setq major-mode 'webkit-mode)
                            (setq-local webkit--id 99))
                          (switch-to-buffer buffer))))
                     ((symbol-function 'mu4e-view-message-html)
                      (lambda (_) "<html><body>中文邮件</body></html>")))
             (with-current-buffer headers
               (setq major-mode 'mu4e-headers-mode)
               (setq-local mu4e--mark-map (make-hash-table))
               (insert (propertize "Fixture\n" 'msg message))
               (goto-char (point-min)))
             (with-current-buffer source
               (setq major-mode 'mu4e-view-mode)
               (setq-local mu4e--view-message message
                           mu4e-linked-headers-buffer headers))
             (set-window-buffer (selected-window) headers)
             (set-window-buffer view source)
             (with-selected-window view
               (setq preview (mu4e-webkit-preview-open)))
             (setq export-directory
                   (buffer-local-value 'mu4e-webkit-preview--export-directory preview))
             ,@body)
         (dolist (buffer (list preview source headers))
           (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest mu4e-webkit-preview-export-declares-utf8-without-html-meta ()
  (mu4e-webkit-test--with-preview
    (let ((file (expand-file-name "index.html" export-directory)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (should (string-prefix-p (unibyte-string #xef #xbb #xbf)
                                 (buffer-string))))
      (with-temp-buffer
        (insert-file-contents file)
        (should (equal (buffer-string)
                       "<html><body>中文邮件</body></html>"))))))

(defun mu4e-webkit-test--delayed-move (headers message)
  "Replay the real mu4e move update handler without modifying a mail index."
  (with-current-buffer headers
    (cl-letf (((symbol-function 'mu4e~headers-docid-at-point) (lambda () 42))
              ((symbol-function 'mu4e~headers-docid-pos) (lambda (_) 1))
              ((symbol-function 'mu4e~headers-field-for-docid) (lambda (&rest _) nil))
              ((symbol-function 'mu4e~headers-remove-header) #'ignore)
              ((symbol-function 'mu4e~headers-goto-docid) (lambda (_) nil))
              ((symbol-function 'mu4e~headers-highlight) #'ignore)
              ((symbol-function 'mu4e-view)
               (lambda (_) (ert-fail "A dismissed message was reopened"))))
      (mu4e~headers-update-handler message t t))))

(ert-deftest mu4e-webkit-preview-quit-prevents-delayed-reopening ()
  (mu4e-webkit-test--with-preview
    (should (eq (window-buffer view) preview))
    (should (mu4e~headers-view-this-message-p 42))
    (with-current-buffer preview (mu4e-webkit-preview-quit))
    (should-not (buffer-live-p source))
    (should-not (buffer-live-p preview))
    (should-not (file-exists-p export-directory))
    (should (equal (mapcar #'window-buffer (window-list)) (list headers)))
    (mu4e-webkit-test--delayed-move headers message)))

(ert-deftest mu4e-webkit-preview-confirmed-move-closes-before-dispatch ()
  (mu4e-webkit-test--with-preview
    (let* ((dispatched nil)
           (mu4e-marks `((trash :action
                              ,(lambda (&rest _)
                                 (should-not (buffer-live-p preview))
                                 (should-not (buffer-live-p source))
                                 (setq dispatched t))))))
      (with-current-buffer headers (puthash 42 '(trash . "/Trash") mu4e--mark-map))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'mu4e~headers-goto-docid) (lambda (_) t))
                ((symbol-function 'mu4e-mark-resolve-deferred-marks) #'ignore)
                ((symbol-function 'mu4e-mark-unmark-all)
                 (lambda (&rest _) (clrhash mu4e--mark-map))))
        (with-current-buffer preview (mu4e-webkit-preview-execute)))
      (should dispatched)
      (should-not mu4e-webkit-preview--buffers)
      (mu4e-webkit-test--delayed-move headers message))))

(ert-deftest mu4e-webkit-preview-cancelled-execution-keeps-reader ()
  (mu4e-webkit-test--with-preview
    (with-current-buffer headers (puthash 42 '(trash . "/Trash") mu4e--mark-map))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
              ((symbol-function 'mu4e-mark-resolve-deferred-marks) #'ignore)
              ((symbol-function 'mu4e-mark-unmark-all) #'ignore))
      (with-current-buffer preview (mu4e-webkit-preview-execute)))
    (should (buffer-live-p preview))
    (should (buffer-live-p source))))

(ert-deftest mu4e-webkit-preview-source-replacement-retires-browser ()
  (mu4e-webkit-test--with-preview
    (kill-buffer source)
    (should-not (buffer-live-p preview))
    (should-not (file-exists-p export-directory))
    (should-not mu4e-webkit-preview--buffers)))

(ert-deftest mu4e-webkit-preview-unrelated-actions-preserve-reader ()
  (mu4e-webkit-test--with-preview
    (mu4e-webkit-preview--before-execute 'trash '(:docid 43))
    (mu4e-webkit-preview--before-execute 'flag message)
    (should (buffer-live-p preview))))

(ert-deftest mu4e-webkit-preview-all-removal-marks-retire-the-pair ()
  (dolist (mark '(trash refile move delete))
    (mu4e-webkit-test--with-preview
      (mu4e-webkit-preview--before-execute mark message)
      (should-not (buffer-live-p preview))
      (should-not (buffer-live-p source)))))

(ert-deftest mu4e-webkit-preview-source-identity-is-checked ()
  (mu4e-webkit-test--with-preview
    (with-current-buffer source (setq mu4e--view-message '(:docid 43)))
    (with-current-buffer preview
      (should-error (mu4e-webkit-preview-trash) :type 'user-error))))

(ert-deftest mu4e-webkit-preview-single-window-returns-headers ()
  (mu4e-webkit-test--with-preview
    (delete-other-windows view)
    (with-current-buffer preview (mu4e-webkit-preview-quit))
    (should (eq (window-buffer) headers))
    (should-not (buffer-live-p source))))

(ert-deftest mu4e-webkit-preview-export-and-link-navigation ()
  (mu4e-webkit-test--with-preview
    (with-temp-buffer
      (insert-file-contents (expand-file-name "index.html" export-directory))
      (should (equal (buffer-string) "<html><body>中文邮件</body></html>")))
    (let (followed)
      (cl-letf (((symbol-function 'webkit--load-uri)
                 (lambda (id uri) (setq followed (list id uri)))))
        (with-current-buffer preview
          (mu4e-webkit-preview--new-view #'ignore "https://example.org/unsubscribe")))
      (should (equal followed '(99 "https://example.org/unsubscribe"))))))

(ert-deftest mu4e-webkit-preview-plain-text-keeps-native-reader ()
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
            ((symbol-function 'mu4e-view-mime-parts)
             (lambda () '((:mime-type "text/plain"))))
            ((symbol-function 'mu4e-webkit-preview-open)
             (lambda (&rest _) (ert-fail "Plain mail opened in WebKit"))))
    (mu4e-webkit-preview--render)))

(ert-deftest mu4e-webkit-preview-disable-closes-previews-and-removes-hooks ()
  (mu4e-webkit-test--with-preview
    (mu4e-webkit-preview-mode 1)
    (mu4e-webkit-preview-mode -1)
    (should-not (buffer-live-p preview))
    (should-not (buffer-live-p source))
    (should-not (memq #'mu4e-webkit-preview--render mu4e-view-rendered-hook))
    (should-not (memq #'mu4e-webkit-preview--before-execute mu4e-mark-execute-pre-hook))))
