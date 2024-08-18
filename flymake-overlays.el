;;; flymake-overlays.el --- toggleable overlays for Flymake diagnostics   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/flymake-overlays.el
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

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

;; This package provides a minor mode `flymake-overlays-mode' that
;; enhances Flymake diagnostics by proving toggleable overlays below
;; the affected lines.

;;; Code:

(require 'flymake)

(defgroup flymake-overlays nil
  "Flymake diagnostics with toggleable overlays."
  :group 'flymake
  :prefix "flymake-overlays-")

(defface flymake-overlays-face
  '((t :inherit shr-mark :extend t))
  "Face for flymake-overlays overlay text.")

(defvar-local flymake-overlays--overlays nil
  "List of diagnostic overlays.")

(defcustom flymake-overlays-fontify-text-function #'flymake-overlays--fontify-text
  "Function to fontify diagnostic text.
Accepts one variable, the text to fontify, and returns the fontified
text."
  :type 'function)

(defun flymake-overlays--fontify-text (text)
  "Fontify TEXT according to current major mode."
  (let ((mode major-mode))
    (with-temp-buffer
      (delay-mode-hooks (funcall mode))
      (insert text)
      (font-lock-ensure)
      (buffer-string))))

(defun flymake-overlays--format-diagnostic (text)
  "Format diagnostic TEXT for display."
  (setq text (funcall flymake-overlays-fontify-text-function text))
  (font-lock-append-text-property 0 (length text)
                                  'face 'flymake-overlays-face
                                  text)
  (concat text "\n"))

(defun flymake-overlays--create-or-update-overlay (beg end text &optional ov)
  "Create or update overlay from BEG to END with TEXT.
If OV is provided, update it instead of creating a new one."
  (let ((overlay (or ov (make-overlay beg end nil t nil))))
    (overlay-put overlay 'flymake-overlays-text
                 (flymake-overlays--format-diagnostic text))
    (if ov
        (when (overlay-get ov 'after-string)
          (overlay-put overlay 'after-string
                       (overlay-get overlay
                                    'flymake-overlays-text)))
      (overlay-put overlay 'after-string nil)  ; Start invisible
      (push overlay flymake-overlays--overlays))
    overlay))

(defun flymake-overlays--handle-report (&rest _)
  "Update our overlays after Flymake reports diagnostics."
  (let ((flymake-diagnostics (flymake-diagnostics)))
    ;; Remove overlays that don't match current Flymake diagnostics
    (setq flymake-overlays--overlays
          (seq-remove
           (lambda (ov)
             (unless (seq-find (lambda (diag)
                                 (= (overlay-start ov)
                                    (flymake-diagnostic-beg diag)))
                               flymake-diagnostics)
               (delete-overlay ov)
               t))
           flymake-overlays--overlays))

    ;; Update existing overlays, create new ones
    (dolist (diag flymake-diagnostics)
      (let* ((beg (flymake-diagnostic-beg diag))
             (end (save-excursion
                    (goto-char (flymake-diagnostic-end diag))
                    (line-beginning-position 2)))
             (text (flymake-diagnostic-text diag))
             (existing-ov (cl-find-if (lambda (ov)
                                        (= (overlay-start ov) beg))
                                      flymake-overlays--overlays)))
        (if existing-ov
            (flymake-overlays--create-or-update-overlay beg end text existing-ov)
          (flymake-overlays--create-or-update-overlay beg end text))))))

(defun flymake-overlays-toggle-at-point ()
  "Toggle the diagnostic overlay at point."
  (interactive)
  (cl-some (lambda (ov)
             (when-let (text (overlay-get ov 'flymake-overlays-text))
               (if (overlay-get ov 'after-string)
                   (overlay-put ov 'after-string nil)
                 (overlay-put ov 'after-string text))
               t))
           (overlays-at (point))))

(defun flymake-overlays-smart-toggle ()
  "Smart toggle for flymake overlays.
If point is within an overlay, toggle that overlay.  Otherwise, toggle
visibility of all overlays in the buffer."
  (interactive)
  (if-let ((ov (seq-find (lambda (o) (overlay-get o 'flymake-overlays-text))
                         (overlays-at (point)))))
      ;; Toggle the overlay at point
      (if (overlay-get ov 'after-string)
          (overlay-put ov 'after-string nil)
        (overlay-put ov 'after-string (overlay-get ov 'flymake-overlays-text)))
    ;; Toggle all overlays
    (if (seq-some (lambda (o) (overlay-get o 'after-string))
                  flymake-overlays--overlays)
        ;; If any overlay is visible, hide all
        (dolist (o flymake-overlays--overlays)
          (overlay-put o 'after-string nil))
      ;; Otherwise, show all
      (dolist (o flymake-overlays--overlays)
        (overlay-put o 'after-string (overlay-get o 'flymake-overlays-text))))))

(defun flymake-overlays--clear-overlays ()
  "Clear all diagnostic overlays."
  (mapc #'delete-overlay flymake-overlays--overlays)
  (setq flymake-overlays--overlays nil))

;;;###autoload
(define-minor-mode flymake-overlays-mode
  "Toggle Flymake overlay diagnostics mode."
  :lighter " FlyOv"
  :global nil
  (if flymake-overlays-mode
      (progn
        (setq flymake-overlays--overlays nil)
        (advice-add 'flymake--handle-report :after #'flymake-overlays--handle-report)
        (flymake-overlays--handle-report))
    (advice-remove 'flymake--handle-report #'flymake-overlays--handle-report)
    (flymake-overlays--clear-overlays)))

(provide 'flymake-overlays)
;;; flymake-overlays.el ends here
