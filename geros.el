;;; geros.el --- Evaluation Result OverlayS for Geiser   -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Tianxiang Xiong

;; Author: Tianxiang Xiong <tianxiang.xiong@gmail.com>
;; Keywords: convenience, lisp
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/nitros12/geros
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Evaluation result overlays for Geiser.

;; The code is mostly taken from CIDER.  For more about CIDER, see:
;; https://github.com/clojure-emacs/cider

;;; Code:

(require 'cl-lib)
(require 'geiser)

;; Customize

(defgroup geros nil
  "Evaluation Result OverlayS for Geiser"
  :prefix "geros-"
  :group 'lisp)

;; (defcustom geros-eval-result-prefix "=> "
;;   "The prefix displayed in the minibuffer before a result value."
;;   :group 'geros
;;   :type 'string
;;   :package-version '(geros "0.1.0"))

(defface geros-result-overlay-face
  '((((class color) (background light))
     :background "grey90" :box (:line-width -1 :color "yellow"))
    (((class color) (background dark))
     :background "grey10" :box (:line-width -1 :color "black")))
  "Face used to display evaluation results at the end of line.
If `geros-overlays-use-font-lock' is non-nil, this face is applied
with lower priority than the syntax highlighting."
  :group 'geros
  :package-version '(geros "0.1.0"))

(defcustom geros-overlays-use-font-lock t
  "If non-nil, results overlays are font-locked as Clojure code.
If nil, apply `geros-result-overlay-face' to the entire overlay instead of
font-locking it."
  :group 'geros
  :type 'boolean
  :package-version '(geros "0.1.0"))

(defcustom geros-eval-result-duration 'command
  "Duration, in seconds, of eval-result overlays.

If nil, overlays last indefinitely.

If the symbol `command', they're erased before the next command."
  :group 'geros
  :type '(choice (integer :tag "Duration in seconds")
                 (const :tag "Until next command" command)
                 (const :tag "Last indefinitely" nil))
  :package-version '(geros "0.1.0"))


;; Overlay

(defun geros--make-overlay (l r type &rest props)
  "Place an overlay between L and R and return it.

TYPE is a symbol put on the overlay's category property.  It is
used to easily remove all overlays from a region with:

    (remove-overlays start end 'category TYPE)

PROPS is a plist of properties and values to add to the overlay."
  (let ((o (make-overlay l (or r l) (current-buffer))))
    (overlay-put o 'category type)
    (overlay-put o 'geros-temporary t)
    (while props (overlay-put o (pop props) (pop props)))
    (push #'geros--delete-overlay (overlay-get o 'modification-hooks))
    o))

(defun geros--delete-overlay (ov &rest _)
  "Safely delete overlay OV.

Never throws errors, and can be used in an overlay's
modification-hooks."
  (ignore-errors (delete-overlay ov)))

(cl-defun geros--make-result-overlay (value &rest props &key where duration (type 'result)
                                            (format " %s ")
                                            (prepend-face 'geros-result-overlay-face)
                                            &allow-other-keys)
  "Place an overlay displaying VALUE at the end of line.

VALUE is used as the overlay's after-string property, meaning it
is displayed at the end of the overlay.  The overlay itself is
placed from beginning to end of current line.

Return nil if the overlay was not placed or if it might not be
visible, and return the overlay otherwise.

Return the overlay if it was placed successfully, and nil if it
failed.

This function takes some optional keyword arguments:

- If WHERE is a number or a marker, apply the overlay over the
  entire line at that place (defaulting to `point').  If it is a
  cons cell, the car and cdr determine the start and end of the
  overlay.

- DURATION takes the same possible values as the
  `geros-eval-result-duration' variable.

- TYPE is passed to `geros--make-overlay' (defaults to `result').

- FORMAT is a string passed to `format'.  It should have exactly
  one %s construct (for VALUE).

All arguments beyond these (PROPS) are properties to be used on
the overlay."
  (declare (indent 1))
  (while (keywordp (car props))
    (setq props (cddr props)))
  ;; If the marker points to a dead buffer, don't do anything.
  (let ((buffer (cond
                 ((markerp where) (marker-buffer where))
                 ((markerp (car-safe where)) (marker-buffer (car where)))
                 (t (current-buffer)))))
    (with-current-buffer buffer
      (save-excursion
        (when (number-or-marker-p where)
          (goto-char where))
        ;; Make sure the overlay is actually at the end of the sexp.
        (skip-chars-backward "\r\n[:blank:]")
        (let* ((beg (if (consp where)
                        (car where)
                      (save-excursion
                        (backward-sexp 1)
                        (point))))
               (end (if (consp where)
                        (cdr where)
                      (line-end-position)))
               (display-string (format format value))
               (o nil))
          (remove-overlays beg end 'category type)
          (funcall (if geros-overlays-use-font-lock
                       #'font-lock-prepend-text-property
                     #'put-text-property)
                   0 (length display-string)
                   'face prepend-face
                   display-string)
          ;; If the display spans multiple lines or is very long, display it at
          ;; the beginning of the next line.
          (when (or (string-match "\n." display-string)
                    (> (string-width display-string)
                       (- (window-width) (current-column))))
            (setq display-string (concat " \n" display-string)))
          ;; Put the cursor property only once we're done manipulating the
          ;; string, since we want it to be at the first char.
          (put-text-property 0 1 'cursor 0 display-string)
          (when (> (string-width display-string) (* 3 (window-width)))
            (setq display-string
                  (concat (substring display-string 0 (* 3 (window-width)))
                          "...\nResult truncated.")))
          ;; Create the result overlay.
          (setq o (apply #'geros--make-overlay
                         beg end type
                         'after-string display-string
                         props))
          (pcase duration
            ((pred numberp) (run-at-time duration nil #'geros--delete-overlay o))
            (`command (if this-command
                          (add-hook 'pre-command-hook
                                    #'geros--remove-result-overlay
                                    nil 'local)
                        (geros--remove-result-overlay))))
          (let ((win (get-buffer-window buffer)))
            ;; Left edge is visible.
            (when (and win
                       (<= (window-start win) (point))
                       ;; In 24.3 `<=' is still a binary predicate.
                       (<= (point) (window-end win))
                       ;; Right edge is visible. This is a little conservative
                       ;; if the overlay contains line breaks.
                       (or (< (+ (current-column) (string-width value))
                              (window-width win))
                           (not truncate-lines)))
              o)))))))

(defun geros--remove-result-overlay ()
  "Remove result overlay from current buffer.

This function also removes itself from `pre-command-hook'."
  (remove-hook 'pre-command-hook #'geros--remove-result-overlay 'local)
  (remove-overlays nil nil 'category 'result))

(defun geros--eval-overlay (value point)
  "Make overlay for VALUE at POINT."
  (geros--make-result-overlay (format "%s" value)
    :where point
    :duration geros-eval-result-duration)
  value)


;; API

(defun geros-eval-last-sexp (print-to-buffer-p)
  "Wrapper for `eval-last-sexp' that overlays results."
  (interactive "P")
  (geros--eval-overlay
   (geiser-eval-last-sexp print-to-buffer-p)
   (point)))

;; Minor mode

;;;###autoload
(define-minor-mode geros-mode
  "Display Geiser evaluation results overlays."
  :global t
  (if geros-mode
      (progn
        (global-set-key [remap geiser-eval-last-sexp] #'geros-eval-last-sexp))
    (global-set-key [remap geiser-eval-last-sexp] nil)))


(provide 'geros)
;;; geros.el ends here
