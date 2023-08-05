;;; centuri.el --- Tweakable content centering in buffers  -*- lexical-binding: t; -*-
;;
;; Author: Henry Timur Domagalski <henrycodev@gmail.com>
;; Version: 1.0.0
;; Keywords: faces windows buffers convenience
;; URL: https://github.com/henryco/centuri-el
;; Package-Requires: ((emacs "28.1"))
;; Compatibility: GNU Emacs 28.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Enable centuri-mode and your text is going to be centered.
;; This is LOCAL mode, so the effect does not apply to other buffers.
;; See group "centuri"
;;
;; Customizable options are:
;;  centuri--min-size
;;  centuri--max-size
;;  centuri--single-window
;;  centuri--center-absolute
;;  centuri--buffers-ignored
;;  centuri--max-scale
;;  centuri--margin-left
;;  centuri--margin-right
;;  centuri--margin-left-factor
;;  centuri--margin-right-factor
;;
;;; Code:

(require 'cl-lib)

(defgroup centuri nil
  "Center text in windows."
  :group 'convenience
  :prefix "centuri-")

(defcustom centuri--lighter
  " CEN"
  "Mode's lighter used in the mode line"
  :group 'centuri
  :type 'string)

(defcustom centuri--min-size 80
  "Mininum windows size to be qualified for centering"
  :group 'centuri
  :type 'integer)

(defcustom centuri--max-size 100
  "Maximum window size after centering"
  :group 'centuri
  :type 'integer)

(defcustom centuri--single-window nil
  "Activate only if this is only window"
  :group 'centuri
  :type 'boolean)

(defcustom centuri--center-absolute t
  "Should center according to absolute or realtive position"
  :group 'centuri
  :type 'boolean)

(defcustom centuri--max-scale 0.75
  "Scale factor when max-size is not defined"
  :group 'centuri
  :type 'float)

(defcustom centuri--margin-left 0
  "Extra left margin added after centering"
  :group 'centuri
  :type 'integer)

(defcustom centuri--margin-right 0
  "Extra right margin added after centering"
  :group 'centuri
  :type 'integer)

(defcustom centuri--margin-left-factor 1.0
  "Extra left maring scale factor applied after centering"
  :group 'centuri
  :type 'float)

(defcustom centuri--margin-right-factor 1.0
  "Extra right maring scale factor applied after centering"
  :group 'centuri
  :type 'float)

(defcustom centuri--buffers-ignored '()
  "List of ignored buffers"
  :group 'centuri)

(defun centuri-positive-p (val)
  "Check if VAL is not nil and a positive number"
  (and val (numberp val) (> val 0)))

(defvar centuri--init-window nil)
(defvar centuri--init-margins nil)
(defvar centuri--init-buffer nil)

(defun centuri-horizontal-windows ()
  "Returns list of widnows splitted horizontally"
  ;; Looks like it works
  ;; (window-list)
  (seq-filter
   (lambda (w)
     (and (>= (window-top-line) (window-top-line w))
          (not (eq w (selected-window)))))
     (window-list)))

(defun centuri-window-names ()
  "Get buffer name of each window"
  (mapcar (lambda (window)
            (buffer-name (window-buffer window)))
          (centuri-horizontal-windows)))

(defun centuri-find-self ()
  "Find itslef windows by buffer name"
  (seq-filter
   (lambda (window)
     (with-selected-window window
       (string= (buffer-name (window-buffer window))
                centuri--init-buffer)))
   (window-list)))

(defun centuri-check-for-many ()
  "Check if there is more then one non-ingored window"
  (length> (cl-set-difference
            (centuri-window-names)
            centuri--buffers-ignored
            :test 'string=)
           0))

(defun centuri-calc-desired-width ()
  "Returns desired width if not nil, otherwise calculates it"
  (if (centuri-positive-p centuri--max-size)
      centuri--max-size
    (* centuri--max-scale (window-width))))

(defun centuri-calc-margin-left (margin)
  "Calculate left margin, for internal usage only"
  (let ((m (if centuri--margin-left centuri--margin-left 0))
        (s (if centuri--margin-left-factor centuri--margin-left-factor 1.0)))
    (if margin
        (truncate (* (+ margin m) s))
        nil)))

(defun centuri-calc-margin-right (margin)
  "Calculate right margin, for internal usage only"
  (let ((m (if centuri--margin-right centuri--margin-right 0))
        (s (if centuri--margin-right-factor centuri--margin-right-factor 1.0)))
    (if margin
        (truncate (* (+ margin m) s))
        nil)))

(defun centuri-center-relative ()
  "Centers window content using its own windows size"
  (let* ((desired (centuri-calc-desired-width))
         (margin (truncate (* 0.5 (- (window-width) desired)))))
    (set-window-margins (selected-window)
                        (centuri-calc-margin-left margin)
                        (centuri-calc-margin-right margin)
                        )))

(defun centuri-center-absolute ()
  "Centers window content using frame size"
  (let* ((desired (centuri-calc-desired-width))
         (frame-w (frame-width))
         (split-w (window-width))
         (padding (window-left-column))
         (right-p (+ padding split-w))
         (margin  (truncate (* 0.5 (- frame-w desired))))

         ;; I'm not sure about this part, so I'll rather leave it here commented
         ;; (left    (truncate (max 1 (- margin (* 0.5 padding)))))
         ;; (right   (truncate (max 0 (- margin (- frame-w (* 0.5 right-p))))))
         
         (left    (truncate (max 1 (- margin padding))))
         (right   (truncate (max 0 (- margin (- frame-w right-p))))))
    (when (> margin 0)
      (set-window-margins (selected-window)
                          (centuri-calc-margin-left left)
                          (centuri-calc-margin-right right)
                          ))))

(defun centuri-center-w ()
  "Centerize current widnow, only for internal useage"
  (when (and centuri-mode
             centuri--init-buffer
             centuri--init-window
             (eq (buffer-name (window-buffer (selected-window)))
                 centuri--init-buffer)
             (>= (window-width) centuri--min-size)
             (>= (window-width) centuri--max-size))    
    (if centuri--center-absolute
      (centuri-center-absolute)
      (centuri-center-relative))))

(defun centuri-restore-w ()
  "Restore window parameters"  
  (when (and centuri--init-buffer
             centuri--init-window
             (eq (buffer-name (window-buffer (selected-window)))
                 centuri--init-buffer))
    (set-window-margins
       (selected-window)
       (car centuri--init-margins)
       (cdr centuri--init-margins))))

(defun centuri--hook-update (&optional arg)
  "Hook function for buffer/window update events (but cooler)"
  (let* ((self (centuri-find-self)))
    (dolist (w self)
      (with-selected-window w
        (centuri-restore-w)
        (centuri-center-w)))))

(defun centuri-enable ()
  "Private function for internal usage only"
  (when (not centuri-mode)
    (error "centuri-mode disabled"))
  
  (make-local-variable 'centuri--init-window)
  (make-local-variable 'centuri--init-margins)
  (make-local-variable 'centuri--init-buffer)
  
  (make-local-variable 'centuri--margin-left)
  (make-local-variable 'centuri--margin-right)
  (make-local-variable 'centuri--margin-left-factor)
  (make-local-variable 'centuri--margin-right-factor)
  
  (setq centuri--init-margins (window-margins))
  (setq centuri--init-window (selected-window))
  (setq centuri--init-buffer (buffer-name))

  (add-hook 'window-size-change-functions
            'centuri--hook-update
            nil
            t)

  (centuri-mid)
  (message "Enabled centuri mode"))

(defun centuri-disable ()
  "Private function for internal useage only"
  (centuri-restore-w)
  (remove-hook 'window-size-change-functions
               'centuri--hook-update
               t)
  (message "Disabled centuri mode"))

;; interactive functions
(defun centuri-mid ()
  "Center current window"
  (interactive)
  (cond ((< (window-width) centuri--min-size)
         (message "Window is too small to centerize (min-size)"))
        ((< (window-width) centuri--max-size)
         (message "Window is too small co centerize (max-size)"))
        ((and centuri--single-window (centuri-check-for-many))
         (message "Not a single window"))
        (centuri--center-absolute
         (centuri-center-absolute))
        (t (centuri-center-relative))))

(defun centuri-set-margin-left (value)
  "Set extra left margin"
  (interactive)
  (when centuri-mode
    (setq centuri--margin-left value)))

(defun centuri-set-margin-right (value)
  "Set extra right margin"
  (interactive)
  (when centuri-mode
    (setq centuri--margin-right value)))

(defun centuri-inc-margin-left ()
  "Increment extra left margin"
  (interactive)
  (when centuri-mode
    (setq centuri--margin-left (+ centuri--margin-left 1))))

(defun centuri-inc-margin-right ()
  "Increment extra right margin"
  (interactive)
  (when centuri-mode
    (setq centuri--margin-right (+ centuri--margin-right 1))))

(defun centuri-dec-margin-left ()
  "Decrement extra left margin"
  (interactive)
  (when centuri-mode
    (setq centuri--margin-left (max 1 (- centuri--margin-left 1)))))

(defun centuri-dec-margin-right ()
  "Decrement extra right margin"
  (interactive)
  (when centuri-mode
    (setq centuri--margin-right (max 0 (- centuri--margin-right 1)))))

;;;###autoload
(defun centuri-mode-toggle ()
  "Toggle centuri mode"
  (if centuri-mode
      (centuri-mode -1)
    (centuri-mode +1)))

;;;###autoload
(define-minor-mode centuri-mode
  "Minor mode that centeres window content"
  :lighter centuri--lighter
  :init-value nil
  :global nil
  (if (symbol-value 'centuri-mode)
      (centuri-enable)
    (centuri-disable)))

(provide 'centuri-mode)
(provide 'centuri)

;;; centuri.el ends here
