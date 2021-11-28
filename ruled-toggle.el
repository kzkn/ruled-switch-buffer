;;; ruled-toggle.el --- Rule based switch buffer framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Kazuki Nishikawa

;; Author: Kazuki Nishikawa <kzkn@hey.com>
;; Keywords: convenience
;; Homepage: https://github.com/kzkn/ruled-toggle
;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 0

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

;; WIP

;;; Code:

(require 'cl-lib)

(defgroup ruled-toggle nil
  "Rule based switch buffer framework."
  :group 'convenience)

(defcustom ruled-toggle-completing-read-fn 'completing-read
  "Function for completing read for choose a file name. "
  :type 'function
  :group 'ruled-toggle)

;; ================================================
;; private

(cl-defstruct ruled-toggle--rule
  name
  matcher
  mappers)

(defvar ruled-toggle--rules '())

(defun ruled-toggle--flatten (x)
  (cl-labels ((rec (x acc)
                   (cond ((null x) acc)
                         ((atom x) (cons x acc))
                         (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun ruled-toggle--push-rule (name matcher mappers)
  (let ((rules (cl-remove-if (lambda (rule) (eq name (ruled-toggle--rule-name rule))) ruled-toggle--rules))
        (rule (make-ruled-toggle--rule
               :name name
               :matcher matcher
               :mappers (if (functionp mappers) (list mappers) mappers))))
    (setq ruled-toggle--rules (cons rule rules))))

(defun ruled-toggle--rule-match-p (rule file-name)
  (funcall (ruled-toggle--rule-matcher rule) file-name))

(defun ruled-toggle--matched-rules (file-name)
  (cl-remove-if-not (lambda (rule)
                      (ruled-toggle--rule-match-p rule file-name))
                    ruled-toggle--rules))

(defun ruled-toggle--mapped-file-names (rule file-name)
  (mapcar (lambda (mapper) (funcall mapper file-name))
          (ruled-toggle--rule-mappers rule)))

(defun ruled-toggle--switch-buffer-candidates (file-name)
  (cl-remove-if-not
   #'file-exists-p
   (ruled-toggle--flatten
    (mapcar (lambda (rule) (ruled-toggle--mapped-file-names rule file-name))
            (ruled-toggle--matched-rules file-name)))))

(defun ruled-toggle--select-file-name (candidate-file-names)
  (if (<= (length candidate-file-names) 1)
      (car candidate-file-names)
    (let ((choices (mapcar #'file-relative-name candidate-file-names)))
      (funcall ruled-toggle-completing-read-fn "Choose: " choices))))

;; ================================================
;; public

;;;###autoload
(cl-defmacro ruled-toggle-define (name &key matcher mappers)
  (declare (indent defun))
  `(progn
     (ruled-toggle--push-rule ',name #',matcher ',mappers)
     t))

;;;###autoload
(defun ruled-toggle ()
  (interactive)
  (let* ((file-name (buffer-file-name))
         (candidate-file-names (ruled-toggle--switch-buffer-candidates file-name))
         (selected-file-name (ruled-toggle--select-file-name candidate-file-names)))
    (when selected-file-name
      (find-file selected-file-name))))

(provide 'ruled-toggle)
;;; ruled-toggle.el ends here
