(require 'cl-lib)

(defgroup rule-based-switch-buffer nil
  "Rule based switch buffer framework."
  :group 'convenience)

(defcustom rule-based-switch-buffer-completing-read-fn 'completing-read
  "Function for completing read for choose a file name. "
  :type 'function
  :group 'rule-based-switch-buffer)

;; ================================================
;; private

(cl-defstruct rbsb-rule
  name
  matcher
  mappers)

(defvar rbsb-rules '())

(defun rbsb-flatten (x)
  (cl-labels ((rec (x acc)
                   (cond ((null x) acc)
                         ((atom x) (cons x acc))
                         (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun rbsb-push-rule (name matcher mappers)
  (let ((rules (cl-remove-if (lambda (rule) (eq name (rbsb-rule-name rule))) rbsb-rules))
        (rule (make-rbsb-rule
               :name name
               :matcher matcher
               :mappers (if (functionp mappers) (list mappers) mappers))))
    (setq rbsb-rules (cons rule rules))))

(defun rbsb-rule-match-p (rule file-name)
  (funcall (rbsb-rule-matcher rule) file-name))

(defun rbsb-matched-rules (file-name)
  (cl-remove-if-not (lambda (rule)
                      (rbsb-rule-match-p rule file-name))
                    rbsb-rules))

(defun rbsb-mapped-file-names (rule file-name)
  (mapcar (lambda (mapper) (funcall mapper file-name))
          (rbsb-rule-mappers rule)))

(defun rbsb-switch-buffer-candidates (file-name)
  (cl-remove-if-not
   #'file-exists-p
   (rbsb-flatten
    (mapcar (lambda (rule) (rbsb-mapped-file-names rule file-name))
            (rbsb-matched-rules file-name)))))

(defun rbsb-select-file-name (candidate-file-names)
  (if (<= (length candidate-file-names) 1)
      (car candidate-file-names)
    (let ((choices (mapcar #'file-relative-name candidate-file-names)))
      (funcall rule-based-switch-buffer-completing-read-fn "Choose: " choices))))

;; ================================================
;; public

;;;###autoload
(cl-defmacro rule-based-switch-buffer-define (name &key matcher mappers)
  (declare (indent defun))
  `(progn
     (rbsb-push-rule ',name #',matcher ',mappers)
     t))

;;;###autoload
(defun rule-based-switch-buffer ()
  (interactive)
  (let* ((file-name (buffer-file-name))
         (candidate-file-names (rbsb-switch-buffer-candidates file-name))
         (selected-file-name (rbsb-select-file-name candidate-file-names)))
    (when selected-file-name
      (find-file selected-file-name))))

(provide 'rule-based-switch-buffer)
