
(defun detect-header (level)
  (save-excursion
  (let ((stars (concat (apply #'concat (cl-loop repeat level collect "\\*")) " ")))
    (when-let ((header (search-forward-regexp (concat "^" stars) nil t)))
              (beginning-of-line) (point)))))

(defun moveto-header (level &optional max-point)
  (when-let ((DH (detect-header level)))
    (when (< DH (or max-point (point-max)))
      (goto-char DH))))

;; (defun section-counter(level &optional max-point)
;;   (if (when (moveto-header level max-point) (org-show-subtree) (end-of-line) t)
;;       (1+ (section-counter level)) 0))

(defun section-counter(level &optional max-point)
  (when (moveto-header level max-point)
    (org-show-subtree)
    (end-of-line)
    (cons
     (point)
     (section-counter level max-point))))

(defun derive-version(change-log.org)
(find-file change-log.org)
(let* (MC (r (list
(progn
  (goto-char (point-min))
  (let* ((SC (section-counter 1)) (lSC (length SC)))
    (setf MC (cadr SC))
    (if(= 0 lSC) lSC (1- lSC))))
(progn
  (goto-char (point-min))
  (when (moveto-header 1) (org-show-subtree) (end-of-line))
  (let ((SC (section-counter 2 MC)))
    (setf MC (cadr SC)) (length SC)))
(progn
  (goto-char (point-min))
  (when (moveto-header 1)(org-show-subtree) (end-of-line))
  (when (moveto-header 2)(org-show-subtree) (end-of-line))
  (length (section-counter 3 MC))))))
(kill-buffer) r))

(defun format-version(change-log.org)
  (let ((r (derive-version change-log.org)))
    (format "%d.%d.%d" (first r) (second r) (third r))))

(defun printangle(FN)
  "to be used in Makefile instead of org-babel-tangle-file"
  (let ((l (length default-directory)))
    (apply #'concat (mapcar #'(lambda(x) (substring (format "%s " x) l)) (org-babel-tangle-file FN)))))
