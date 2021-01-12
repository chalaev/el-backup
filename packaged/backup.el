;;; backup.el --- daily backup for text files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Oleg Shalaev <oleg@chalaev.com>

;; Author:     Oleg Shalaev <oleg@chalaev.com>
;; Version:    0.2.1

;; Package-Requires: (shalaev)
;; Keywords:   backup, gpg, encryption
;; URL:        https://github.com/chalaev/el-backup

;;; Commentary:

;; This elisp-package
;; 1. is supposed to be called from shell script
;; 2. saves the list of recently changed important user files

;; For quick start and documentation see
;; https://github.com/chalaev/el-backup
  
;;; Code:
(require 'shalaev)
(lett((black-groups '("tmp"))
(white-groups (split-string "important keepOneYear keepTwoYears keepThreeYears"))
(white-extensions (split-string "asd patch bib fig svg diff mk json txt org conf el lisp sh mk tex sql html css js py c h cpp c++ pl gnp sed awk sk pov ini"))
(grey-extensions (split-string "pdf jpg jpeg png xcf"))
(black-extensions (split-string "jar ex list db vcproj pm o ttf ac3 afm aux idx ilg ind avi bak bbl blg brf bst bz2 cache chm cp cps dat deb dvi dv eps fb2 fn fls img iso gpx segments ky mjpeg m md mov mpg mkv jpg gif jpeg png log mp3 mp4 m2v ogg ogm out part pbm pfb pg pod pgm pnm ps rar raw gz sfd woff tbz tgz tga tif tiff toc tp vob vr wav xml xz Z zip"))
(white-names '("Makefile"))
email to-be-saved; list of files to be archieved
rejected; list of rejected files

(conf-dir (need-dir emacs-d "conf"))
(conf-file (concat conf-dir "el-backup.conf"))
(lists-of-strings (split-string "black-root-dirs black-matches black-groups white-groups white-extensions grey-extensions black-extensions white-names to-be-saved rejected"))
(str-params '("email"))
(defun read-conf(); reads configuration file
(needs ((conf (read-conf-file conf-file) (clog :error "could not read %s" conf-file)))
(dolist (CP (mapcar #'car conf))
  (setcdr (assoc CP conf)
    (cond
((member CP str-params) (car (split-string (cdr (assoc CP conf)))))
((member CP lists-of-strings) (split-string (cdr (assoc CP conf)))))))
conf))

(maxDirRecursion 7)
(pubConf "/etc/el-backup/public.conf")
(black-matches (split-string "tmp /old /log /Downloads /Загрузки /.git/"))
(black-root-dirs (split-string "/tmp/ /etc/"))

(file-fields; indices numerating array fields
(list
'plain; original (local) file name
'uname; user name
'gname; group name
'mtime; modification time
'size; file size (should not be saved)
'modes))); permissions
(let((i 0)) (dolist (field-name file-fields) (setf i (1+ (set field-name i)))))

(update-conf (read-conf) (append str-params lists-of-strings))

(defun backup-black-p(FN &optional FR)
(ifn-let ((FR(or FR(get-file-properties FN)))) t
(let*((FR(or FR(get-file-properties FN))) (group(aref FR gname)) (ext(file-name-extension FN)))
  (or
   (member group black-groups)
   (member ext black-extensions) (backup-file-name-p FN)
   (string-match (eval `(rx ,(cons 'or black-matches))) FN)
   (when black-matches  (string-match (eval `(rx ,(cons 'or black-matches))) FN))
   (when black-root-dirs(string-match (eval `(rx bol ,(cons 'or black-root-dirs))) FN))
   (string-match (to-dir ~ "local") FN) (string-match (concat ~ "\\.") FN)
   (string-match "^#.*#$" (file-name-nondirectory FN))))))
(defun backup-white-p(FN &optional FR)
(when-let((FR(or FR(get-file-properties FN))) (group(aref FR gname)) (ext(file-name-extension FN)))
  (or (member group white-groups) (member ext white-extensions) (member(file-name-nondirectory FN) white-names))))

(defun backup?(time-ref FN &optional importantDirP)
"decides on whether a file (not dir) should be archived"
(when-let((FR(get-file-properties FN)) (group(aref FR gname)) (ext(file-name-extension FN)))
(and
 (time< time-ref (aref FR mtime)); file should not be too old
 (not(or (backup-black-p FN FR)
     (and (not importantDirP) (member ext grey-extensions))))
 (or
  (backup-white-p FN FR)
(< (aref FR size) 1048000)))))

(defun scan-dir(time-ref &optional DN rec-number importantDirP)
(let((DN(file-name-as-directory(or DN ~))) (rec-number(or rec-number 0)))
;;(clog :info "scanning dir %s" DN)
  (if(< maxDirRecursion rec-number) (clog :error "reached max dir recursion=%d for %s" maxDirRecursion DN)
    (dolist (LN (directory-files DN nil nil t))
      (unless(member LN '("." "..")); warning: LN is INCOMPLETE file name
	(let((FN (concat DN LN)))
	  (if(file-directory-p FN)
	      (cond
	       (   (backup-white-p FN)   (scan-dir time-ref FN (1+ rec-number) t))
	       ((not(backup-black-p FN)) (scan-dir time-ref FN (1+ rec-number)))
	       (t(clog :info "not descending into blacklisted %S" DN)))

(unless(file-symlink-p FN); not saving symlinks and not following them
(if(backup? time-ref FN importantDirP)
 (push FN to-be-saved)
(unless(member (file-name-extension FN) black-extensions)
 (push FN rejected)))))))))))

(defun backup-do()
"main function that launches backup"

(needs((localDir (file-name-as-directory(cdr(assoc "localDir" (read-conf-file pubConf)))) (clog :error "could not read localDir from %s" pubConf))
       (BI(concat localDir "backup.version"))
       (BI-FR(get-file-properties BI)(clog :error "could not read %s" BI))
       (vvv(split-string(with-temp-buffer (insert-file-contents BI) (read-line)))); e.g., ("0" "0" "1")
       (V-V-V(mapconcat 'identity vvv "-")))
(let((MMM(file-name-as-directory(concat localDir V-V-V))))

(clog :info "scanning user %s's files" (user-login-name))
(scan-dir (aref BI-FR mtime))
(clog :info "there are %d user files to be archived" (length to-be-saved))

(with-temp-file (concat MMM (user-login-name) ".list")
(mapcar #'(lambda(FN) (insert FN) (newline)) (reverse to-be-saved)))
(with-temp-file (concat MMM (user-login-name) ".rejected")
(mapcar #'(lambda(FN) (insert FN) (newline)) (reverse rejected))))
"see the log")))
(provide 'backup)
;;; backup.el ends here
