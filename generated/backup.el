(mapcar #'require '(cl-lib shalaev))
(letc(read-conf-file (FN *config-directory* "el-backup.conf"))
(((:string) black-groups '("tmp"))
 ((:string) white-groups (split-string "important keepOneYear keepTwoYears keepThreeYears"))
 ((:string) white-extensions (split-string "asd patch bib fig svg diff mk json txt org conf el lisp sh mk tex sql html css js py c h cpp c++ pl gnp sed awk sk pov ini"))
 ((:string) grey-extensions (split-string "pdf jpg jpeg png xcf"))
 ((:string) black-extensions (split-string "jar ex list db vcproj pm o ttf ac3 afm aux idx ilg ind avi bak bbl blg brf bst bz2 cache chm cp cps dat deb dvi dv eps fb2 fn fls img iso gpx segments ky mjpeg m md mov mpg mkv jpg gif jpeg png log mp3 mp4 m2v ogg ogm out part pbm pfb pg pod pgm pnm ps rar raw gz sfd woff tbz tgz tga tif tiff toc tp vob vr wav xml xz Z zip"))
 ((:string) white-names '("Makefile")) (black-names '(".git"))
 ((:string) black-matches (split-string "tmp /old /log /Downloads /Загрузки /.git/"))
 ((:string) black-root-dirs (split-string "/tmp/ /etc/")))

(let(to-be-saved; list of files to be archieved
  rejected; list of rejected files
  (maxDirRecursion 7)
  (pubConf "/etc/el-backup/public.conf")

(file-fields '(plain uname gname mtime size modes)))
(let((i 0)) (dolist (field-name file-fields) (setf i (1+ (set field-name i)))))

(defun backup-black-p(FN &optional FR)
(ifn-let ((FR(or FR(get-file-properties FN)))) t
(let*((FR(or FR(get-file-properties FN))) (group(aref FR gname)) (LN (file-name-nondirectory FN)) (ext(file-name-extension LN)))
  (or
   (member LN black-names)

(member group black-groups)
   (member ext black-extensions) (backup-file-name-p FN)
   (when-let((black-matches black-matches)) (string-match (eval `(rx ,(cons 'or black-matches))) FN))
   (when-let((black-root-dirs black-root-dirs)) (string-match (eval `(rx bol ,(cons 'or black-root-dirs))) FN))
   (string-match (to-dir ~ "local") FN) (string-match (concat ~ "\\.") FN)
   (string-match "^#.*#$" LN)))))

(defun backup-white-p(FN &optional FR)
  (when-let((FR(or FR(get-file-properties FN))) (group(aref FR gname)))
    (let((ext(file-name-extension FN)))
      (or (member group white-groups) (member ext white-extensions) (member(file-name-nondirectory FN) white-names)))))

(defun backup?(time-ref FN &optional importantDirP)
"decides on whether a file (not dir) should be archived"
(when-let((FR(get-file-properties FN)) (group(aref FR gname)))
  (let((ext(file-name-extension FN)))
(and
 (time< time-ref (aref FR mtime)); file should not be too old
 (not(or(backup-black-p FN FR)
     (and (not importantDirP) (member ext grey-extensions))))
 (or
  (backup-white-p FN FR)
(< (aref FR size) 1048000))))))

(defun scan-dir(time-ref &optional DN rec-number importantDirP)
(let((DN(file-name-as-directory(or DN ~))) (rec-number(or rec-number 0)))
;;(clog :info "scanning dir DN= %s" DN)
  (unless(< maxDirRecursion rec-number)
    (dolist(LN(error-in(format "scanning %s" DN)(directory-files DN nil nil t)))
      ;; (clog :info "LN= %s" LN); when el-backup
      (unless(member LN '("." "..")); LN is *base* file name
	(let((FN(concat DN LN)))
        (unless(file-symlink-p FN)
	  (if(file-directory-p FN)
	      (cond
	       (    (backup-white-p FN)  (scan-dir time-ref FN (1+ rec-number) t))
	       ((not(backup-black-p FN)) (scan-dir time-ref FN (1+ rec-number))))
	       ;; (t(clog :info "not descending into blacklisted %S" DN))

(if(backup? time-ref FN importantDirP)
 (push FN to-be-saved)
 (unless(member(file-name-extension FN) black-extensions)
  (push FN rejected)))))))))))

(defun previous-backup-version(vvv)
(let((v1(car vvv))(v2(cadr vvv))(v3(caddr vvv)))
  (if(< 0 v3)(cl-decf v3)
    (if(< 0 v2)(cl-decf v2)
      (cl-decf v1)))
  (list v1 v2 v3))); v1 might = -1
(defun backup-do()
"main function that launches backup"

(needs((localDir(file-name-as-directory(cdr(assoc 'localDir (read-conf-file pubConf)))) (clog :error "could not read localDir from %s" pubConf))
       (BI(concat localDir "backup.version"))
       (vvv(split-string(with-temp-buffer (insert-file-contents BI) (read-line)))); e.g., ("0" "0" "1")
       (V-V-V(mapconcat 'identity vvv "-"))
       (BI-FR(get-file-properties BI)(clog :error "could not read %s" BI)))
(let*((MMM(file-name-as-directory(concat localDir V-V-V)))
      (vvv(previous-backup-version(mapcar #'string-to-number vvv)))
      (10yrsAgo (time-add (current-time) (* -60 60 24 365 10)))
      (previous-backup-date

(let(theDate)
(while(and(not theDate)(< 0 (apply #'+ vvv)))
(let((FN(format "%s%d-%d-%d.tar.bz2" localDir (car vvv) (cadr vvv) (caddr vvv))))
(clog :info "looking for %s..." FN)
(ifn(file-exists-p FN)
 (setf vvv(previous-backup-version vvv))
 (clog :info "...found")
 (setf theDate (aref(get-file-properties FN) mtime)))))
(or theDate
(progn
(clog :warning "no previous archives found, saving ALL files over the last 10yrs")
(setf theDate 10yrsAgo))))))

(clog :info "scanning user %s's files" (user-login-name))
(scan-dir previous-backup-date)
(clog :info "there are %d user files to be archived" (length to-be-saved))

(with-temp-file (concat MMM (user-login-name) ".list")
  (mapc #'(lambda(FN) (insert FN) (newline)) (reverse to-be-saved)))
(with-temp-file (concat MMM (user-login-name) ".rejected")
  (mapc #'(lambda(FN) (insert FN) (newline)) (reverse rejected))))
"see the log"))))
