OFNs = backup
ORGs = $(addsuffix .org, $(OFNs))
EMACS = emacs -q --no-site-file --batch

all: README.md $(addprefix generated/from/, $(ORGs)) packaged/backup.el

packaged/backup.el: version.org generated/from/backup.org packaged/
	sed "s/the-version/`head -n1 $<`/" header.el > $@
	cat generated/backup.el >> $@
	echo "(provide 'backup)" >> $@
	echo ";;; backup.el ends here" >> $@
	emacsclient -e '(untilde (cdr (assoc "local-packages" package-archives)))' | xargs cp $@
	-@chgrp tmp $@

version.org: change-log.org helpers/derive-version.el
	emacsclient -e '(progn (load "$(CURDIR)/helpers/derive-version.el") (format-version "$<"))' | xargs echo > $@
	@echo "← generated `date '+%m/%d %H:%M'` from [[file:$<][$<]]" >> $@
	@echo "by [[file:helpers/derive-version.el][derive-version.el]]" >> $@
	-@chgrp tmp $@

generated/from/%.org: %.org generated/from/
	emacsclient -e '(progn (load "$(CURDIR)/helpers/derive-version.el") (printangle "$<"))' | xargs echo > $@
	-@chgrp tmp $@ `cat $@`
	-@chmod a-x `cat $@`

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown) (kill-buffer))'
	@sed -i "s/\.md)/.org)/g"  $@
	-@chgrp tmp $@

clean:
	-rm -r generated version.org

.PHONY: clean all

%/:
	[ -d $@ ] || mkdir -p $@
