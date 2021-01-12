
# Table of Contents

1.  [Requirements](#org888d782)
2.  [Quick Start](#org4c618af)
3.  [Backup philosophy](#org5e674a8)
4.  [License](#org57dda06)

Simple backup system for Linux users who use `emacs` a lot.


<a id="org888d782"></a>

# Requirements

1.  `emacs`, `mktemp`, `tar`, `gpg` (all available from standard linux packages),
2.  elisp part of my [lisp-goodies](https://github.com/chalaev/lisp-goodies) project, and
3.  [~/.emacs.d/batch-start.el](https://github.com/chalaev/lisp-goodies/blob/master/packaged/batch-start.el) (needed for `emacsclient` called in [el-backup](el-backup)).


<a id="org4c618af"></a>

# Quick Start

1.  Examine [el-backup](el-backup) and [common.sh](common.sh) (both supposed to run with root privileges).  
    Create `/usr/local/bin/el-backup/` and copy [el-backup](el-backup) and [common.sh](common.sh) there.
2.  Create `/etc/el-backup/` and copy [secret.conf](secret.conf) and [public.conf](public.conf) there.
    Edit global configuration files in `/etc/el-backup/`. Ensure that only `root` can read `/etc/el-backup/secret.conf`
    and that non-root users are allowed to read  `/etc/el-backup/public.conf`.
3.  For each user enumerated in `/etc/el-backup/secret.conf`, copy [el-backup.conf](el-backup.conf) to `~/.emacs.d/conf/el-backup.conf`
    and edit it.
4.  Create group `tmp`; the code uses it to mark temporary files.
5.  Copy [packaged/backup.el](packaged/backup.el) to `~/.emacs.d/local-packages/`.
    ([~/.emacs.d/batch-start.el](https://github.com/chalaev/lisp-goodies/blob/master/packaged/batch-start.el) will configure emacs to load [backup.el](packaged/backup.el) from there.)
6.  Run [/usr/local/bin/el-backup/el-backup](el-backup) daily with root privileges.
    This will create files `X-Y-Z.tar.bz2` and `X-Y-Z/user.list.gz` (locally on your computer) where X, Y, Z are non-negative integers.
    Their encrypted copies in remote directory will be `X-Y-Z.gpg` and `X-Y-Z.user.gpg`


<a id="org5e674a8"></a>

# Backup philosophy

Only recently modified (later than the previous backup) files will be archived.
By default non-text files are ignored; still binary files can be archived if

-   they have "white" extension, or
-   their group is "white", or
-   their (base) name  is "white".

"White", "grey", and "black" lists can be configured in  [~/.emacs.d/conf/el-backup.conf](el-backup.conf).

There is no special command to restore the files; the user is supposed to do it manually. Here is my way to do it:

1.  Decide if I prefer to use local of remote (encrypted) backup files.
2.  With `zgrep` utility, find the file in the list of archived files. Locate the archive with this file.
3.  If needed, decrypt the archive. Extract the file from the `tar` archive.


<a id="org57dda06"></a>

# License

This code is released under [MIT license](https://mit-license.org/).

