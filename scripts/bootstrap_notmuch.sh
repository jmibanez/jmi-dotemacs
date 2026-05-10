#!/bin/bash
# Bootstrap notmuch index


if [ -d ~/.notmuch -a  -f ~/.notmuch-config ]; then
    exit
fi

cat >> ~/.notmuch-config << EOF
# .notmuch-config - Configuration file for the notmuch mail system
[database]
path=$HOME/.notmuch/

[user]
name=JM Ibañez
primary_email=jm@jmibanez.com
other_email=jmibanez@gmail.com

[new]
tags=unread;
ignore=.nnmairix;.nnmairix.all;.nnmaildir;.nnmaildir.bork;.DS_Store;.mbsyncstate;.mbsyncstate.lock;.uidvalidity;.jma.db;.jma.db-wal;.jma.db-shm;.jma.db.lock;.jma.lock

[search]
exclude_tags=deleted;spam;trash

[maildir]
synchronize_flags=true
EOF

mkdir ~/.notmuch
cd ~/.notmuch
ln -s ~/Maildir/gmail gmail
ln -s ~/Maildir/jmibanez.com jmibanez.com
ln -s ~/Mail.archive/archive archive

notmuch new
