**Backup first**

```sh
mkdir -p ~/Desktop/p/emacs-backup/
cp ~/.emacs.d/init.el ~/Desktop/p/emacs-backup/init`date +%Y%m%d%H%M`.el
cp ~/.emacs.d/readme.md ~/Desktop/p/emacs-backup/readme`date +%Y%m%d%H%M`.md
cp ~/.emacs.d/development.md ~/Desktop/p/emacs-backup/development`date +%Y%m%d%H%M`.md
ls ~/Desktop/p/emacs-backup/
```

**Sync**

```sh
cp ./init.el  ~/.emacs.d/init.el
cp ./readme.md  ~/.emacs.d/readme.md
cp ./development.md  ~/.emacs.d/development.md
cp ./example.md  ~/.emacs.d/example.md
cp ./test.md  ~/.emacs.d/test.md

ls ~/.emacs.d/
cat ~/.emacs.d/readme.md
```
