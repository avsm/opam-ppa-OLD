#!/bin/sh -ex
curl -OL https://github.com/OCamlPro/opam/archive/master.tar.gz
file=opam_1.0.1+1SNAPSHOT`date +%Y%m%d`.orig.tar
rm -rf opam-master
tar -zxvf master.tar.gz
cd opam-master
./configure
make archive
grep -v 'src_ext' .gitignore > .gnew
mv .gnew .gitignore
rm -f src/core/opamScript.ml src/core/opamGitVersion.ml src/core/opamVersion.ml
rm -f config.status config.log
rm -f opam-full-1.1.0.tar.gz
rm -f src_ext/*.tbz src_ext/*.tar.gz
rm -f Makefile.config
cd ..
rm -f ${file}.bz2 ${file}
rm -rf opam-1.1.0
mv opam-master opam-1.1.0
tar -jcvf ${file}.bz2 opam-1.1.0
