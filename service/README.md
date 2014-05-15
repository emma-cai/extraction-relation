# Ermine service

This is a server that has a configured set of Ermine pipelines that it can run.

## API

TODO

## Installing on EC2

In order to run the Ferret pipelines, you need to build swipl from source.

Instructions are as follows:
```
$ sudo yum install gcc autoconf chrpath libunwind freetype-devel gmp-devel \
  java-1.7.0-openjdk-devel jpackage-utils libICE-devel libjpeg-devel libSM-devel \
  libX11-devel libXaw-devel libXext-devel libXft-devel libXinerama-devel \
  libXmu-devel libXpm-devel libXrender-devel libXt-devel ncurses-devel \
  openssl-devel pkgconfig readline-devel unixODBC-devel zlib-devel uuid-devel \
  libarchive-devel
$ export LD_LIBRARY_PATH=/usr/lib/jvm/jre-1.7.0/lib/amd64/server
$ curl -O http://www.swi-prolog.org/download/stable/src/pl-6.6.5.tar.gz
$ tar xzvf pl-6.6.5.tar.gz
$ cd pl-6.6.5
$ cp build.templ build
$ # Edit build to update PREFIX and SUDO:
$ perl -p -i -e 's|^PREFIX=.*|PREFIX=/usr/local|' build
$ perl -p -i -e 's|^SUDO=.*|SUDO="sudo"|' build
$ ./build
```
