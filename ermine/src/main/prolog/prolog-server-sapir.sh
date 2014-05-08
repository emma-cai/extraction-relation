# sudo yum install pl -> v5.10.2: too old

# install from src: http://www.swi-prolog.org/download/stable/src/pl-6.6.2.tar.gz
# sudo yum install \
#  gcc \
#  autoconf \
#  chrpath \
#  libunwind \
#  freetype-devel \
#  gmp-devel \
#  java-1.6.0-openjdk-devel \
#  jpackage-utils \
#  libICE-devel \
#  libjpeg-devel \
#  libSM-devel \
#  libX11-devel \
#  libXaw-devel \
#  libXext-devel \
#  libXft-devel \
#  libXinerama-devel \
#  libXmu-devel \
#  libXpm-devel \
#  libXrender-devel \
#  libXt-devel \
#  ncurses-devel \
#  openssl-devel \
#  pkgconfig \
#  readline-devel \
#  unixODBC-devel \
#  zlib-devel \
#  uuid-devel \
#  libarchive-devel
# in pl-6.6.2: ./configure; make; make install
# in pl-6.6.2/packages: ./configure; make; make install

/usr/local/bin/swipl -q -l extract-sapir.pl -g 'server(9623)'
