# Ermine service

This is a server that has a configured set of Ermine pipelines that it can run.

Pipelines are loaded from [application.conf](https://github.com/allenai/extraction/tree/master/service/src/main/resources/application.conf)
at startup.

The service handles [requests](https://github.com/allenai/extraction/blob/master/api/src/main/scala/org/allenai/extraction/api/JsonProtocol.scala#L10)
on `/pipeline/{name}`. When getting a request for the subpath "name", the service will run the pipeline
named "name" in the config file, using the provided named inputs.

There is currently no way to configure default (unnamed) inputs in the config file.

# Pre-deploy Installtion on EC2

In order to run the Ferret pipelines through the service on EC2, you need to build swipl from source.

Instructions are as follows:
```
# In your favorite shell, run:
sudo yum install gcc autoconf chrpath libunwind freetype-devel gmp-devel \
  java-1.7.0-openjdk-devel jpackage-utils libICE-devel libjpeg-devel libSM-devel \
  libX11-devel libXaw-devel libXext-devel libXft-devel libXinerama-devel \
  libXmu-devel libXpm-devel libXrender-devel libXt-devel ncurses-devel \
  openssl-devel pkgconfig readline-devel unixODBC-devel zlib-devel uuid-devel \
  libarchive-devel
export LD_LIBRARY_PATH=/usr/lib/jvm/jre-1.7.0/lib/amd64/server
curl -O http://www.swi-prolog.org/download/stable/src/pl-6.6.5.tar.gz
tar xzvf pl-6.6.5.tar.gz
cd pl-6.6.5
cp build.templ build
# Edit build to update PREFIX and SUDO, using perl magic:
perl -p -i -e 's|^PREFIX=.*|PREFIX=/usr/local|' build
perl -p -i -e 's|^SUDO=.*|SUDO="sudo"|' build
./build
```

# Deploying

Once your server is ready to run the service, just execute `sbt 'project service' 'deploy prod'` to deploy!
