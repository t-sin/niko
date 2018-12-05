#### This is a Dockerfile for portable building and executing Common Lisp program.
#### This requires some preliminalies to the program which built with this:
####
#### - The program shall be executed as a single executable file
#### - Entry point of the program is written as a roswell script
####     - More details, see [Roswell](https://github.com/roswell/roswell)
####
#### This Dockerfile can mainly used in two cases:
####
#### - To build the executable file
#### - To run the program by **not Common Lisp user**


### Base image for building
#
# If you want to build only once, you should atatch a tag individually like this:
# `$ sudo docker build --target cl-base -t cl-base .`
FROM debian:stretch as cl-base

# Builder requires some dependent not-Common-Lisp library, because of `ql:quickload`.
RUN apt update && apt install -y libev-dev build-essential libcurl4-gnutls-dev autoconf git wget unzip
RUN wget https://github.com/roswell/roswell/archive/master.zip && unzip master.zip
RUN cd roswell-master && ./bootstrap && ./configure && make && \
    make install && ros setup && ros install qlot


### Execution environment
#
# If you want to build only once, do this:
# `$ sudo docker build --target runner -t runner .`
FROM debian:stretch as runner

RUN apt update
RUN apt install -y libev-dev libcurl4-gnutls-dev autoconf git


### Dependency installed environment (to reduce build speed)
#
# If you want to build only once, do this:
# `$ sudo docker build --target deps -t deps .`
FROM cl-base as deps

ADD ./qlfile /app/
ADD ./qlfile.lock /app/
RUN cd /app && $HOME/.roswell/bin/qlot install


### Build environment
FROM deps as builder

ADD ./ /app/
RUN cd /app && $HOME/.roswell/bin/qlot exec ros build roswell/niko.ros


### Execution environment
#
# You can use this container to run the program or copy executable file built
FROM runner

COPY --from=builder /app/roswell/niko /usr/bin/niko
CMD [ "/usr/bin/niko", "start", "0.0.0.0", "5000" ]
EXPOSE 5000
