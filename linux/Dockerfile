FROM ubuntu:22.04 as buildenv

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get -y update \
    && apt-get -y install \
        libchicken-dev \
        chicken-bin \
        build-essential \
        bash

COPY src/install_chicken_eggs.sh /
RUN chmod a+x /install_chicken_eggs.sh

RUN /install_chicken_eggs.sh

VOLUME /src
