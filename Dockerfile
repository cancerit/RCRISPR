FROM rocker/r-ubuntu:20.04

USER root

ENV LC_ALL C
ENV LC_ALL C.UTF-8
ENV LANG C.UTF-8
ENV DISPLAY=:0
ENV DEBIAN_FRONTEND=noninteractive

ENV OPT /opt/wsi-t113
ENV R_LIBS $OPT/R-lib
ENV R_LIBS_USER $R_LIBS
ENV BUILD /build
RUN mkdir -p $R_LIBS_USER $BUILD

RUN apt-get update && \
  apt-get install -yq --no-install-recommends \
  libssl-dev \
  libcurl4-openssl-dev \
  libxml2-dev \
  pandoc

COPY . $BUILD/
WORKDIR $BUILD

RUN R -e "install.packages('devtools', lib = Sys.getenv(\"R_LIBS_USER\"))"
RUN R -e 'devtools::install_deps(dep = T, lib = Sys.getenv("R_LIBS_USER"))'

# WARNING: Up to here should be identical to Dockerfile-ci
RUN R -e 'devtools::install()'

RUN adduser --disabled-password --gecos '' ubuntu && chsh -s /bin/bash && mkdir -p /home/ubuntu
WORKDIR /home/ubuntu
USER ubuntu

CMD ["/bin/bash"]
