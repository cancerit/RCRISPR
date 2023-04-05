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

# hadolint ignore=DL3059
RUN mkdir -p $R_LIBS_USER $BUILD
# hadolint ignore=DL3008
RUN apt-get update && \
  apt-get install -yq --no-install-recommends \
  libssl-dev \
  libcurl4-openssl-dev \
  libxml2-dev \
  libnlopt-dev \
  libfontconfig1-dev \
  pandoc \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY . $BUILD/
WORKDIR $BUILD

ENV INST_NCPU=$(nproc)

# hadolint ignore=DL3059
RUN R -e "install.packages('devtools', lib = Sys.getenv(\"R_LIBS_USER\"), Ncpus = Sys.getenv(\"INST_NCPU\"))"
# hadolint ignore=DL3059
RUN R -e "devtools::install_deps(dep = T, lib = Sys.getenv(\"R_LIBS_USER\"), threads = Sys.getenv(\"INST_NCPU\"))"
# hadolint ignore=DL3059
RUN R -e 'devtools::install()'

## user config
# hadolint ignore=DL3059
RUN adduser --disabled-password --gecos '' ubuntu && chsh -s /bin/bash && mkdir -p /home/ubuntu
WORKDIR /home/ubuntu
USER ubuntu

CMD ["/bin/bash"]
