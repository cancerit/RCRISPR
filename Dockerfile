FROM rocker/r-base:4.0.5

USER root

ENV LC_ALL C
ENV LC_ALL C.UTF-8
ENV LANG C.UTF-8
ENV DISPLAY=:0
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
  apt-get install -yq --no-install-recommends \
  libssl-dev \
  libcurl4-openssl-dev \
  libxml2-dev \
  pandoc

ENV OPT /opt/wsi-t113
RUN mkdir -p "$OPT"

ENV R_LIBS $OPT/R-lib
ENV R_LIBS_USER $R_LIBS
RUN mkdir -p $R_LIBS_USER

RUN mkdir $OPT/rcrispr
COPY . $OPT/rcrispr

RUN mkdir /data
RUN R CMD build --no-build-vignettes --no-manual $OPT/rcrispr && mv rcrispr*gz /data/rcrispr.tar.gz
RUN chmod -R 777 /opt/wsi-t113
RUN Rscript -e "install.packages(c('optparse', 'devtools', 'testthat', 'htmltools', 'DT', 'covr'), repos = 'https://www.stats.bris.ac.uk/R/', lib = \"${R_LIBS_USER}\")"
RUN R CMD INSTALL -l "${R_LIBS_USER}" /data/rcrispr.tar.gz

RUN adduser --disabled-password --gecos '' ubuntu && chsh -s /bin/bash && mkdir -p /home/ubuntu
WORKDIR /home/ubuntu
USER ubuntu

CMD ["/bin/bash"]