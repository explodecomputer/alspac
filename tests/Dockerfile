FROM rocker/r-ver:4.0.2

ARG WHEN

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
       libz-dev

RUN R -e "\
  options(repos = list(CRAN = 'https://mran.revolutionanalytics.com/snapshot/${WHEN}/')); \
  install.packages('remotes'); \
  remotes::install_github('explodecomputer/alspac') \
  "

RUN mkdir /home/alspac

RUN mkdir /home/example
