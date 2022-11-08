# load base image
FROM rocker/tidyverse:4.2.2

# custom tag
LABEL maintainer="Nik Agarwal <gitnik@niks.me>"

# update Linux

RUN apt-get update && apt-get install -y --no-install-recommends \
    libglpk40 \
    htop \
    jq \
    libxtst6 \
    libxt6 \
    xdg-utils \
    nano \
    python3-pip \
    && python3 -m pip --no-cache-dir install radian \
    && ln -s /usr/local/lib/R/lib/libR.so /lib/x86_64-linux-gnu/libR.so

# Install RENV
ENV RENV_VERSION 0.16.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

# create renv cache folder
RUN mkdir -p /renv/cache

# expose the port for RStudio
EXPOSE 8787

CMD ["/init"]

