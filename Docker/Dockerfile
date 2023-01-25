# Sample usage: 
#
# docker build . -t huazhou/ucla_biostat_203b_2023w
# docker-compose up
# docker image push huazhou/ucla_biostat_203b_2023w
#
# https://rocker-project.org/images/versioned/rstudio.html
# ubuntu: 22.04 (jammy), R: 4.2.2, Python: 3.10
##

FROM rocker/geospatial:4.2.2

RUN /rocker_scripts/install_jupyter.sh

COPY install_extra_pkgs.sh /rocker_scripts/install_extra_pkgs.sh

RUN /rocker_scripts/install_extra_pkgs.sh

###
FROM rocker/r-ver:4.2.1

ENV S6_VERSION=v2.1.0.2
ENV RSTUDIO_VERSION=daily
ENV DEFAULT_USER=rstudio
ENV PANDOC_VERSION=default
ENV PATH=/usr/lib/rstudio-server/bin:$PATH

RUN /rocker_scripts/install_rstudio.sh
RUN /rocker_scripts/install_pandoc.sh

RUN apt-get update && apt-get install -y openssh-client
RUN mkdir -p /home/rstudio/.ssh
RUN chown -R rstudio:rstudio /home/rstudio/.ssh

EXPOSE 8787

CMD ["/init"]