FROM rocker/verse:latest

RUN apt-get update && apt-get install -y \
    ranger \
    git \
    tig \ 
    && install2.r --error \
       --deps TRUE \
       styler \
    && ln -sf /usr/share/zoneinfo/Europe/Berlin /etc/localtime \
    && git config --global user.email "scheer@freescience.de" \
    && git config --global user.name "Marsel Scheer" \
    && mv /root/.gitconfig /home/rstudio \
    && chown rstudio:rstudio /home/rstudio/.gitconfig
