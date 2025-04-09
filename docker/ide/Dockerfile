ARG BASE_IMAGE=
FROM ${BASE_IMAGE}

ARG USER_ID=
ARG GROUP_ID=

RUN apt-get update && apt-get install -y \
    ranger \
    git \
    tig \
    htop \
    emacs \
# graphviz necessary for dot-programm for instance used
# by plantuml
    graphviz \
# xdg-utils and ff necessary to display 
# drake-network-graphs from emacs
    xdg-utils \
    firefox \
    && ln -sf /usr/share/zoneinfo/Europe/Berlin /etc/localtime

USER rstudio
WORKDIR /home/rstudio

RUN git config --global user.email "scheer@freescience.de" \
    && git config --global user.name "Marsel Scheer"
# last command MUST be USER root?!?
# otherwise container RStudio is not available
USER root


