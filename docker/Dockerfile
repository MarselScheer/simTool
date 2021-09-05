FROM rocker/verse:4.1.0

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
RUN mkdir -p /tmp/hostfs \
    && ln -s /tmp/hostfs \
    && ln -s /tmp/hostfs/.emacs.d \
    && ln -s /tmp/hostfs/.gitconfig \
    && ln -s /tmp/hostfs/.ssh \
    && echo "export DISPLAY=:0" > /home/rstudio/.bashrc \
    && echo RENV_PATHS_ROOT=~/hostfs/renv/ > /home/rstudio/.Renviron

# last command MUST be USER root?!?
# otherwise container will directly stop
USER root
