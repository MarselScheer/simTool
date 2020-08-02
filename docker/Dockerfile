FROM rocker/verse:4.0.0

RUN apt-get update && apt-get install -y \
    ranger \
    git \
    tig \
    emacs \
# xdg-utils and ff necessary to display 
# drake-network-graphs from doom-emacs
    xdg-utils \
    firefox \
    && ln -sf /usr/share/zoneinfo/Europe/Berlin /etc/localtime \
    && git config --global user.email "scheer@freescience.de" \
    && git config --global user.name "Marsel Scheer" \
    && mv /root/.gitconfig /home/rstudio \
    && chown rstudio:rstudio /home/rstudio/.gitconfig \
    && usermod --shell /bin/bash rstudio \
# create directory for mounting the host file system
    && su -l rstudio -c "mkdir -p /tmp/hostfs" \
    && su -l rstudio -c "ln -s /tmp/hostfs /home/rstudio/hostfs" \
    && su -l rstudio -c "echo RENV_PATHS_ROOT=~/hostfs/renv/ > /home/rstudio/.Renviron"
