FROM rocker/verse:4.0.0

RUN apt-get update && apt-get install -y \
    ranger \
    git \
    tig \
    emacs \
    && install2.r --error \
       --deps TRUE \
       styler \
       badgecreatr \
    && ln -sf /usr/share/zoneinfo/Europe/Berlin /etc/localtime \
    && git config --global user.email "scheer@freescience.de" \
    && git config --global user.name "Marsel Scheer" \
    && mv /root/.gitconfig /home/rstudio \
    && chown rstudio:rstudio /home/rstudio/.gitconfig \
    && usermod --shell /bin/bash rstudio

USER rstudio

RUN git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d \
    && ~/.emacs.d/bin/doom --yes install \
    && sed -i 's/;;ess/ess/g' ~/.doom.d/init.el \
    && ~/.emacs.d/bin/doom refresh \
    && sed -i "s/display-line-numbers-type t/display-line-numbers-type 'relative/g" ~/.doom.d/config.el \
    && mkdir -p /tmp/hostfs \
    && ln -s /tmp/hostfs ~/hostfs



USER root
