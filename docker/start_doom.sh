sudo docker run --rm -d -p 8787:8787 -e DISABLE_AUTH=true \
  -v ~/docker_fs:/tmp/hostfs \
  -v /tmp/.X11-unix:/tmp/.X11-unix \
  doom_r:4.0.0

# in order to display doom-emacs on the host:
# echo $DISPLAY
# xhost +local:'containerID'
# docker exec -it containerID /bin/bash
# su -l rstudio
# export DISPLAY=<DISPLAY from host>
# emacs



