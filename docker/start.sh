sudo docker run --rm -d -p 8787:8787 -e DISABLE_AUTH=true \
  -v ~/docker_fs:/tmp/hostfs \
  -v /tmp/.X11-unix:/tmp/.X11-unix \
  rstudio:4.0.0

