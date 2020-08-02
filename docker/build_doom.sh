#!/bin/bash

./build.sh
sudo docker build -t doom_r:4.0.0 -f Dockerfile.doom .
