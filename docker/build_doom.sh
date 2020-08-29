#!/bin/bash

./build.sh
sudo docker build -t doom_r:4.0.2 -f Dockerfile.doom .
