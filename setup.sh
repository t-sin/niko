#!/bin/sh

sudo docker build --target niko-cl-base -t niko-cl-base .
sudo docker build --target niko-runner -t niko-runner .
sudo docker build --target niko-deps -t niko-deps .
