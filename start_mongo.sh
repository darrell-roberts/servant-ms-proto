#!/bin/bash

docker run --name servant-mongo -v data:/data/db -p 27017:27017 -d mongo

