#!/bin/bash

cd "$(dirname "$0")"
docker build -f Dockerfile -t dev-haskell ./
