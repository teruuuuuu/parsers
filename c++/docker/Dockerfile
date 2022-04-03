# syntax=docker/dockerfile:1
FROM ubuntu:20.04

# c++
RUN apt-get update && \
    apt-get install -y build-essential libssl-dev git wget && \
    mkdir install && cd install && \
    wget https://github.com/Kitware/CMake/releases/download/v3.23.0-rc4/cmake-3.23.0-rc4.tar.gz >> install.log && \
    tar xvf cmake-3.23.0-rc4.tar.gz >> install.log && cd cmake-3.23.0-rc4 && \
    ./configure >> install.log && make >> install.log && make install >> install.log && \
    cd ../.. && rm -rf install && \
    echo export "PATH=/usr/local/bin/:\$PATH" >> ~/.bashrc && \
    apt-get clean

# googletest
RUN mkdir /root/lib && \
    cd /root/lib && \
    git clone https://github.com/google/googletest.git -b release-1.11.0 && \
    cd /root/lib/googletest && mkdir build && cd build && \
    cmake .. && make && make install && \
    cd /root/lib && rm -rf /root/lib/googletest

# gdb
RUN apt-get install -y gdb && \
    apt-get clean