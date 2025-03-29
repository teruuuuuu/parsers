# ベースイメージとしてUbuntuを使用
FROM ubuntu:25.04

# タイムゾーンを日本時間に設定
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y tzdata && \
    ln -fs /usr/share/zoneinfo/Asia/Tokyo /etc/localtime && \
    dpkg-reconfigure --frontend noninteractive tzdata && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# sshdのインストール
RUN apt-get update && \
    apt-get install -y openssh-server && \
    rm -rf /var/lib/apt/lists/* && \
    echo 'root:root' chpasswd && \
    sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# 必要なパッケージのインストール
RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    libssl-dev \
    pkg-config \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
# Rustのインストール
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y

# パスを設定
ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /root
CMD ["/usr/sbin/sshd", "-D"]
