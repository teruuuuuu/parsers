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


# 必要なツールをインストール
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    gnupg \
    software-properties-common \
    build-essential \
    libtinfo-dev \
    libgmp-dev

# https://downloads.haskell.org/~ghcup/で最新版のGHCupを確認
# GHCupのインストールスクリプトをダウンロードして実行
RUN curl -sSL https://downloads.haskell.org/~ghcup/0.1.30.0/aarch64-linux-ghcup-0.1.30.0 -o ghcup && \
    chmod +x ghcup && \
    ./ghcup install && \
    mv ghcup "/root/.ghcup/bin"

# RUN curl --proto '=https' --tlsv1.2 -sSf 'https://downloads.haskell.org/~ghcup/0.1.30.0/aarch64-linux-static-ghcup-0.1.30.0' | sh -s -- --skip-upgrade

# bashの初期化ファイルにGHCupの初期化スクリプトを追加
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE 1
ENV PATH "/root/.ghcup/bin:${PATH}"

# GHCとCabalをインストール
RUN ghcup install ghc latest && \
    ghcup set ghc latest && \
    ghcup install cabal latest && \
    ghcup set cabal latest

# 最新版のStackをインストール
RUN curl -sSL https://get.haskellstack.org/ | sh && \
    stack setup --resolver lts-23.2

WORKDIR /root
CMD ["/usr/sbin/sshd", "-D"]
