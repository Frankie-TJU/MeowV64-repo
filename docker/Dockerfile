# https://github.com/nightscape/scala-mill/blob/master/Dockerfile
FROM debian:stable
RUN sed -i 's/deb.debian.org/mirrors.tuna.tsinghua.edu.cn/g' /etc/apt/sources.list.d/debian.sources
RUN sed -i 's/security.debian.org/mirrors.tuna.tsinghua.edu.cn/g' /etc/apt/sources.list.d/debian.sources
RUN apt update
RUN apt install -y wget verilator iverilog curl gcc-riscv64-unknown-elf gcc-riscv64-linux-gnu
RUN sh -c "curl -L https://github.com/com-lihaoyi/mill/releases/download/0.11.5/0.11.5 > /usr/local/bin/mill && chmod +x /usr/local/bin/mill"
RUN apt update
RUN apt install -y openjdk-17-jdk
RUN touch build.sc
RUN mill -i resolve _
RUN rm build.sc
RUN apt install -y make bsdextrautils autoconf g++ build-essential
RUN apt install -y zlib1g-dev
RUN apt install -y libgmp-dev
RUN apt install -y pkg-config
RUN apt install -y z3
RUN apt install -y git
RUN apt install -y device-tree-compiler
RUN apt install -y libboost-dev
