{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/0ea7a8f1b939d74e5df8af9a8f7342097cdf69eb.tar.gz") {}
}:

pkgs.pkgsCross.riscv64.mkShell {
  # native
  nativeBuildInputs = with pkgs; [
    # Scala
    (mill.override { jre = pkgs.jdk11; })
    (sbt.override { jre = pkgs.jdk11; })
    dtc

    # Testcases
    autoconf

    # Simulation
    verilator
    pkg-config
    gmp
  ];

  buildInputs = with pkgs; [
    # Simulation
    zlib
    gmp
  ];
  # for riscv64-unknown-linux-gnu-gcc
  TARGET = "riscv64-unknown-linux-gnu";
  PREFIX = "riscv64-unknown-linux-gnu-";
}

