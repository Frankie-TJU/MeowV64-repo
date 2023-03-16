{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/3afaf7d263025fc8c24247ec368f593af951b72f.tar.gz") {}
}:

pkgs.pkgsCross.riscv64-embedded.mkShell {
  # native
  nativeBuildInputs = with pkgs; [
    # Scala
    (mill.override { jre = pkgs.jdk11; })
    (sbt.override { jre = pkgs.jdk11; })
    dtc

    # Testcases
    autoconf
    spike

    # Simulation
    verilator
    pkg-config
    gmp
    openocd

    # Others
    bear
  ];

  buildInputs = with pkgs; [
    # Simulation
    zlib
    gmp
  ];
  # for riscv64-none-elf-gcc
  TARGET = "riscv64-none-elf";
  PREFIX = "riscv64-none-elf-";
}

