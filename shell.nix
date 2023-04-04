{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/9e5e77806a692277da477ca1879e24789350911e.tar.gz") {}
}:

pkgs.pkgsCross.riscv64-embedded.mkShell {
  # native
  nativeBuildInputs = with pkgs; [
    # Scala
    (mill.override { jre = pkgs.jdk11; })
    (sbt.override { jre = pkgs.jdk11; })
    dtc
    circt

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

