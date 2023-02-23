{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/1085ad554ffecf098555c77b0a40f4b14b51f940.tar.gz") {}
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

