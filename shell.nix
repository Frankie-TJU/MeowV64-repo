{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/e42377bbe5ef06ffec13eebf7949d72793ed66f9.tar.gz") {}
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
    openocd

    # Others
    bear
  ];

  buildInputs = with pkgs; [
    # Simulation
  ];
  # for riscv64-none-elf-gcc
  TARGET = "riscv64-none-elf";
  PREFIX = "riscv64-none-elf-";
}

