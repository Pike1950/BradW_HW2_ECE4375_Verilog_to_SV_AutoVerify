# ============================================================
# Verilator + GTKWave development container
# Works on x86_64 (your Windows desktop) and arm64 (Raspberry Pi)
# ============================================================
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# Core build tools + Verilator + GTKWave
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    verilator \
    gtkwave \
    git \
    ccache \
    help2man \
    perl \
    python3 \
    autoconf \
    flex \
    bison \
    libfl2 \
    libfl-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Verify install
RUN verilator --version && gtkwave --version || true

WORKDIR /workspace
