FROM mcr.microsoft.com/devcontainers/base:ubuntu-24.04

USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
    opam build-essential m4 pkg-config git curl ca-certificates python3 \
    libgmp-dev libffi-dev libssl-dev zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*
USER vscode
