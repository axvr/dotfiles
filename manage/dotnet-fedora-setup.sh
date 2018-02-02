#!/usr/bin/env sh

# Install Tools for .NET Core Development on Fedora

# .NET Core SDK, CLI & Runtime
sudo dnf copr enable @dotnet-sig/dotnet
sudo dnf install dotnet-sdk-2.0

# Microsoft VSTS-CLI Tool
echo "Set install location to: '~/.vsts-cli'"
curl -L https://aka.ms/install-vsts-cli | bash

# TODO Team Explorer Everywhere (TFVC: TEE-CLC)
# Download (wget) latest TEE-CLC-XX-XXX-X.zip from:
# https://github.com/Microsoft/team-explorer-everywhere/releases
# unzip TEE-CLC-XX-XXX-X.zip
# mv TEE-CLC-XX-XXX-X ~/.tee-clc
# rm TEE-CLC-XX-XXX-X.zip
# Restart shell
# run and accept: tf eula

