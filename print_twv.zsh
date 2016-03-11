#!/usr/bin/zsh

print ${1}

./scripts/termselect.sh lib/terms/ivoov.map output/${1} scoring iv
./scripts/termselect.sh lib/terms/ivoov.map output/${1} scoring oov
./scripts/termselect.sh lib/terms/ivoov.map output/${1} scoring all
