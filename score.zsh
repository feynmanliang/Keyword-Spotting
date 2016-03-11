#!/usr/bin/zsh

scp gate:~/output/${1} output/.
rm -rf scoring/${1:r}
./scripts/score.sh output/${1} scoring

./print_twv.zsh ${1}
