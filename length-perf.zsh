#!/bin/zsh

for i in {1..20}; do
  #print $i $(./scripts/termselect.sh lengths-word.map output/${1} scoring $i)
  print $i $(./scripts/termselect.sh lengths-morph.map output/${1} scoring $i)
done
