#!/bin/bash

# Compilación del programa de simulación.
cd ./RTSUFVM/
make clean
make

cd ./bin
./Compile.sh

cd ../../paintDELFIN/
python paint.py
