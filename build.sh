#!/bin/bash

# Make dirs
mkdir -p build
mkdir -p bin
cd build

export CMAKE_CC_COMPILER=mpicc
export CMAKE_CXX_COMPILER=mpic++
export CMAKE_FORTRAN_COMPILER=mpif90


cmake -DCMAKE_CC_COMPILER=$CMAKE_CC_COMPILER -DCMAKE_CXX_COMPILER=$CMAKE_CXX_COMPILER -DCMAKE_Fortran_COMPILER=$CMAKE_FORTRAN_COMPILER -DCMAKE_Fortran_FLAGS="-O0 -g"  ../src

make -j 2

cd ../
cp build/main/EAMPF bin/EAMPF.x
