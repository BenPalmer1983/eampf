#####################################################
#    Top level CMakeLists
#####################################################

# Set CMAKE version
cmake_minimum_required(VERSION 3.22)

# C, C++ and Fortran
project(EAMPF LANGUAGES C CXX Fortran)

# Need Fortan, MPI and C++ 17 standard
enable_language(Fortran)
find_package(MPI REQUIRED)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED true)

# Release
set(CMAKE_BUILD_TYPE Release)

# Fortran-C Interface
include(FortranCInterface)
FortranCInterface_VERIFY(CXX)

# Add libs directory and main
add_subdirectory(libs)
add_subdirectory(main)



#####################################################
