# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.22

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /DATA/disk3/cloud/Code/fortran/eampf/src

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /DATA/disk3/cloud/Code/fortran/eampf/build

# Include any dependencies generated for this target.
include libs/corelib/CMakeFiles/corelib.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include libs/corelib/CMakeFiles/corelib.dir/compiler_depend.make

# Include the progress variables for this target.
include libs/corelib/CMakeFiles/corelib.dir/progress.make

# Include the compile flags for this target's objects.
include libs/corelib/CMakeFiles/corelib.dir/flags.make

libs/corelib/CMakeFiles/corelib.dir/fortran/kinds_mod.f90.o: libs/corelib/CMakeFiles/corelib.dir/flags.make
libs/corelib/CMakeFiles/corelib.dir/fortran/kinds_mod.f90.o: /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/fortran/kinds_mod.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/DATA/disk3/cloud/Code/fortran/eampf/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object libs/corelib/CMakeFiles/corelib.dir/fortran/kinds_mod.f90.o"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/fortran/kinds_mod.f90 -o CMakeFiles/corelib.dir/fortran/kinds_mod.f90.o

libs/corelib/CMakeFiles/corelib.dir/fortran/kinds_mod.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/corelib.dir/fortran/kinds_mod.f90.i"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/fortran/kinds_mod.f90 > CMakeFiles/corelib.dir/fortran/kinds_mod.f90.i

libs/corelib/CMakeFiles/corelib.dir/fortran/kinds_mod.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/corelib.dir/fortran/kinds_mod.f90.s"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/fortran/kinds_mod.f90 -o CMakeFiles/corelib.dir/fortran/kinds_mod.f90.s

libs/corelib/CMakeFiles/corelib.dir/fortran/string/character_mod.f90.o: libs/corelib/CMakeFiles/corelib.dir/flags.make
libs/corelib/CMakeFiles/corelib.dir/fortran/string/character_mod.f90.o: /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/fortran/string/character_mod.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/DATA/disk3/cloud/Code/fortran/eampf/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building Fortran object libs/corelib/CMakeFiles/corelib.dir/fortran/string/character_mod.f90.o"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/fortran/string/character_mod.f90 -o CMakeFiles/corelib.dir/fortran/string/character_mod.f90.o

libs/corelib/CMakeFiles/corelib.dir/fortran/string/character_mod.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/corelib.dir/fortran/string/character_mod.f90.i"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/fortran/string/character_mod.f90 > CMakeFiles/corelib.dir/fortran/string/character_mod.f90.i

libs/corelib/CMakeFiles/corelib.dir/fortran/string/character_mod.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/corelib.dir/fortran/string/character_mod.f90.s"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/fortran/string/character_mod.f90 -o CMakeFiles/corelib.dir/fortran/string/character_mod.f90.s

libs/corelib/CMakeFiles/corelib.dir/fortran/string/string_mod.f90.o: libs/corelib/CMakeFiles/corelib.dir/flags.make
libs/corelib/CMakeFiles/corelib.dir/fortran/string/string_mod.f90.o: /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/fortran/string/string_mod.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/DATA/disk3/cloud/Code/fortran/eampf/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building Fortran object libs/corelib/CMakeFiles/corelib.dir/fortran/string/string_mod.f90.o"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/fortran/string/string_mod.f90 -o CMakeFiles/corelib.dir/fortran/string/string_mod.f90.o

libs/corelib/CMakeFiles/corelib.dir/fortran/string/string_mod.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/corelib.dir/fortran/string/string_mod.f90.i"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/fortran/string/string_mod.f90 > CMakeFiles/corelib.dir/fortran/string/string_mod.f90.i

libs/corelib/CMakeFiles/corelib.dir/fortran/string/string_mod.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/corelib.dir/fortran/string/string_mod.f90.s"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/fortran/string/string_mod.f90 -o CMakeFiles/corelib.dir/fortran/string/string_mod.f90.s

libs/corelib/CMakeFiles/corelib.dir/cpp/path_mod.f90.o: libs/corelib/CMakeFiles/corelib.dir/flags.make
libs/corelib/CMakeFiles/corelib.dir/cpp/path_mod.f90.o: /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/cpp/path_mod.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/DATA/disk3/cloud/Code/fortran/eampf/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building Fortran object libs/corelib/CMakeFiles/corelib.dir/cpp/path_mod.f90.o"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/cpp/path_mod.f90 -o CMakeFiles/corelib.dir/cpp/path_mod.f90.o

libs/corelib/CMakeFiles/corelib.dir/cpp/path_mod.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/corelib.dir/cpp/path_mod.f90.i"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/cpp/path_mod.f90 > CMakeFiles/corelib.dir/cpp/path_mod.f90.i

libs/corelib/CMakeFiles/corelib.dir/cpp/path_mod.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/corelib.dir/cpp/path_mod.f90.s"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/cpp/path_mod.f90 -o CMakeFiles/corelib.dir/cpp/path_mod.f90.s

libs/corelib/CMakeFiles/corelib.dir/cpp/file_system_mod.f90.o: libs/corelib/CMakeFiles/corelib.dir/flags.make
libs/corelib/CMakeFiles/corelib.dir/cpp/file_system_mod.f90.o: /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/cpp/file_system_mod.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/DATA/disk3/cloud/Code/fortran/eampf/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building Fortran object libs/corelib/CMakeFiles/corelib.dir/cpp/file_system_mod.f90.o"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/cpp/file_system_mod.f90 -o CMakeFiles/corelib.dir/cpp/file_system_mod.f90.o

libs/corelib/CMakeFiles/corelib.dir/cpp/file_system_mod.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/corelib.dir/cpp/file_system_mod.f90.i"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/cpp/file_system_mod.f90 > CMakeFiles/corelib.dir/cpp/file_system_mod.f90.i

libs/corelib/CMakeFiles/corelib.dir/cpp/file_system_mod.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/corelib.dir/cpp/file_system_mod.f90.s"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && /usr/bin/mpif90 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib/cpp/file_system_mod.f90 -o CMakeFiles/corelib.dir/cpp/file_system_mod.f90.s

# Object files for target corelib
corelib_OBJECTS = \
"CMakeFiles/corelib.dir/fortran/kinds_mod.f90.o" \
"CMakeFiles/corelib.dir/fortran/string/character_mod.f90.o" \
"CMakeFiles/corelib.dir/fortran/string/string_mod.f90.o" \
"CMakeFiles/corelib.dir/cpp/path_mod.f90.o" \
"CMakeFiles/corelib.dir/cpp/file_system_mod.f90.o"

# External object files for target corelib
corelib_EXTERNAL_OBJECTS =

libs/corelib/libcorelib.a: libs/corelib/CMakeFiles/corelib.dir/fortran/kinds_mod.f90.o
libs/corelib/libcorelib.a: libs/corelib/CMakeFiles/corelib.dir/fortran/string/character_mod.f90.o
libs/corelib/libcorelib.a: libs/corelib/CMakeFiles/corelib.dir/fortran/string/string_mod.f90.o
libs/corelib/libcorelib.a: libs/corelib/CMakeFiles/corelib.dir/cpp/path_mod.f90.o
libs/corelib/libcorelib.a: libs/corelib/CMakeFiles/corelib.dir/cpp/file_system_mod.f90.o
libs/corelib/libcorelib.a: libs/corelib/CMakeFiles/corelib.dir/build.make
libs/corelib/libcorelib.a: libs/corelib/CMakeFiles/corelib.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/DATA/disk3/cloud/Code/fortran/eampf/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Linking CXX static library libcorelib.a"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && $(CMAKE_COMMAND) -P CMakeFiles/corelib.dir/cmake_clean_target.cmake
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/corelib.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
libs/corelib/CMakeFiles/corelib.dir/build: libs/corelib/libcorelib.a
.PHONY : libs/corelib/CMakeFiles/corelib.dir/build

libs/corelib/CMakeFiles/corelib.dir/clean:
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib && $(CMAKE_COMMAND) -P CMakeFiles/corelib.dir/cmake_clean.cmake
.PHONY : libs/corelib/CMakeFiles/corelib.dir/clean

libs/corelib/CMakeFiles/corelib.dir/depend:
	cd /DATA/disk3/cloud/Code/fortran/eampf/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /DATA/disk3/cloud/Code/fortran/eampf/src /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corelib /DATA/disk3/cloud/Code/fortran/eampf/build /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corelib/CMakeFiles/corelib.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : libs/corelib/CMakeFiles/corelib.dir/depend

