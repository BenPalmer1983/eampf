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
include libs/corecpplib/CMakeFiles/corecpplib.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include libs/corecpplib/CMakeFiles/corecpplib.dir/compiler_depend.make

# Include the progress variables for this target.
include libs/corecpplib/CMakeFiles/corecpplib.dir/progress.make

# Include the compile flags for this target's objects.
include libs/corecpplib/CMakeFiles/corecpplib.dir/flags.make

libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_cwd.cpp.o: libs/corecpplib/CMakeFiles/corecpplib.dir/flags.make
libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_cwd.cpp.o: /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corecpplib/path/get_cwd.cpp
libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_cwd.cpp.o: libs/corecpplib/CMakeFiles/corecpplib.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/DATA/disk3/cloud/Code/fortran/eampf/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_cwd.cpp.o"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corecpplib && /usr/bin/mpic++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_cwd.cpp.o -MF CMakeFiles/corecpplib.dir/path/get_cwd.cpp.o.d -o CMakeFiles/corecpplib.dir/path/get_cwd.cpp.o -c /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corecpplib/path/get_cwd.cpp

libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_cwd.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/corecpplib.dir/path/get_cwd.cpp.i"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corecpplib && /usr/bin/mpic++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corecpplib/path/get_cwd.cpp > CMakeFiles/corecpplib.dir/path/get_cwd.cpp.i

libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_cwd.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/corecpplib.dir/path/get_cwd.cpp.s"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corecpplib && /usr/bin/mpic++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corecpplib/path/get_cwd.cpp -o CMakeFiles/corecpplib.dir/path/get_cwd.cpp.s

libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_file_list.cpp.o: libs/corecpplib/CMakeFiles/corecpplib.dir/flags.make
libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_file_list.cpp.o: /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corecpplib/path/get_file_list.cpp
libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_file_list.cpp.o: libs/corecpplib/CMakeFiles/corecpplib.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/DATA/disk3/cloud/Code/fortran/eampf/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building CXX object libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_file_list.cpp.o"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corecpplib && /usr/bin/mpic++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_file_list.cpp.o -MF CMakeFiles/corecpplib.dir/path/get_file_list.cpp.o.d -o CMakeFiles/corecpplib.dir/path/get_file_list.cpp.o -c /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corecpplib/path/get_file_list.cpp

libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_file_list.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/corecpplib.dir/path/get_file_list.cpp.i"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corecpplib && /usr/bin/mpic++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corecpplib/path/get_file_list.cpp > CMakeFiles/corecpplib.dir/path/get_file_list.cpp.i

libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_file_list.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/corecpplib.dir/path/get_file_list.cpp.s"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corecpplib && /usr/bin/mpic++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corecpplib/path/get_file_list.cpp -o CMakeFiles/corecpplib.dir/path/get_file_list.cpp.s

# Object files for target corecpplib
corecpplib_OBJECTS = \
"CMakeFiles/corecpplib.dir/path/get_cwd.cpp.o" \
"CMakeFiles/corecpplib.dir/path/get_file_list.cpp.o"

# External object files for target corecpplib
corecpplib_EXTERNAL_OBJECTS =

libs/corecpplib/libcorecpplib.a: libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_cwd.cpp.o
libs/corecpplib/libcorecpplib.a: libs/corecpplib/CMakeFiles/corecpplib.dir/path/get_file_list.cpp.o
libs/corecpplib/libcorecpplib.a: libs/corecpplib/CMakeFiles/corecpplib.dir/build.make
libs/corecpplib/libcorecpplib.a: libs/corecpplib/CMakeFiles/corecpplib.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/DATA/disk3/cloud/Code/fortran/eampf/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Linking CXX static library libcorecpplib.a"
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corecpplib && $(CMAKE_COMMAND) -P CMakeFiles/corecpplib.dir/cmake_clean_target.cmake
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corecpplib && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/corecpplib.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
libs/corecpplib/CMakeFiles/corecpplib.dir/build: libs/corecpplib/libcorecpplib.a
.PHONY : libs/corecpplib/CMakeFiles/corecpplib.dir/build

libs/corecpplib/CMakeFiles/corecpplib.dir/clean:
	cd /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corecpplib && $(CMAKE_COMMAND) -P CMakeFiles/corecpplib.dir/cmake_clean.cmake
.PHONY : libs/corecpplib/CMakeFiles/corecpplib.dir/clean

libs/corecpplib/CMakeFiles/corecpplib.dir/depend:
	cd /DATA/disk3/cloud/Code/fortran/eampf/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /DATA/disk3/cloud/Code/fortran/eampf/src /DATA/disk3/cloud/Code/fortran/eampf/src/libs/corecpplib /DATA/disk3/cloud/Code/fortran/eampf/build /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corecpplib /DATA/disk3/cloud/Code/fortran/eampf/build/libs/corecpplib/CMakeFiles/corecpplib.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : libs/corecpplib/CMakeFiles/corecpplib.dir/depend
