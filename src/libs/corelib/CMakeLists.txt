#####################################################
#    corelib
#####################################################

# static corelib
add_library(corelib STATIC)


# Include corelib for mod files
target_include_directories(corelib PRIVATE ${CMAKE_BINARY_DIR}/libs/kindslib)

# Link to corecpplib
target_link_libraries(corelib kindslib)
target_link_libraries(corelib corecpplib)

# Source files
target_sources(corelib PRIVATE

    fortran/casting_mod.f90
    fortran/errors_mod.f90

    fortran/string/character_mod.f90
    fortran/string/string_mod.f90
    fortran/string/string_array_mod.f90

    fortran/arguments/args_mod.f90
    fortran/arguments/arg_mod.f90


    cpp/path_mod.f90
    cpp/file_system_mod.f90
)

# Link corelib

#####################################################
