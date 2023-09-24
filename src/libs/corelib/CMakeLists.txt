#####################################################
#    corelib
#####################################################

# static corelib
add_library(corelib STATIC)

# Link to corecpplib
target_link_libraries(corelib corecpplib)

# Source files
target_sources(corelib PRIVATE
    fortran/kinds_mod.f90

    fortran/string/character_mod.f90
    fortran/string/string_mod.f90

    cpp/path_mod.f90
    cpp/file_system_mod.f90
)


#####################################################
