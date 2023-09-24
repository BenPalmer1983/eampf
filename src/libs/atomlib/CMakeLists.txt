#####################################################
#    Sub directory libraries
#####################################################

# static atomlib
add_library(atomlib STATIC)

# Include corelib for mod files
target_include_directories(atomlib PRIVATE ${CMAKE_BINARY_DIR}/libs/corelib)

# Link corelib
target_link_libraries(atomlib corelib)

# Source files
target_sources(atomlib PRIVATE
    labels_mod.f90
    config_mod.f90
)



#####################################################
