#####################################################
#    Sub directory libraries
#####################################################

# static eampflib
add_library(eampflib STATIC)

# Include corelib for mod files
target_include_directories(eampflib PRIVATE ${CMAKE_BINARY_DIR}/libs/kindslib)
target_include_directories(eampflib PRIVATE ${CMAKE_BINARY_DIR}/libs/corelib)
target_include_directories(eampflib PRIVATE ${CMAKE_BINARY_DIR}/libs/atomlib)

# Link libs
target_link_libraries(eampflib kindslib)
target_link_libraries(eampflib atomlib)
target_link_libraries(eampflib corelib)
target_link_libraries(eampflib corecpplib)

# Source files for eampflib
target_sources(eampflib PRIVATE
    banner_mod.f90
    eampf_mod.f90
    input_file_mod.f90
)



#####################################################
