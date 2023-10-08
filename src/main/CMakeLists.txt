#####################################################
#    Main EAMPF program
#####################################################

# Output binary
add_executable(EAMPF)

# Mod file locations
target_include_directories(EAMPF PRIVATE ${CMAKE_BINARY_DIR}/libs/eampflib)
target_include_directories(EAMPF PRIVATE ${CMAKE_BINARY_DIR}/libs/corelib)

# Libraries
target_link_libraries(EAMPF PRIVATE
    corelib    
    eampflib    
)

# Include main source file
target_sources(EAMPF PRIVATE
    main.f90
)




#####################################################
