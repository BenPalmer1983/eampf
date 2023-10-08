#####################################################
#    Core cpp library
#####################################################

# static corecpplib
add_library(corecpplib STATIC)

# Must include c++17 standard
set_target_properties(corecpplib PROPERTIES CXX_STANDARD 17)

# Source files
target_sources(corecpplib PRIVATE
    path/get_cwd.cpp
    path/get_file_list.cpp
    path/file_exists.cpp
)



#####################################################
