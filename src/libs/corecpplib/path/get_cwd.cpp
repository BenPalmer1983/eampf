#include <filesystem>
#include <cstring>
#include <iostream>

#include "get_cwd.h"

void get_cwd(char* path, int paths_size) 
{
    // Get path and convert to string
    std::filesystem::path current_path = std::filesystem::current_path();
    std::string current_path_string = current_path.string();

    int actual_size = std::min(static_cast<int>(current_path_string.size()), paths_size - 1);

    // Copy the C++ string to the character buffer
    std::strncpy(path, current_path_string.c_str(), actual_size);
    path[actual_size] = '\0'; // Null-terminate the string
}

