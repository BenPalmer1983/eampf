#include <filesystem>
#include <cstring>
#include <iostream>


#include "file_exists.h"


int file_exists(char* path) 
{
    if(std::filesystem::exists(path))
    {
        return 1;
    }
    return 0;
}


