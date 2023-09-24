
#include <iostream>
#include <filesystem>
#include <cstring>
#include <vector>



extern "C"
{
    int get_file_list(const char* path, int paths_size, char* paths);
}




int get_file_list(const char* path, int paths_size, char* paths)
{
    std::string fileList;
    int n = 0;
    int paths_length = 0;
    for (const auto& entry : std::filesystem::directory_iterator(path)) 
    {
        if (entry.is_regular_file()) 
        {
            if(n == 0)
            {
                fileList += entry.path().string();
                paths_length = paths_length + entry.path().string().length();
            }
            else
            {
                fileList += "|" + entry.path().string();
                paths_length = paths_length + 1 + entry.path().string().length();
            }
            n = n + 1;
        }
    }

    std::strcpy(paths, fileList.c_str());
    std::cout << paths_length << std::endl;

    return paths_length;
}

