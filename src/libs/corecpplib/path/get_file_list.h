#ifndef GET_FILE_LIST_H
#define GET_FILE_LIST_H

extern "C"
{
    int get_file_list(const char* path, int paths_size, char* paths);
}

#endif