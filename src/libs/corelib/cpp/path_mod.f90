module path_mod

!use ISO_C_binding, only:                         C_CHAR, C_INT, C_NULL_CHAR,  c_ptr
  use ISO_C_binding

implicit none
private


public get_cwd, get_file_list



interface
    subroutine c_get_cwd(path, path_size) bind(C, name="get_cwd")
        use iso_c_binding, only:                     C_INT, C_CHAR
        character(c_char), dimension(*) :: path
        integer(c_int), value :: path_size
    end subroutine c_get_cwd
end interface


interface
    function c_get_file_list(path, paths_size, paths) bind(C, name="get_file_list")
        use iso_c_binding, only:                     C_INT, C_CHAR
        character(c_char) :: path
        integer(c_int), value :: paths_size
        character(c_char), dimension(*) :: paths
        integer(c_int) :: c_get_file_list
    end function c_get_file_list
end interface

    
contains

function get_cwd()
    character(len=:), allocatable ::               get_cwd
    integer ::                                   i, length, get_cwd_result
    character(255) ::     c_path
    character(255) ::     buffer

    call c_get_cwd(c_path, len(c_path))
    buffer = ""
        
    length = index(c_path, C_NULL_CHAR) - 1

    allocate(character(len=length) :: get_cwd)
    do i = 1, length
      get_cwd(i:i) = c_path(i:i)
    end do  
          
end function get_cwd


function get_file_list(path)
    character(len=*) ::                          path
    character(255) ::     c_path
    character(1000000) ::     c_paths
    integer(C_INT) ::     paths_length
    character(255) ::     buffer
    character(255), allocatable ::    get_file_list(:)
    integer :: file_count
    integer :: n, m, k
  
    ! Call the C++ function to get the character array
    c_path = "/cloud/Code/fortran/eampf/examples/test" // c_null_char
    paths_length = c_get_file_list(c_path, len(c_paths), c_paths)

    file_count = 1
    do n=1, paths_length
        if(c_paths(n:n) == "|")then
            file_count = file_count + 1
        end if
    end do

    print *, file_count
    print *, paths_length

    print *, "#",c_paths(1:paths_length),"#"

    if(allocated(get_file_list)) deallocate(get_file_list)
    allocate(get_file_list(file_count))

    do n=1, file_count
        get_file_list(n) = ""
    end do

    m = 1
    k = 1
    do n=1, paths_length
        if(c_paths(n:n) == "|")then
            m = m + 1
            k = 1
        else
            get_file_list(m)(k:k) = c_paths(n:n)
            k = k + 1
        end if
    end do


end function get_file_list




end module path_mod
