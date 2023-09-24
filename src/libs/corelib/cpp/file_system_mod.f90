module file_system_mod

    !use ISO_C_binding, only:                         C_CHAR, C_INT, C_NULL_CHAR,  c_ptr
    use ISO_C_binding
    
    implicit none
    private
    
    
    
    !###########################
    ! Interfaces to C
    !###########################
    
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
    

! 
    type, public :: file_system_t
        character(255), allocatable ::           file_list(:)
        
    contains
        procedure, nopass, public ::             get_cwd
        procedure, public ::                     get_file_list
        procedure, public ::                     get_file_count
        procedure, public ::                     get_file_path
    end type file_system_t



        
!###################################################################################
contains
!###################################################################################
    
    function get_cwd()
        character(len=:), allocatable ::         get_cwd
        integer ::                               i, length, get_cwd_result
        character(255) ::                        c_path
        character(255) ::                        buffer
    
        call c_get_cwd(c_path, len(c_path))
        buffer = ""
            
        length = index(c_path, C_NULL_CHAR) - 1
    
        allocate(character(len=length) :: get_cwd)
        do i = 1, length
          get_cwd(i:i) = c_path(i:i)
        end do  
              
    end function get_cwd
    
    
    subroutine get_file_list(this, path)
        class(file_system_t) ::                  this
        character(len=*) ::                      path
        character(255) ::                        c_path
        character(1000000) ::                    c_paths
        integer(C_INT) ::                        paths_length
        character(255) ::                        buffer
        integer ::                               file_count
        integer ::                               n, m, k
      
        ! Call the C++ function to get the character array
        c_path = trim(path) // c_null_char
        paths_length = c_get_file_list(c_path, len(c_paths), c_paths)
    
        file_count = 1
        do n=1, paths_length
            if(c_paths(n:n) == "|")then
                file_count = file_count + 1
            end if
        end do
        
        if(allocated(this%file_list)) deallocate(this%file_list)
        allocate(this%file_list(file_count))
    
        do n=1, file_count
            this%file_list(n) = ""
        end do
    
        m = 1
        k = 1
        do n=1, paths_length
            if(c_paths(n:n) == "|")then
                m = m + 1
                k = 1
            else
                this%file_list(m)(k:k) = c_paths(n:n)
                k = k + 1
            end if
        end do
    end subroutine get_file_list


    function get_file_count(this)
        class(file_system_t) ::                  this
        integer ::                               get_file_count
        get_file_count = size(this%file_list)
    end function get_file_count


    function get_file_path(this, idx)
        class(file_system_t) ::                  this
        integer ::                               idx
        character(len=:), allocatable::          get_file_path

        allocate(character(len=len_trim(this%file_list(idx))) :: get_file_path)

        get_file_path = trim(this%file_list(idx))


    end function get_file_path
    
    


    
end module file_system_mod
    