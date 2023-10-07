module args_mod

use kinds_mod
use arg_mod, only :                              arg_t
    
implicit none
private
    

! 
type, public :: args_t
    type(arg_t), allocatable ::              arg_list(:)
contains
    procedure, public ::                     load
end type args_t




    
!#############################################################################
contains
!#############################################################################
    
    
subroutine load(this)
    class(args_t) ::                             this
    integer ::                                   n
    integer ::                                   arg_count
    character(len=255) ::                        buffer
    integer ::                                   buffer_length
    type(arg_t) ::                               new_arg

    ! get arg count
    arg_count = COMMAND_ARGUMENT_COUNT()

    ! allocate
    if(allocated(this%arg_list)) deallocate(this%arg_list)
    allocate(this%arg_list(arg_count))

    do n = 1, arg_count
        call GET_COMMAND_ARGUMENT(n, buffer, buffer_length)
        call new_arg%set(buffer)
        write(*, *) new_arg%get_raw()

    end do

end subroutine load
    
    
    
    
    
    
    
    
    
    
end module args_mod
    