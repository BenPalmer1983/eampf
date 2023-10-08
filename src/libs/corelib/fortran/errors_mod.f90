module errors_mod

use kinds_mod
    
implicit none
private
    
integer(INT32), parameter, public ::             ERR_FILE_MISSING = 101
integer(INT32), parameter, public ::             ERR_DIR_MISSING = 101

public :: exception

    
!#############################################################################
contains
!#############################################################################
    
    
subroutine exception(error_code, msg)
    integer, intent(in) ::                       error_code
    character(len=*), intent(in), optional ::    msg
    write(*, *) "EXCEPTION: error code ", error_code
    if(present(msg)) write(*, *) "Message: ", msg
    write(*, *) "STOPPING"
    stop
end subroutine exception    
    
    
    
    
    
    
    
end module errors_mod
    