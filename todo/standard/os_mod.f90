module os_mod

use mpi
use omp_lib
use kinds_mod

implicit none
private

public ::                                        is_windows, is_linux

!#############################################################################
contains
!#############################################################################


function is_windows()
logical ::                                       is_windows
character(len=255) ::                            path_string
is_windows = .true.
call get_environment_variable("PATH", path_string)
if(path_string(1:1) == "/") is_windows = .false.
end function is_windows


function is_linux()
logical ::                                       is_linux
character(len=255) ::                            path_string
is_linux = .false.
call get_environment_variable("PATH", path_string)
if(path_string(1:1) == "/") is_linux = .true.
end function is_linux



end module os_mod