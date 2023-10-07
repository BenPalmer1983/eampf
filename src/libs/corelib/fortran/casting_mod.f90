module casting_mod

use omp_lib
use kinds_mod

implicit none
private

public get_int

!#############################################################################
contains
!#############################################################################


function get_int(char_in)
    integer(kind=int32) ::      get_int
    character(len=*) ::         char_in
    read(char_in, '(i1)') get_int
end function get_int










end module casting_mod
