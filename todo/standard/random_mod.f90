module random_mod

use mpi
use omp_lib
use kinds_mod

implicit none
private

public random_integer
public random_string

character(len=62), parameter, public ::            alphanum = "abcdefghijklmnopqrstuvwxyz" // &
                                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" // &
                                                            "0123456789"

!#############################################################################
contains
!#############################################################################


function random_integer(i_min, i_max)
  integer ::                                     random_integer
  integer ::                                     i_min
  integer ::                                     i_max
  real(real64) ::                                rn
  call random_number(rn)
  random_integer = i_min + floor((i_max + 1 - i_min) * rn)
end function random_integer


subroutine random_string(str_inout)
  character(len=*), intent(inout) ::             str_inout
  integer ::                                     n, rn
  do n=1, len(str_inout)
    rn = random_integer(1, len(alphanum))
    str_inout(n:n) = alphanum(rn:rn)
  end do
end subroutine random_string





end module random_mod