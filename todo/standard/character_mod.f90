module character_mod

use mpi
use omp_lib
use kinds_mod

implicit none
private

public ::                                        lower_case, upper_case
public ::                                        tab_to_space
public ::                                        set_character
public ::                                        remove_comments


!#############################################################################
contains
!#############################################################################


subroutine lower_case(str_inout)
  character(len=*), intent(inout) ::             str_inout
  integer ::                                     c, n
  do n = 1, len(str_inout)
    c = ichar(str_inout(n:n))
    if(c >= 65 .and. c <= 90) str_inout(n:n) = char(c + 32)    
  end do
end subroutine lower_case


subroutine upper_case(str_inout)
  character(len=*), intent(inout) ::             str_inout
  integer ::                                     c, n
  do n = 1, len(str_inout)
    c = ichar(str_inout(n:n))
    if(c >= 97 .and. c <= 122) str_inout(n:n) = char(c - 32)    
  end do
end subroutine upper_case


subroutine tab_to_space(str_inout)
  character(len=*), intent(inout) ::             str_inout
  integer ::                                     c, n
  do n = 1, len(str_inout)
    c = ichar(str_inout(n:n))
    if(c == 9) str_inout(n:n) = " "    
  end do
end subroutine tab_to_space


subroutine set_character(inp_str, out_str, strip)
  character(len=*), intent(in) ::                 inp_str
	character(len=:), allocatable, intent(inout) :: out_str
	character(len=len(inp_str)) ::                  buffer
  logical, optional ::                            strip
  integer ::                                      n
  buffer = inp_str
  n = len(buffer)
  if(present(strip) .and. strip .eqv. .true.)then
    buffer = trim(adjustl(buffer))
    n = len_trim(buffer)
  end if
  if(allocated(out_str)) deallocate(out_str)
  allocate(character(len=n) :: out_str)
  out_str = buffer(:n)
end subroutine set_character


subroutine remove_comments(str)
character(len=*), intent(inout) ::               str
integer ::                                       n
logical ::                                       blank = .false.
blank = .false.
do n = 1, len(str)
  if(str(n:n) == "!")then
    blank = .true.
  else if(n < len(str))then
    if(str(n:n+1) == "//")then
       blank = .true.
    end if
  end if
  if(blank) str(n:n) = " "
end do
end subroutine remove_comments


end module character_mod