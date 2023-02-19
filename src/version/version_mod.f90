module version_mod

! Modules to use
use kinds_mod, only :                            int32, real64

! Implicit & private throughout module
implicit none
private


! Parameters
character(len=*), parameter, public  ::          VERSION = '1.0'


type, public :: version_t
contains
  procedure, nopass, public ::                   display
end type version_t


!###################################################################################
contains
!###################################################################################


subroutine display()

write(*, '(2a)') 'Version ', VERSION

end subroutine display


end module version_mod
