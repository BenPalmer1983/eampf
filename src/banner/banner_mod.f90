module banner_mod

! Modules to use
use kinds_mod, only :                            int32, real64

! Implicit & private throughout module
implicit none
private


! 
type, public :: banner_t
contains
  procedure, nopass, public ::                   display
end type banner_t


!###################################################################################
contains
!###################################################################################


subroutine display()

write(*, '(a)') '#############################################################'
write(*, '(a)') '#              ░█▀▀▀ ░█▀▀█ ░█▀▄▀█ ░█▀▀█ ░█▀▀▀               #'
write(*, '(a)') '#              ░█▀▀▀ ░█▄▄█ ░█░█░█ ░█▄▄█ ░█▄▄                #'
write(*, '(a)') '#              ░█▄▄▄ ░█ ░█ ░█  ░█ ░█    ░█                  #'
write(*, '(a)') '#                                                           #'
write(*, '(a)') '#          Embedded Atom Method Potential Fitting           #'
write(*, '(a)') '#############################################################'
write(*, '(a)') ''

end subroutine display


end module banner_mod