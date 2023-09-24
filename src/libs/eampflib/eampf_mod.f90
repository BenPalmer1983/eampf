module eampf_mod

! Modules to use
use kinds_mod, only :                            INT32, REAL64
use banner_mod, only :                           banner_t
use file_system_mod, only :                      file_system_t
use character_mod, only : lower_case
    
! Implicit & private throughout module
implicit none
private
    
    
! 
type, public :: eampf_t
contains
    procedure, nopass, public ::                   run
end type eampf_t
    
    
!###################################################################################
contains
!###################################################################################
    
    
subroutine run()
    
type(banner_t) :: banner
type(file_system_t) :: fs
character(len=:), allocatable ::                            path
character(len=255), allocatable ::                          paths(:)
integer :: n

call banner%display()    

path = fs%get_cwd()
print *, path

call fs%get_file_list("/cloud/Code/fortran/eampf/examples/test")
print *, fs%get_file_count()
do n = 1, fs%get_file_count()
    print *, fs%get_file_path(n)
end do
!call get_file_list(path)

!do n = 1, size(paths)
!    print *, trim(paths(n))
!end do
    
end subroutine run
    

end module eampf_mod