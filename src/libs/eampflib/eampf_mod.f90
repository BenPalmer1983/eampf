module eampf_mod

! Modules to use
use kinds_mod, only :                            INT32, REAL64
use banner_mod, only :                           banner_t
use configs_mod, only :                          configs_t
use args_mod, only :                             args_t
use input_file_mod, only :                       input_file_t
    
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
    
type(args_t) ::                                  args
type(banner_t) ::                                banner
type(input_file_t) ::                            input_file
type(configs_t) ::                               configs


! Read args
call args%load()


! Show banner
call banner%display()  


! Input files
call input_file%load()


! Load configs
!call configs



!path = fs%get_cwd()
!print *, path

!call fs%get_file_list("/cloud/Code/fortran/eampf/examples/test")
!print *, fs%get_file_count()
!do n = 1, fs%get_file_count()
!    print *, fs%get_file_path(n)
!end do
!call get_file_list(path)

!do n = 1, size(paths)
!    print *, trim(paths(n))
!end do
    
end subroutine run
    

end module eampf_mod