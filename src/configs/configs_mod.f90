module configs_mod

! Modules to use
use kinds_mod, only :                            int32, real64
use config_mod

! Implicit & private throughout module
implicit none
private

! Parameters
integer, private, parameter ::                   CONFIGS_MAX = 1000

! Types
type, public :: configs_t
  logical ::                                     initialised
  integer(kind=int32) ::                         count
  type(config_t), allocatable ::                 configs
contains
  procedure, public ::                           init
  procedure, public ::                           reset
  procedure, public ::                           load_files
end type configs_t

! Globals
type(configs_t), public ::                       configs_g


!###################################################################################
contains
!###################################################################################


subroutine init(this)
  class(configs_t) ::                            this
  if(.not. this%initialised) call this%reset()
end subroutine init


subroutine reset(this)
  class(configs_t), intent(inout) ::             this
  this%initialised = .true.
  !if(allocated(this%labels)) deallocate(this%labels)
end subroutine reset


subroutine load_files(this)
  class(configs_t), intent(inout) ::             this

  print *, "Configs"

  

end subroutine load_files







end module configs_mod
