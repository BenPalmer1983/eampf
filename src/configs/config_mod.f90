module config_mod

use kinds_mod, only :                  int32, real64

implicit none
private


public :: config_t


type :: config_t
  logical ::                                     initialised = .false.
  integer(kind=int32) ::                         atoms = 0
  integer(kind=int32), allocatable ::            id(:)
  real(kind=real64), allocatable ::              xyz(:, :)
  real(kind=real64), allocatable ::              nl_r(:, :)
contains
  procedure, public ::                           init
  procedure, public ::                           reset
  procedure, public ::                           load_from_file
  procedure, public ::                           calc_nl
end type config_t


!###################################################################################
contains
!###################################################################################


subroutine init(this)
  class(config_t), intent(inout) ::   this
  if(.not. this%initialised) call this%reset()
end subroutine init


subroutine reset(this)
  class(config_t), intent(inout) ::   this
  this%initialised = .false.
  this%atoms = 0
  if(allocated(this%id)) deallocate(this%id)
  if(allocated(this%xyz)) deallocate(this%xyz)
end subroutine reset


subroutine load_from_file(this)
  class(config_t), intent(inout) ::   this
end subroutine load_from_file


subroutine calc_nl(this)
  class(config_t), intent(inout) ::   this
end subroutine calc_nl



end module config_mod
