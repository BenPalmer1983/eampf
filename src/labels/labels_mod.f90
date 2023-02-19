module labels_mod

! Modules to use
use kinds_mod, only :                  int32, real64

! Implicit & private throughout module
implicit none
private

! Parameters
integer, private, parameter ::                   LABELS_MAX = 1000

! Type
type :: labels_t
  logical ::                                     initialised
  character(len=128), allocatable ::             labels(:)
contains
  procedure, public ::                           init
  procedure, public ::                           reset
end type labels_t

! DATA
type(labels_t), public ::                        labels_g


!###################################################################################
contains
!###################################################################################


subroutine init(this)
  class(labels_t), intent(inout) ::              this
  if(.not. this%initialised) call this%reset()
end subroutine init


subroutine reset(this)
  class(labels_t), intent(inout) ::              this
  this%initialised = .true.
  if(allocated(this%labels)) deallocate(this%labels)
  allocate(this%labels(LABELS_MAX))
end subroutine reset





end module labels_mod