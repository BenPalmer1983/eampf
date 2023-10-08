module labels_mod

! Modules to use
use kinds_mod, only :                            int32, real64
use character_mod, only :                        lower_case
use string_mod, only :                           string_t

! Implicit & private throughout module
implicit none
private

! Parameters
integer, private, parameter ::                   LABELS_MAX = 1000


! Type
type :: labels_t
  logical ::                                     initialised
  type(string_t), allocatable ::                 labels(:)
  integer ::                                     counter = 0
  logical ::                                     case_sensitive = .false.
contains
  procedure, public ::                           init
  procedure, public ::                           reset
  procedure, public ::                           set => set_label
  procedure, public ::                           get => get_label
  procedure, public ::                           display => display_label
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
  this%counter = 0
  if(allocated(this%labels)) deallocate(this%labels)
  allocate(this%labels(LABELS_MAX))
end subroutine reset


function set_label(this, label)
  class(labels_t), intent(inout) ::              this
  integer ::                                     set_label
  character(len=*) ::                            label
  integer ::                                     n
  call this%init()
  if(.not. this%case_sensitive)then
    call lower_case(label)
  end if  
  if(this%counter == 0)then
    this%counter = this%counter + 1
    call this%labels(this%counter)%set(label)
    set_label = this%counter 
  else 
    set_label = -1
    do n = 1, this%counter
      if(this%labels(n)%is(label))then
        set_label = n
        exit
      end if
    end do
    if(set_label == -1)then
      this%counter = this%counter + 1
      call this%labels(this%counter)%set(label)
      set_label = this%counter 
    end if
  end if
end function set_label


function get_label(this, label)
  class(labels_t), intent(inout) ::              this
  integer ::                                     get_label
  character(len=*) ::                            label
  integer ::                                     n
  get_label = -1
  do n = 1, this%counter
    if(this%labels(n)%is(label))then
      get_label = n
      exit
    end if
  end do
end function get_label

subroutine display_label(this)
  class(labels_t), intent(in) ::                 this  
  integer ::                                     n
  do n = 1, this%counter
    print *, n, this%labels(n)%string
  end do
end subroutine display_label




end module labels_mod