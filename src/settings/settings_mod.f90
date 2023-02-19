module settings_mod

! Modules to use
use kinds_mod, only :                            int32, real64
use standard_mod, only :                         get_int
use version_mod, only :                          VERSION, version_t

! Implicit & private throughout module
implicit none
private


! Types
type, private :: settings_t
  logical ::                                     initialised = .false.
  integer ::                                     verbose = .false. 
  character ::                                   input_file = ""
contains
  procedure, public ::                           init
  procedure, public ::                           reset
  procedure, public ::                           arguments
  procedure, public ::                           help
end type settings_t



! Globals
type(settings_t), public ::                      settings_g


!###################################################################################
contains
!###################################################################################


subroutine init(this)
  class(settings_t), intent(inout) ::            this
  if(.not. this%initialised) call this%reset()
end subroutine init


subroutine reset(this)
  class(settings_t), intent(inout) ::            this
  this%initialised = .true.
end subroutine reset


subroutine arguments(this)
  class(settings_t), intent(inout) ::            this
  type(version_t) ::                             version
  character(len=64) ::                           arg
  integer ::                                     i
  
  ! Initialise
  call this%init()

  
  do i = 1, command_argument_count()
    call get_command_argument(i, arg)

    select case (arg)
      case ('-v', '--version')
        call version%display()
        stop

      case ('-V', '--verbose')
        this%verbose = get_int("3")
        

      case ('-h', '--help')
        call this%help()
        stop

      case ('-i', '--input')
        print *, "input"

      case default
        this%input_file = arg
    end select
  end do

  print *, input_file

end subroutine arguments



subroutine help(this)
  class(settings_t), intent(inout) ::            this
  
  write(*, *) "Help"

end subroutine help


end module settings_mod
