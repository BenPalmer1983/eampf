module settings_mod

! Modules to use
use kinds_mod, only :                            int32, real64
use standard_mod, only :                         get_int
use character_mod, only :                        set_character

! Implicit & private throughout module
implicit none
private

! Parameters
character(len=*), parameter, public  ::          VERSION = '1.0'
integer, parameter, public ::                    BUFFER_LEN = 255

! Types
type, private :: settings_t
  logical ::                                     initialised = .false.
  logical ::                                     verbose = .false. 
  character(len=:), allocatable ::               input_file
contains
  procedure, public ::                           init
  procedure, public ::                           reset
  procedure, public ::                           arguments
  procedure, nopass, private ::                  display_banner
  procedure, nopass, private ::                  display_help
  procedure, nopass, private ::                  display_version
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
  character(len=64) ::                           arg
  character(len=64) ::                           buffer = ""
  integer ::                                     i
  
  ! Initialise
  call this%init()

  ! Banner
  call this%display_banner()
  
  do i = 1, command_argument_count()
    call get_command_argument(i, arg)
    !print *, i, arg
  end do
   
  ! Temporary set as input.nml
  buffer = "input.nml"
  call set_character(buffer, this%input_file)

  !print *, this%input_file

end subroutine arguments




subroutine display_banner()
write(*, '(a)') '#############################################################'
write(*, '(a)') '#              ░█▀▀▀ ░█▀▀█ ░█▀▄▀█ ░█▀▀█ ░█▀▀▀               #'
write(*, '(a)') '#              ░█▀▀▀ ░█▄▄█ ░█░█░█ ░█▄▄█ ░█▄▄                #'
write(*, '(a)') '#              ░█▄▄▄ ░█ ░█ ░█  ░█ ░█    ░█                  #'
write(*, '(a)') '#                                                           #'
write(*, '(a)') '#          Embedded Atom Method Potential Fitting           #'
write(*, '(a)') '#############################################################'
write(*, '(a)') ''
end subroutine display_banner


subroutine display_help()  
  write(*, *) "Help"
end subroutine display_help



subroutine display_version()
write(*, '(2a)') 'Version ', VERSION
end subroutine display_version


end module settings_mod
