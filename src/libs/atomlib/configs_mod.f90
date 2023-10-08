module configs_mod

! Modules to use
use kinds_mod, only :                            int32, real64
use character_mod, only :                        set_character
use config_mod, only :                           config_t
!use path_mod, only :                             file_list
!use config_mod
!use input_file_mod, only :                       input_file_g

! Implicit & private throughout module
implicit none
private


! Types
type, public :: configs_t
    logical ::                                     initialised
    integer(kind=int32) ::                         count
    type(config_t), allocatable ::                 configs(:)
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










subroutine load_files(this, configs_path)
    class(configs_t), intent(inout) ::           this
    character(len=*), intent(in) ::              configs_path
    character(len=255), allocatable ::           config_list(:)
    integer ::                                   n




  ! Allocate array of configs
  !if(allocated(this%configs)) deallocate(this%configs)
  !allocate(this%configs(CONFIGS_MAX))

  ! Load config files into a list
  !call file_list(trim(input_file_g%configs_dir), config_list, .true.)

  !do n=1, size(config_list, 1)
  !  call this%configs(n)%load(trim(config_list(n)))
  !end do
  
  

end subroutine load_files







end module configs_mod
