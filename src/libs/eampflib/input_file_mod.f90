module input_file_mod

! Modules to use
use kinds_mod, only :                            int32, real64
!use standard_mod, only :                         get_int
use character_mod, only :                        set_character
use file_system_mod, only :                      file_system_t, file_exists, get_cwd
use errors_mod
!use path_mod, only :                             file_exists
!use settings_mod, only :                         settings_g, &
!                                                 BUFFER_LEN

! Implicit & private throughout module
implicit none
private

! Types
type, public :: input_file_t
    logical ::                                     initialised = .false.
    logical ::                                     verbose = .false. 
    ! Directories
    character(len=:), allocatable ::               configs_dir
    character(len=:), allocatable ::               potential_dir
    ! Fitting
    

contains
    procedure, public ::                           init
    procedure, public ::                           reset
    procedure, public ::                           load
    ! Getters
    !procedure, public ::                           load

end type input_file_t

! Globals
type(input_file_t), public ::                    input_file_g


!###################################################################################
contains
!###################################################################################


subroutine init(this)
    class(input_file_t), intent(inout) ::          this
    if(.not. this%initialised) call this%reset()
end subroutine init


subroutine reset(this)
    class(input_file_t), intent(inout) ::          this
    this%initialised = .true.
end subroutine reset


subroutine load(this)
    class(input_file_t), intent(inout) ::          this
    character(len=255) ::                          configs = ""
    character(len=255) ::                          potential = ""
    integer ::                                     fh, status
    logical ::                                     exists
    !character(len=64) ::                           arg
    !integer ::                                     i
    namelist /DIRECTORIES/ configs, potential
  
    ! Initialise
    call this%init()
    
      ! Load
    exists = file_exists("input.nml")
    if(exists)then

    else
        call exception(ERR_FILE_MISSING)
    end if

    ! Check if input file exists
    !exists = file_exists(settings_g%input_file, .true.)

    ! Open and read Namelist file.
    !open(newunit=fh, action='read', file=settings_g%input_file, iostat=status)
    !read(unit=fh, nml=DIRECTORIES, iostat=status)
    !if (rc /= 0) write (stderr, '("Error: invalid Namelist format")')

    !call set_character(configs, this%configs_dir)
    !call set_character(potential, this%potential_dir)



end subroutine load





end module input_file_mod
