module config_mod

use kinds_mod, only :                            int32, real64
use character_mod, only :                        set_character
use character_mod, only :                        remove_comments
use string_mod, only :                           string_t
use string_array_mod, only :                     string_array_t
use labels_mod, only :                           labels_g

implicit none
private


public :: config_t


type :: config_t
  logical ::                                     initialised = .false.
  character(len=:), allocatable ::               file_path
  ! Units
  character(len=8) ::                            length_units = "angs"
  character(len=8) ::                            energy_units = "ev"
  character(len=8) ::                            force_units = "gpa"
  character(len=8) ::                            stress_units = "ev/ang"
  ! Basis vectors
  real(kind=real64) ::                           alat = 0.0_real64 
  real(kind=real64) ::                           uv(1:3, 1:3) = 0.0_real64 
  real(kind=real64) ::                           rcut = 0.0_real64 
  ! Atoms
  integer(kind=int32) ::                         atoms = 0
  integer(kind=int32), allocatable ::            id(:)
  real(kind=real64), allocatable ::              xyz(:, :)
  integer(kind=int32), allocatable ::            nl_id(:)
  real(kind=real64), allocatable ::              nl_r(:)
  real(kind=real64), allocatable ::              nl_rvec(:, :)
  ! Energy, Forces, Stress
  real(kind=real64) ::                           energy_known = 0.0_real64
  real(kind=real64), allocatable ::              forces_known(:, :)
  real(kind=real64) ::                           stress_known(1:3, 1:3) = 0.0_real64 
  real(kind=real64) ::                           energy_calculated = 0.0_real64
  real(kind=real64), allocatable ::              forces_calculated(:, :)
  real(kind=real64) ::                           stress_calculated(1:3, 1:3) = 0.0_real64
contains
  procedure, public ::                           init
  procedure, public ::                           reset
  procedure, public ::                           load
  procedure, public ::                           calc_nl
end type config_t


!###################################################################################
contains
!###################################################################################


subroutine init(this)
  class(config_t), intent(inout) ::              this
  if(.not. this%initialised) call this%reset()
end subroutine init


subroutine reset(this)
  class(config_t), intent(inout) ::              this
  this%initialised = .false.
  this%atoms = 0
  if(allocated(this%id)) deallocate(this%id)
  if(allocated(this%xyz)) deallocate(this%xyz)
  if(allocated(this%nl_id)) deallocate(this%nl_id)
  if(allocated(this%nl_r)) deallocate(this%nl_r)
  if(allocated(this%nl_rvec)) deallocate(this%nl_rvec)
end subroutine reset


subroutine load(this, file_path)
  class(config_t), intent(inout) ::              this
  character(len=*) ::                            file_path
  !integer(kind=int32), allocatable ::            id_temp(:)
  !real(kind=real64), allocatable ::              xyz_temp(:, :)
  !real(kind=real64), allocatable ::              forces_temp(:, :)
  integer ::                                     n, cn
  integer ::                                     fh, ierr
  character(len=255) ::                          buffer
  character(len=8) ::                            label_temp
  real(kind=real64) ::                           a, b, c, d, e
  type(string_array_t) ::                        line

  ! Initialise
  call this%init()

  ! Set file path
  call set_character(trim(file_path), this%file_path)

  ! Count number of atoms
  open(newunit=fh, file=this%file_path, action="read")
  n = 0
  do
    read(fh, fmt='(a)', iostat=ierr) buffer
    if (ierr /= 0) exit
    if(len_trim(buffer) .ne. 0)then
      cn = ichar(buffer(1:1))
      if((cn >= 65 .and. cn <= 90) .or. (cn >= 97 .and. cn <= 112))then
        n = n + 1
      end if
    end if
  end do
  close (fh)
  this%atoms = n

  ! Allocate
  !if(allocated(id_temp)) deallocate(id_temp)
  !if(allocated(xyz_temp)) deallocate(xyz_temp)
  !if(allocated(forces_temp)) deallocate(forces_temp)
  !allocate(id_temp(n))
  !allocate(xyz_temp(n, 1:3))
  !allocate(forces_temp(n, 1:3))
  if(allocated(this%id)) deallocate(this%id)
  if(allocated(this%xyz)) deallocate(this%xyz)
  if(allocated(this%forces_known)) deallocate(this%forces_known)
  allocate(this%id(this%atoms))
  allocate(this%xyz(this%atoms, 1:3))

  ! Read data
  open(newunit=fh, file=this%file_path, action="read")
  n = 0
  do
    read(fh, fmt='(a)', iostat=ierr) buffer
    call remove_comments(buffer)
    if (ierr /= 0) exit
    if(buffer(:8) .eq. "#L_UNITS")then
      this%length_units = trim(adjustl(buffer(9:)))
    else if(buffer(:8) .eq. "#E_UNITS")then
      this%energy_units = trim(adjustl(buffer(9:)))
    else if(buffer(:8) .eq. "#F_UNITS")then
      this%force_units = trim(adjustl(buffer(9:)))
    else if(buffer(:8) .eq. "#S_UNITS")then
      this%stress_units = trim(adjustl(buffer(9:)))
    else if(buffer(:8) .eq. "#EPA")then
          
    ! If an atom line
    else if(len_trim(buffer) .ne. 0)then
      cn = ichar(buffer(1:1))
      if((cn >= 65 .and. cn <= 90) .or. (cn >= 97 .and. cn <= 112))then
        call line%split(buffer) 
        if(line%size() .eq. 4)then
          n = n + 1
          this%id(n) = labels_g%set(line%string(1))
          this%xyz(n, 1) = line%real(2)
          this%xyz(n, 2) = line%real(3)
          this%xyz(n, 3) = line%real(4)
        else if(line%size() .eq. 7)then
          n = n + 1
          if(.not. allocated(this%forces_known)) allocate(this%forces_known(this%atoms, 1:3))
          this%id(n) = labels_g%set(line%string(1))
          this%xyz(n, 1) = line%real(2)
          this%xyz(n, 2) = line%real(3)
          this%xyz(n, 3) = line%real(4)
          this%forces_known(n, 1) = line%real(5)
          this%forces_known(n, 2) = line%real(6)
          this%forces_known(n, 3) = line%real(7)
        end if
        !call split_string(buffer, atom_line)
      end if   
    
    end if
  end do
  close (fh)

  !call test2%split("This string    word1 2 3 532.668e2")
  !call test2%print()
  !print *, test2%real(6)
  !call test2%set("This string 2")
  !call test1%print()

  call labels_g%display()
end subroutine load


subroutine calc_nl(this)
  class(config_t), intent(inout) ::   this
end subroutine calc_nl



end module config_mod
