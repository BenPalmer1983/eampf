module string_mod

use mpi
use omp_lib
use kinds_mod
use character_mod, only :                        lower_case, upper_case, &
                                                 tab_to_space

implicit none
private

public ::                                        string_t
public ::                                        string_array_t

type :: string_t
  character(len=:), allocatable ::               string
contains
  procedure, public ::                           set => set_string
  procedure, public ::                           lower => string_lower_case
  procedure, public ::                           upper => string_upper_case
  procedure, public ::                           integer => string_get_integer
  procedure, public ::                           real => string_get_real
  !procedure, public ::                           string => string_get_string
  procedure, public ::                           print => print_string
  generic, public ::                             is => is_character, is_string
  procedure, private ::                          is_character
  procedure, private ::                          is_string
end type string_t

type :: string_array_t
  integer ::                                     counter
  type(string_t), allocatable ::                 strings(:)
contains
  procedure, public ::                           set => set_string_array
  procedure, public ::                           split => split_string
  procedure, public ::                           integer => string_array_get_integer
  procedure, public ::                           real => string_array_get_real
  procedure, public ::                           string => string_array_get_string
  procedure, public ::                           print => print_string_array
end type string_array_t

!#############################################################################
contains
!#############################################################################


!#############################################################################
! String
!#############################################################################

subroutine set_string(this, inp_str, strip)
  class(string_t) ::                             this
  character(len=*), intent(in) ::                inp_str
  logical, optional ::                           strip
	character(len=len(inp_str)) ::                 buffer
  integer ::                                     n
  ! Copy to buffer
  buffer = inp_str
  n = len(buffer)
  if(present(strip) .and. strip .eqv. .true.)then
    buffer = trim(adjustl(buffer))
    n = len_trim(buffer)
  end if
  ! Allocate string
  if(allocated(this%string)) deallocate(this%string)
  allocate(character(len=n) :: this%string)
  ! Copy string
  this%string = buffer(:n)
end subroutine set_string


subroutine string_lower_case(this)
  class(string_t) ::                             this
  call lower_case(this%string)
end subroutine string_lower_case


subroutine string_upper_case(this)
  class(string_t) ::                             this
  call upper_case(this%string)
end subroutine string_upper_case


function string_get_integer(this)
  class(string_t) ::                             this
  integer ::                                     string_get_integer
  read(this%string, "(I8)") string_get_integer
end function string_get_integer


function string_get_real(this)
  class(string_t) ::                             this
  real(real64) ::                                string_get_real
  read(this%string, "(F16.8)") string_get_real
end function string_get_real


function string_get_string(this)
  class(string_t) ::                             this
  character(len=:), allocatable ::               string_get_string
  allocate(character(len=len(this%string)) :: string_get_string)
  string_get_string = this%string
end function string_get_string


subroutine print_string(this)
  class(string_t) ::                             this
  print *, this%string
end subroutine print_string


function is_character(this, str_in)
  class(string_t) ::                             this
  logical ::                                     is_character
  character(len=*) ::                            str_in
  is_character = .false.
  if(trim(this%string) == trim(str_in)) is_character = .true.
end function is_character


function is_string(this, str_in)
  class(string_t) ::                             this
  logical ::                                     is_string
  type(string_t) ::                              str_in
  is_string = .false.
  if(trim(this%string) == trim(str_in%string)) is_string = .true.
end function is_string


!#############################################################################
! String array
!#############################################################################

subroutine set_string_array(this, str_a, str_b, str_c, strip)
  class(string_array_t) ::                       this
  character(len=*), intent(in) ::                str_a
  character(len=*), optional, intent(in) ::      str_b
  character(len=*), optional, intent(in) ::      str_c
  logical, optional, intent(in) ::               strip
	!character(len=len(inp_str)) ::                 buffer
  !integer ::                                     n
  ! Copy to buffer
  
end subroutine set_string_array


subroutine split_string(this, str)
  class(string_array_t) ::                       this
  character(len=*), intent(in) ::                str
	character(len=len(str)) ::                     str_buffer
	character(len=len(str)) ::                     buffer
  character(len=1) ::                            sep = " "
  character(len=1) ::                            last_char = char(0)
  integer ::                                     n, counter, buffer_len

  str_buffer = str
  call tab_to_space(str_buffer)

  ! Count how many splits in string
  counter = 1
  last_char = char(0)
  do n=1, len_trim(str_buffer)
    if(str_buffer(n:n) == sep .and. last_char .ne. sep)then
      counter = counter + 1
    end if
    last_char = str_buffer(n:n)
  end do

  ! Allocate array
  this%counter = counter
  if(allocated(this%strings)) deallocate(this%strings)
  allocate(this%strings(counter))

  ! Read in
  counter = 1
  last_char = char(0)
  buffer = ""
  buffer_len = 0
  do n=1, len_trim(str_buffer)
    if(str_buffer(n:n) == sep .and. last_char .ne. sep)then
      ! Store buffer
      if(allocated(this%strings(counter)%string)) deallocate(this%strings(counter)%string)
      allocate(character(len=buffer_len) :: this%strings(counter)%string)
      this%strings(counter)%string(:buffer_len) = buffer(:buffer_len)
      buffer = ""
      buffer_len = 0
      counter = counter + 1
    else if(str_buffer(n:n) .ne. sep)then
      buffer_len = buffer_len + 1
      buffer(buffer_len:buffer_len) = str_buffer(n:n)
    end if
    last_char = str_buffer(n:n)
  end do
  ! Store final buffer
  if(allocated(this%strings(counter)%string)) deallocate(this%strings(counter)%string)
  allocate(character(len=buffer_len) :: this%strings(counter)%string)
  this%strings(counter)%string(:buffer_len) = buffer(:buffer_len)

end subroutine split_string


function string_array_get_integer(this, n)
  class(string_array_t) ::                       this
  integer ::                                     string_array_get_integer
  integer ::                                     n
  string_array_get_integer = this%strings(n)%integer()
end function string_array_get_integer


function string_array_get_real(this, n)
  class(string_array_t) ::                       this
  real(real64) ::                                string_array_get_real
  integer ::                                     n
  string_array_get_real = this%strings(n)%real()
end function string_array_get_real


function string_array_get_string(this, n)
  class(string_array_t) ::                             this
  character(len=:), allocatable ::               string_array_get_string
  integer ::                                     n
  allocate(character(len=len(this%strings(n)%string)) :: string_array_get_string)
  string_array_get_string = this%strings(n)%string
end function string_array_get_string


subroutine print_string_array(this)
  class(string_array_t) ::                       this
  integer ::                                     n
  do n = 1, this%counter
    call this%strings(n)%print()
  end do  
end subroutine print_string_array












end module string_mod