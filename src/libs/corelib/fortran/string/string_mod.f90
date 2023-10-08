module string_mod

use omp_lib
use kinds_mod
use character_mod, only :                        lower_case, upper_case, &
                                                 tab_to_space

implicit none
private

type, public :: string_t
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


!#############################################################################
contains
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



end module string_mod