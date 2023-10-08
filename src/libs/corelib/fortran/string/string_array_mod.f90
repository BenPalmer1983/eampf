module string_array_mod

    use omp_lib
    use kinds_mod
    use character_mod, only :                    lower_case, upper_case, &
                                                 tab_to_space
    use string_mod, only :                       string_t
    
    implicit none
    private
        
    type, public :: string_array_t
      type(string_t), allocatable ::             strings(:)
    contains
        ! Setters
        procedure, public ::                     set => set_string_array
        ! Getters
        procedure, public ::                     size => get_size
        ! Other
        procedure, public ::                     split => split_string
        procedure, public ::                     integer => string_array_get_integer
        procedure, public ::                     real => string_array_get_real
        procedure, public ::                     string => string_array_get_string
        procedure, public ::                     print => print_string_array
    end type string_array_t
    
!#############################################################################
contains
!#############################################################################
    
    ! Setters
    !#####################################
    
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


    ! Getters
    !#####################################

    function get_size(this)
        class(string_array_t) ::                  this
        integer ::                                get_size
        get_size = size(this%strings)
    end function get_size


    ! Other
    !#####################################
    
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
      do n = 1, size(this%strings)
        call this%strings(n)%print()
      end do  
    end subroutine print_string_array
    
    
  
    
    
    
end module string_array_mod