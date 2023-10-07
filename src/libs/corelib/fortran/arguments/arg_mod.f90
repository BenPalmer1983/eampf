module arg_mod

    use kinds_mod
        
    implicit none
    private
        
    
    type, public :: arg_t
        character(len=:), allocatable ::         raw
        character(len=:), allocatable ::         flag
        character(len=:), allocatable ::         value
    contains
        procedure, public ::                     set
        procedure, public ::                     get_raw
    end type arg_t
    
      
    
        
    !#############################################################################
    contains
    !#############################################################################
        
        
    subroutine set(this, buffer)
        class(arg_t), intent(inout) ::           this
        character(len=*) ::                      buffer
        integer ::                               raw_len = 0
        
        if(allocated(this%raw)) deallocate(this%raw)
        if(allocated(this%flag)) deallocate(this%flag)
        if(allocated(this%value)) deallocate(this%value)


        raw_len = len_trim(buffer) 
        allocate(character(len=raw_len) :: this%raw)
        this%raw = trim(buffer)

    
    end subroutine set
        
        
        
    function get_raw(this)
        class(arg_t), intent(in) ::              this
        character(len=:), allocatable ::         get_raw
        
        if(allocated(get_raw)) deallocate(get_raw)
        allocate(character(len=len(this%raw)) :: get_raw)
        get_raw = this%raw

    end function get_raw    
        
        
        
        
end module arg_mod
        