module kinds_mod

implicit none
private

public :: int32, real32, real64
  
integer, PARAMETER :: real32 = Selected_Real_Kind(6,37)      ! single real, 6 decimal precision, exponent range 37
integer, PARAMETER :: real64 = Selected_Real_Kind(15,307)    ! double real, 15 decimal precision, exponent range 307
integer, PARAMETER :: real128 = Selected_Real_Kind(33,4931)  ! quadrupole real
integer, PARAMETER :: int8 = Selected_Int_Kind(1)            ! tiny integer         1 byte    -2^4 to 2^4-1   
integer, PARAMETER :: int16 = Selected_Int_Kind(4)           ! small integer        3 bytes  -2^16 to 2^16-1  
integer, PARAMETER :: int32 = Selected_Int_Kind(8)           ! standard integer     4 bytes  -2^31 to 2^31-1  
integer, PARAMETER :: int64 = Selected_Int_Kind(12)          ! long integer         8 bytes -2^63 to 2^63-1    
                
end module kinds_mod
