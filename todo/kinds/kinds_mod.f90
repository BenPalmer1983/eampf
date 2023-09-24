module kinds_mod

implicit none
private

public :: int32, real32, real64
  
integer, parameter :: real32 = Selected_Real_Kind(6,37)      
integer, parameter :: real64 = Selected_Real_Kind(15,307)    ! ****
integer, parameter :: real128 = Selected_Real_Kind(33,4931)  ! 
integer, parameter :: int8 = Selected_Int_Kind(1)            ! 
integer, parameter :: int16 = Selected_Int_Kind(4)           !
integer, parameter :: int32 = Selected_Int_Kind(8)           ! ****
integer, parameter :: int64 = Selected_Int_Kind(12)          ! 
                
end module kinds_mod
