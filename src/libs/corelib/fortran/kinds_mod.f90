module kinds_mod

implicit none
private

public :: INT32, REAL32, REAL64
  
integer, parameter :: REAL32 = Selected_Real_Kind(6,37)      
integer, parameter :: REAL64 = Selected_Real_Kind(15,307)    ! ****
integer, parameter :: REAL128 = Selected_Real_Kind(33,4931)  ! 
integer, parameter :: INT8 = Selected_Int_Kind(1)            ! 
integer, parameter :: INT16 = Selected_Int_Kind(4)           !
integer, parameter :: INT32 = Selected_Int_Kind(8)           ! ****
integer, parameter :: INT64 = Selected_Int_Kind(12)          ! 
                
end module kinds_mod
