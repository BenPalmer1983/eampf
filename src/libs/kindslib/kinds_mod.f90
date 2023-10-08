module kinds_mod

implicit none
private

integer, parameter, public :: REAL32 = Selected_Real_Kind(6,37)
integer, parameter, public :: REAL64 = Selected_Real_Kind(15,307)
integer, parameter, public :: REAL128 = Selected_Real_Kind(33,4931) 

integer, parameter, public :: INT8 = Selected_Int_Kind(1) 
integer, parameter, public :: INT16 = Selected_Int_Kind(4)
integer, parameter, public :: INT32 = Selected_Int_Kind(8)
integer, parameter, public :: INT64 = Selected_Int_Kind(12)
integer, parameter, public :: INT128 = Selected_Int_Kind(32) 

end module kinds_mod
