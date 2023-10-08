program eampf

call main()

contains

subroutine main()

use eampf_mod, only :                  eampf_t 

implicit none

type(eampf_t) :: eampf_o

call eampf_o%run()



end subroutine main

end program eampf
