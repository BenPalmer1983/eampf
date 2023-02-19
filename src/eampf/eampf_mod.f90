module eampa_mod

use mpi
use omp_lib
use kinds_mod
use banner_mod, only :                           banner_t
use settings_mod, only :                         settings_g
use labels_mod, only :                           labels_g
use configs_mod, only :                          configs_g

implicit none
private

! Subroutines
public ::                                        run

  
!#############################################################################
contains
!#############################################################################



subroutine run()

  type(banner_t) ::                              banner

  ! print banner
  call banner%display()

  ! read arguments
  call settings_g%arguments()


  call configs_g%load_files()






end subroutine run







end module eampa_mod
