module eampf_mod

use mpi
use omp_lib
use kinds_mod
use random_mod, only :                           random_integer, random_string
use os_mod, only :                               is_windows
use path_mod, only :                             temp_file_name, file_list
use settings_mod, only :                         settings_g
use input_file_mod, only :                       input_file_g
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

  ! read arguments
  call settings_g%arguments()

  ! load input file
  call input_file_g%load()

  ! load configs
  call configs_g%load_files()



end subroutine run







end module eampf_mod
