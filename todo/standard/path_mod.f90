module path_mod

use mpi
use omp_lib
use kinds_mod
use os_mod, only :                               is_linux
use random_mod, only :                           random_string

implicit none
private

public ::                                        file_exists
public ::                                        temp_file_name
public ::                                        file_list
public ::                                        delete_file


!#############################################################################
contains
!#############################################################################


function file_exists(file_path, exit_prog)
  logical ::                                     file_exists
  character(len=*) ::                            file_path
  logical, optional ::                           exit_prog
  integer ::                                     status
  inquire (file=file_path, iostat=status,  exist=file_exists)
  if (.not. file_exists) then
    write (*, '("Error: ", a, " does not exist")') trim(file_path)
    file_exists = .false.
    if(present(exit_prog) .and. exit_prog) stop
  end if
end function file_exists



function temp_file_name()
  character(len=20) ::                           temp_file_name
  temp_file_name(:5) = ".tmp_"
  call random_string(temp_file_name(6:))
end function temp_file_name



subroutine file_list(dir_path, list, full)
  character(len=*), intent(in) ::                  dir_path
  character(len=255), allocatable, intent(inout) :: list(:)
  logical, optional, intent(in) ::                 full
  logical ::                                       os_linux
  if(is_linux())then
    call file_list_linux(dir_path, list, full)
  else
    call file_list_win32(dir_path, list, full)
  end if
end subroutine file_list



subroutine file_list_linux(dir_path, list, full)
  character(len=*), intent(in) ::                  dir_path
  character(len=255), allocatable, intent(inout) :: list(:)
  logical, optional, intent(in) ::                 full
  character(len=255) ::                            buffer
  character(len=255) ::                            cwd
  character(len=20) ::                             temp_file
  integer ::                                       fh, ierr
  integer ::                                       file_count
  integer ::                                       n
  integer ::                                       dir_path_len

  if(allocated(list)) deallocate(list)

  temp_file = temp_file_name()
  call system('ls ' // dir_path // ' > ' // temp_file)

  open(newunit=fh, file=temp_file, action="read")
  n = 0
  do
    read(fh, fmt='(a)', iostat=ierr) buffer
    if (ierr /= 0) exit
    n = n + 1
  end do
  close(fh)

  allocate(list(n))
  open(newunit=fh, file=temp_file, action="read")
  n = 0
  do
    read(fh, fmt='(a)', iostat=ierr) buffer
    if (ierr /= 0) exit
    n = n + 1
    list(n) = buffer
  end do
  close (fh, status="delete")

  dir_path_len = len_trim(adjustl(dir_path))
  if(present(full) .and. full)then
    call getcwd(cwd)
    do n=1, size(list)
      buffer = list(n)
      if(dir_path_len == 0)then
        list(n) = cwd // "/" // buffer
      else if(dir_path_len > 0 .and. dir_path(1:1) == "/")then
        list(n) = trim(dir_path) // "/" // trim(buffer)
      else if(dir_path_len > 0 .and. dir_path(1:1) .ne. "/")then
        list(n) = trim(cwd) // "/" // trim(dir_path) // "/" // trim(buffer)
      end if
    end do
  end if

end subroutine file_list_linux



subroutine file_list_win32(dir_path, list, full)
  character(len=*), intent(in) ::                  dir_path
  character(len=255), allocatable, intent(inout) :: list(:)
  logical, optional, intent(in) ::                 full

end subroutine file_list_win32



subroutine delete_file(file_path)
  character(len=*), intent(in) ::                  file_path
  integer ::                                       fh, ierr
  open(newunit=fh, file=file_path, status="old", iostat=ierr)
  if (ierr == 0) close (fh, status="delete")
end subroutine delete_file














end module path_mod