! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

module test_m
use easifemBase
use easifemClasses
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test0
  type(FortranFile_) :: obj
  call obj%initiate(file="./example.txt", status='REPLACE', &
    & action='WRITE')
  call obj%open()
  call obj%close()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type(FortranFile_) :: obj
  call obj%initiate(file="./hello/world.txt", status='REPLACE', &
    & action='WRITE')
  call display(obj%getFilePath(), "Path")
  call display(obj%getFileExt(), "Ext")
  call display(obj%getFileName(), "FileName")
  call obj%open()
  call obj%close()
end subroutine

end module test_m

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_m
implicit none
call test0
end program main