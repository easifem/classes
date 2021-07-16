
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
  type( domain_ ) :: dom
  type( HDF5File_ ) :: meshfile
  call display( "Testing getTotalNodes" )
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call dom%initiate( meshfile )
  call display( dom%getTotalNodes(), "102=" )
  call display( dom%getTotalNodes(physicalName=string("bottom")), "9=" )
  call display( dom%getTotalNodes(physicalName=string("right")), "9=" )
  call display( dom%getTotalNodes(physicalName=string("top")), "9=" )
  call display( dom%getTotalNodes(physicalName=string("left")), "9=" )
  call display( dom%getTotalNodes(physicalName=string("region_1")), "56=" )
  call display( dom%getTotalNodes(physicalName=string("region_2")), "55=" )
  call display( dom%getTotalNodes(physicalTag=1, dim=1), "9=" )
  call display( dom%getTotalNodes(physicalTag=2, dim=1), "9=" )
  call display( dom%getTotalNodes(physicalTag=3, dim=1), "9=" )
  call display( dom%getTotalNodes(physicalTag=4, dim=1), "9=" )
  call display( dom%getTotalNodes(physicalTag=1, dim=2), "56=" )
  call display( dom%getTotalNodes(physicalTag=2, dim=2), "55=" )
  call display( dom%getTotalNodes(entityNum=1, dim=1), "9=" )
  call display( dom%getTotalNodes(entityNum=2, dim=1), "9=" )
  call display( dom%getTotalNodes(entityNum=3, dim=1), "9=" )
  call display( dom%getTotalNodes(entityNum=4, dim=1), "9=" )
  call dom%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( domain_ ) :: dom
  type( HDF5File_ ) :: meshfile
  call display( "Testing Initiate and DeallocateData" )
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call dom%initiate( meshfile )
  call dom%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine exportMesh
  TYPE( MSH_ ) :: mshFile
  CALL mshFile%initiate( file="./mesh.msh", NSD=2 )
  CALL mshFile%ExportMesh( file="./mesh.h5" )
  CALL mshFile%DeallocateData()
end


end module test_m

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_m
implicit none
! call exportMesh
call test0
end program main