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

subroutine test11
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr, ii
  type( HDF5File_ ) :: meshfile
  type( elemShapeData_ ) :: elemsd
  type( quadraturePoint_ ) :: quad
  class( referenceElement_ ), pointer :: refelem
  type( vectorField_ ) :: nodeCoord
  !
  call display( colorize('TEST:', color_fg='pink', style='underline_on') &
    & // colorize('Element shape data formation  :', color_fg='blue', &
    & style='underline_on') )
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )
  refelem => obj%getRefElemPointer()
  quad = GaussLegendreQuadrature( refelem=refelem, order=refelem%order )
  call initiate( obj = elemsd, quad = quad, refelem = refelem, &
    & continuityType= TypeH1, interpolType = TypeLagrangeInterpolation )
  do ii = obj%minElemNum,obj%maxElemNum
    if( .NOT. obj%isElementPresent(ii ) ) cycle
  end do
  call obj%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test10
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr, ii
  type( HDF5File_ ) :: meshfile
  call display( colorize('TEST:', color_fg='pink', style='underline_on') &
    & // colorize('testing  :', color_fg='blue', &
    & style='underline_on') )
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )
  do ii = obj%minElemNum,obj%maxElemNum
    if( .NOT. obj%isElementPresent(ii ) ) cycle
    if( obj%isBoundaryElement(ii) ) then
      call display( obj%getBoundaryElementData( ii ), &
      & "element = " // trim( string( ii ) ) &
      & // ' is connected to global elements = ' )
    end if
  end do
  call obj%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test9
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr, ii
  type( HDF5File_ ) :: meshfile
  call display( colorize('TEST:', color_fg='pink', style='underline_on') &
    & // colorize('getNptrs, getInternalNptrs, getBoundaryNptrs :', color_fg='blue', &
    & style='underline_on') )
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )
  call display( obj%getNptrs(), "getNptrs = ")
  call display( obj%getInternalNptrs(), "getInternalNptrs = ")
  call display( obj%getBoundaryNptrs(), "getBoundaryNptrs = ")
  call obj%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test8
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr, ii
  type( HDF5File_ ) :: meshfile
  call display( colorize('TEST:', color_fg='pink', style='underline_on') &
    & // colorize('ELEMENT TO ELEMENTS:', color_fg='blue', &
    & style='underline_on') )
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )
  do ii = obj%minElemNum, obj%maxElemNum
    if( .not. obj%isElementPresent( ii ) ) cycle
    call display( obj%getElementToElements( ii ), &
      & "element = " // trim( string( ii ) ) &
      & // ' is connected to global elements = ' )
  end do
  do ii = obj%minElemNum, obj%maxElemNum
    if( .not. obj%isElementPresent( ii ) ) cycle
    call display( obj%isBoundaryElement( ii ), &
      & "element = " // trim( string( ii ) ) &
      & // ' is a boundary element = ' )
  end do
  call obj%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test7
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr, ii
  type( HDF5File_ ) :: meshfile
  call display( colorize('TEST:', color_fg='pink', style='underline_on') &
    & // colorize('NODE TO NODES:', color_fg='blue', &
    & style='underline_on') )
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )
  do ii = obj%minNptrs, obj%maxNptrs
    if( .not. obj%isNodePresent( ii ) ) cycle
    call display( obj%getNodeToNodes( ii, .true. ), &
      & "node = " // trim( string( ii ) ) &
      & // ' is connected to global nodes = ' )
  end do
  call obj%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test6
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr, ii
  type( HDF5File_ ) :: meshfile
  call display( colorize('TEST:', color_fg='pink', style='underline_on') &
    & // colorize('NODE TO ELEMENTS:', color_fg='blue', &
    & style='underline_on') )
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )
  do ii = obj%minElemNum, obj%maxElemNum
    if( .not. obj%isElementPresent( ii ) ) cycle
  end do
  do ii = obj%minNptrs, obj%maxNptrs
    if( .not. obj%isNodePresent( ii ) ) cycle
    call display( obj%getNodeToElements( ii ), "node = " // trim( string( ii ) ) // ' is connected to global elements = '  )
  end do
  call obj%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr, ii, jj
  type( HDF5File_ ) :: meshfile
  call display( colorize('TEST:', color_fg='pink', style='underline_on') &
    & // colorize('LOOPING ON NODES:', color_fg='blue', &
    & style='underline_on') )
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )

  do ii = obj%minNptrs, obj%maxNptrs
    if( .not. obj%isNodePresent( ii ) ) cycle
    call display( obj%getNodeToElements( ii ), "node = " // trim( string( ii ) ) // ' is connected to global elements = '  )
  end do

  do ii = 1, obj%getTotalNodes()
    jj = obj%getGlobalNodeNumber( ii )
    call display( obj%getNodeToElements( jj ), "node = " // trim( string( jj ) ) // ' is connected to global elements = '  )
  end do

  call obj%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr, ii, jj
  type( HDF5File_ ) :: meshfile
  call display( colorize('TEST:', color_fg='pink', style='underline_on') &
    & // colorize('LOOPING ON ELEMENTS:', color_fg='blue', &
    & style='underline_on') )
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )

  do ii = obj%minElemNum, obj%maxElemNum
    if( .not. obj%isElementPresent( ii ) ) cycle
  end do

  do ii = 1, obj%getTotalElements()
    jj = obj%getGlobalElemNumber( ii )
    if( .not. obj%isElementPresent( jj ) ) cycle
  end do

  call obj%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr
  type( HDF5File_ ) :: meshfile
  call display( colorize('TEST:', color_fg='blue', style='underline_on') &
    & // colorize('INITIATE POINT ENTITIES:', color_fg='green', &
    & style='underline_on') )
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(hdf5=meshfile, group="/pointEntities_1" )
  call obj%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr
  type( HDF5File_ ) :: meshfile
  call display( colorize('TEST:', color_fg='blue', style='underline_on') //&
    & colorize('INITIATE CURVE ENTITIES:', color_fg='green', &
    & style='underline_on') )
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(hdf5=meshfile, group="/curveEntities_1" )
  call obj%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr
  type( HDF5File_ ) :: meshfile
  call display( colorize('TEST:', color_fg='blue', style='underline_on') &
    & // colorize('INITIATE SURFACE ENTITIES:', color_fg='green', &
    & style='underline_on') )
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(hdf5=meshfile, group="/surfaceEntities_1" )
  call obj%deallocateData()
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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