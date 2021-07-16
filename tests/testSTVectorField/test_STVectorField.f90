
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
  type( STVectorField_ ) :: obj
  type( ScalarField_ ) :: scalarObj
  type( HDF5File_ ) :: meshfile
  type( ParameterList_ ) :: param
  integer( i4b ) :: ierr
  real( DFP ), ALLOCATABLE :: real1( : ), real2( :, : ), real3( :, :, : )
  real( DFP ) :: real0

  call display( "Testing get methods for normal data" )
  CALL FPL_INIT()
  CALL param%initiate()
  ierr = param%set(key="name", value="U" )
  ierr = param%set(key="fieldType", value=FIELD_TYPE_NORMAL)
  ierr = param%set(key="spaceCompo", value=3)
  ierr = param%set(key="timeCompo", value=2)
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call dom%initiate( meshfile )

  call obj%initiate( param, dom )
  call obj%set( value= 11.0_DFP, spaceCompo=1, timeCompo=1 )
  call obj%set( value= 21.0_DFP, spaceCompo=2, timeCompo=1 )
  call obj%set( value= 31.0_DFP, spaceCompo=3, timeCompo=1 )
  call obj%set( value= 12.0_DFP, spaceCompo=1, timeCompo=2 )
  call obj%set( value= 22.0_DFP, spaceCompo=2, timeCompo=2 )
  call obj%set( value= 32.0_DFP, spaceCompo=3, timeCompo=2 )
  call obj%get( value = real3 )
  call display( real3, "value = ")

  call equalline()
  call obj%get( value = real3, globalNode=[1,3,5] )
  call display( real3, "value = ")

  call equalline()
  call obj%get( value = real1, globalNode=[1,3,5], spaceCompo=1, timeCompo=2 )
  call display( real1, "value = ")

  call equalline()
  call obj%get( value = real0, globalNode=5, spaceCompo=1, timeCompo=2 )
  call display( real0, "value = ")

  call obj%deallocateData()
  call dom%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
  call param%deallocateData()
  call FPL_FINALIZE()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
  type( domain_ ) :: dom
  type( STVectorField_ ) :: obj
  type( ScalarField_ ) :: scalarObj
  type( HDF5File_ ) :: meshfile
  type( ParameterList_ ) :: param
  integer( i4b ) :: ierr
  real( DFP ), ALLOCATABLE :: real1( : ), real2( :, : ), real3( :, :, : )
  call display( "Testing set methods for normal data" )
  CALL FPL_INIT()
  CALL param%initiate()
  ierr = param%set(key="name", value="U" )
  ierr = param%set(key="fieldType", value=FIELD_TYPE_NORMAL)
  ierr = param%set(key="spaceCompo", value=3)
  ierr = param%set(key="timeCompo", value=2)
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call dom%initiate( meshfile )
  call obj%initiate( param, dom )

  call reallocate( real2, 3, 2 )
  real2( :, 1 ) = 1.0
  real2( :, 2 ) = 2.0
  call obj%set( globalNode = 10, value= real2 )
  call obj%display( "test-1: space-time vector field = ")

  call obj%set( value= real2 )
  call obj%display( "test-2: space-time vector field = ")

  call obj%set( value= -10.0_DFP, spaceCompo=2, timeCompo=1 )
  call obj%set( value= -20.0_DFP, spaceCompo=2, timeCompo=2 )
  call obj%display( "test-3: space-time vector field = ")

  call reallocate( real3, [3, 2, dom%getTotalNodes()] )
  real3 = 1.0_DFP
  call obj%set( value=real3 )
  call obj%display( "test-4: space-time vector field = " )

  call reallocate( real1, dom%getTotalNodes() )
  real1 = 3.0_DFP
  call obj%set( value=real1, spaceCompo=3, timeCompo=2 )
  call obj%display( "test-5: space-time vector field = " )

  call scalarObj%initiate( param, dom )
  call scalarObj%set( value = 0.0_DFP )
  call obj%set( value=scalarObj, spaceCompo=3, timeCompo=2 )
  call obj%display( "test-6: space-time vector field = ")

  ierr = param%set( key="fieldType", value=FIELD_TYPE_CONSTANT)
  call scalarObj%deallocateData()
  call scalarObj%initiate( param, dom )
  call scalarObj%set( value=3.0_DFP )
  call obj%set( value=scalarObj, spaceCompo=3, timeCompo=2 )
  call obj%display( "test-7: space-time vector field = ")
  ierr = param%set( key="fieldType", value=FIELD_TYPE_NORMAL)

  call reallocate( real3, 3, 2, 2 )
  real3( :, :, 1 ) = -100.0
  real3( :, :, 2 ) = -200.0
  call obj%set( value=real3, globalNode=[1,102] )
  call obj%display( "test-8: space-time vector field = " )

  call reallocate( real1, 4)
  real1 = [1,10,100,1000]
  call obj%set( value=real1, globalNode=[1,3,5,7], timeCompo=1,spaceCompo=1 )
  call obj%display( "test-9: space-time vector field = " )

  call obj%deallocateData()
  call dom%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
  call param%deallocateData()
  call FPL_FINALIZE()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( domain_ ) :: dom
  type( STVectorField_ ) :: obj
  type( HDF5File_ ) :: meshfile
  type( ParameterList_ ) :: param
  integer( i4b ) :: ierr
  call display( "Testing Initiate and DeallocateData for normal data" )
  call FPL_INIT()
  call param%initiate()
  ierr = param%set(key="name", value="U" )
  ierr = param%set(key="fieldType", value=FIELD_TYPE_NORMAL)
  ierr = param%set(key="spaceCompo", value=3)
  ierr = param%set(key="timeCompo", value=3)
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call dom%initiate( meshfile )
  call obj%initiate( param, dom )
  call obj%display( "space-time vector field = ")
  call obj%deallocateData()
  call dom%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
  call param%deallocateData()
  call FPL_FINALIZE()
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