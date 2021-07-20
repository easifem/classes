
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
  type( STScalarField_ ) :: obj
  type( ScalarField_ ) :: scalarObj
  type( HDF5File_ ) :: meshfile
  type( ParameterList_ ) :: param
  integer( i4b ) :: ierr
  real( DFP ), ALLOCATABLE :: real1( : ), real2( :, : )
  real( DFP ) :: real0

  call display( "Testing get methods for normal data" )
  CALL FPL_INIT()
  CALL param%initiate()
  ierr = param%set(key="name", value="U" )
  ierr = param%set(key="fieldType", value=FIELD_TYPE_NORMAL)
  ierr = param%set(key="timeCompo", value=3)
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call dom%initiate( meshfile )

  call obj%initiate( param, dom )
  call obj%set( globalNode = 10, value= 10.0_DFP*[1,2,3] )
  call obj%get( globalNode = 10, value = real1 )
  call display( real1, "test-1: get() = " )

  call obj%set( value= -10.0_DFP, timeCompo=1 )
  call obj%set( value= -20.0_DFP, timeCompo=2 )
  call obj%set( value= -30.0_DFP, timeCompo=3 )
  call obj%get( value = real1, timeCompo = 1 )
  call display( real1, "test-2: get() = " )

  call reallocate( real2, 3, 4 )
  real2( :, 1 ) = [1,2,3]
  real2( :, 2 ) = [4,5,6]
  real2( :, 3 ) = [7,8,9]
  real2( :, 4 ) = [10,11,12]
  call obj%set( value=real2, globalNode=[3,5,7,9])
  call obj%get( value=real2, globalNode=[3,5,7,9])
  call display( real2, "test-3: get() = ")

  call obj%get( value=real1, globalNode=[3,5,7,9], timeCompo=1)
  call display( real1, "test-4: get() = ")

  call obj%get( value=real0, globalNode=5, timeCompo=1)
  call display( real0, "test-5: get() = ")
  call obj%get( value=real0, globalNode=5, timeCompo=2)
  call display( real0, "test-5: get() = ")
  call obj%get( value=real0, globalNode=5, timeCompo=3)
  call display( real0, "test-5: get() = ")

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

subroutine test0
  type( domain_ ) :: dom
  type( STScalarField_ ) :: obj
  type( ScalarField_ ) :: scalarObj
  type( HDF5File_ ) :: meshfile
  type( ParameterList_ ) :: param
  integer( i4b ) :: ierr
  real( DFP ), ALLOCATABLE :: real1( : ), real2( :, : )
  call display( "Testing set methods for normal data" )
  CALL FPL_INIT()
  CALL param%initiate()
  ierr = param%set(key="name", value="U" )
  ierr = param%set(key="fieldType", value=FIELD_TYPE_NORMAL)
  ierr = param%set(key="timeCompo", value=3)
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call dom%initiate( meshfile )
  call obj%initiate( param, dom )


  call obj%set( globalNode = 10, value= 100.0_DFP*[1,1,1] )
  call obj%display( "test-1: space-time scalar field = ")

  call obj%set( value= 10.0_DFP*[1,1,1] )
  call obj%display( "test-2: space-time scalar field = ")

  call obj%set( value= -10.0_DFP, timeCompo=1 )
  call obj%set( value= -20.0_DFP, timeCompo=2 )
  call obj%set( value= -30.0_DFP, timeCompo=3 )
  call obj%display( "test-3: space-time scalar field = ")

  call reallocate( real2, 3, dom%getTotalNodes() )
  real2 = 1.0_DFP
  call obj%set( value=real2 )
  call obj%display( "test-4: space-time scalar field = " )

  call reallocate( real1, dom%getTotalNodes() )
  real1 = 3.0_DFP
  call obj%set( value=real1, timeCompo=3 )
  call obj%display( "test-5: space-time scalar field = " )

  call scalarObj%initiate( param, dom )
  call scalarObj%set( value = 2.0_DFP )
  call obj%set( value=scalarObj, timeCompo=2 )
  call obj%display( "test-6: space-time scalar field = ")
  ierr = param%set( key="fieldType", value=FIELD_TYPE_CONSTANT)
  call scalarObj%deallocateData()
  call scalarObj%initiate( param, dom )
  call scalarObj%set( value=10.0_DFP )
  call obj%set( value=scalarObj, timeCompo=1 )
  call obj%display( "test-7: space-time scalar field = ")


  ierr = param%set( key="fieldType", value=FIELD_TYPE_NORMAL)

  call reallocate( real2, 3, 4)
  real2( :, 1 ) = -1.0; real2( :, 2 ) = -2.0; real2( :, 3 ) = -3.0
  real2( :, 4 ) = -4.0
  call obj%set( value=real2, globalNode=[1,3,5,7] )
  call obj%display( "test-8: space-time scalar field = ")

  call reallocate( real1, 4)
  real1 = [1,10,100,1000]
  call obj%set( value=real1, globalNode=[1,3,5,7], timeCompo=1 )
  call obj%display( "test-9: space-time scalar field = " )

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
  type( STScalarField_ ) :: obj
  type( HDF5File_ ) :: meshfile
  type( ParameterList_ ) :: param
  integer( i4b ) :: ierr

  call display( "Testing Initiate and DeallocateData for normal data" )
  CALL FPL_INIT()
  CALL param%initiate()
  ierr = param%set(key="name", value="U" )
  ierr = param%set(key="fieldType", value=FIELD_TYPE_NORMAL)
  ierr = param%set(key="timeCompo", value=3)
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call dom%initiate( meshfile )
  call obj%initiate( param, dom )
  call obj%display( "space-time scalar field = ")
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
  type( STScalarField_ ) :: obj
  type( HDF5File_ ) :: meshfile
  type( ParameterList_ ) :: param
  integer( i4b ) :: ierr

  call display( "Testing Initiate and DeallocateData for constant data" )
  CALL FPL_INIT()
  CALL param%initiate()
  ierr = param%set(key="name", value="U" )
  ierr = param%set(key="fieldType", value=FIELD_TYPE_CONSTANT)
  ierr = param%set(key="timeCompo", value=3)
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call dom%initiate( meshfile )

  call obj%initiate( param, dom )
  call obj%display( "space-time scalar field = ")
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