
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

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! subroutine test0
!   type( domain_ ) :: dom
!   type( MatrixField_ ) :: obj
!   type( HDF5File_ ) :: meshfile
!   type( ParameterList_ ) :: param
!   integer( i4b ) :: ierr
!   real( DFP ), ALLOCATABLE :: realVec( : )

!   call display( "Testing get methods for constant data" )
!   CALL FPL_INIT()
!   CALL param%initiate()
!   ierr = param%set(key="name", value="U" )
!   ierr = param%set(key="fieldType", value=FIELD_TYPE_CONSTANT)
!   call meshfile%initiate( filename="./mesh.h5", mode="READ" )
!   call meshfile%open()
!   call dom%initiate( meshfile )

!   call obj%initiate( param, dom )
!   call obj%set( globalNode = 10, value= 100.0_DFP )
!   realVec = [obj%get( globalNode = 10 )]
!   call display( realVec, "100.0=" )


!   call obj%set( value= 200.0_DFP )
!   realVec = obj%get()
!   call display( realVec, "realVec = " )


!   call obj%set(globalNode=[1,2,5], value=1.0_DFP )
!   realVec = obj%get([1,2,5])
!   call display( realVec, "realVec = " )

!   call obj%deallocateData()
!   call dom%deallocateData()
!   call meshfile%close()
!   call meshfile%deallocateData()
!   call param%deallocateData()
!   call FPL_FINALIZE()
! end subroutine

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! subroutine test5
!   type( domain_ ) :: dom
!   type( MatrixField_ ) :: obj
!   type( HDF5File_ ) :: meshfile
!   type( ParameterList_ ) :: param
!   integer( i4b ) :: ierr
!   real( DFP ), ALLOCATABLE :: realVec( : )

!   call display( "Testing get methods for normal data" )
!   CALL FPL_INIT()
!   CALL param%initiate()
!   ierr = param%set(key="name", value="U" )
!   ierr = param%set(key="fieldType", value=FIELD_TYPE_NORMAL)
!   call meshfile%initiate( filename="./mesh.h5", mode="READ" )
!   call meshfile%open()
!   call dom%initiate( meshfile )

!   call obj%initiate( param, dom )
!   call obj%set( globalNode = 10, value= 100.0_DFP )
!   realVec = [obj%get( globalNode = 10 )]
!   call display( realVec, "100.0=" )


!   call obj%set( value= 200.0_DFP )
!   realVec = obj%get()
!   call display( realVec, "realVec = " )


!   call obj%set(globalNode=[1,2,5], value=1.0_DFP )
!   realVec = obj%get([1,2,5])
!   call display( realVec, "realVec = " )

!   call obj%deallocateData()
!   call dom%deallocateData()
!   call meshfile%close()
!   call meshfile%deallocateData()
!   call param%deallocateData()
!   call FPL_FINALIZE()
! end subroutine

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! subroutine test4
!   type( domain_ ) :: dom
!   type( MatrixField_ ) :: obj
!   type( HDF5File_ ) :: meshfile
!   type( ParameterList_ ) :: param
!   integer( i4b ) :: ierr
!   real( DFP ), ALLOCATABLE :: realVec( : )

!   call eScalarField%setStopOnError( .FALSE. )
!   call display( "Testing set methods for constant data" )
!   CALL FPL_INIT()
!   CALL param%initiate()
!   ierr = param%set(key="name", value="U" )
!   ierr = param%set(key="fieldType", value=FIELD_TYPE_CONSTANT)
!   call meshfile%initiate( filename="./mesh.h5", mode="READ" )
!   call meshfile%open()
!   call dom%initiate( meshfile )

!   call obj%initiate( param, dom )
!   call obj%set( globalNode = 10, value= 100.0_DFP )
!   call obj%display( "scalar field = ")
!   call obj%set( value= 200.0_DFP )
!   call obj%display( "scalar field = ")

!   call reallocate( realVec, obj%domain%getTotalNodes() )
!   call RANDOM_NUMBER( realVec )
!   call obj%set(realVec)
!   call obj%display( "scalar field = ")

!   call obj%set(globalNode=[1,2,5], value=0.0_DFP )
!   call obj%display( "scalar field = ")

!   call obj%deallocateData()
!   call dom%deallocateData()
!   call meshfile%close()
!   call meshfile%deallocateData()
!   call param%deallocateData()
!   call FPL_FINALIZE()
! end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
  type( domain_ ) :: dom
  type( MatrixField_ ) :: obj
  type( HDF5File_ ) :: meshfile, hdf5
  type( ParameterList_ ) :: param
  integer( i4b ) :: ierr, tnodes
  class( mesh_ ), pointer :: meshObj

  !-------------------------------------------------------------------!
  call display( "TESTING SET METHODS" )
  ! Initiate FPL environment
  call FPL_INIT()
  ! open mesh file
  call meshfile%initiate( filename="./mesh.h5", mode="READ" ); call meshfile%open()
  ! open output file
  call hdf5%initiate(filename="./test3.h5", mode="NEW" )
  call hdf5%open()
  !> Initiate domain
  call dom%initiate( meshfile )
  !> close mesh file
  call meshfile%close(); call meshfile%deallocateData()
  ! initiate parameters and set parameters for matrix field
  call param%initiate()
  call setMatrixFieldParam( param, "K", "UNSYM", 3, 2, FIELD_TYPE_NORMAL )
  ! initiate matrix field
  call obj%initiate( param, dom )
  ! getPointer to mesh
  meshObj => dom%getMeshPointer( dim=2, tag=1 )
  ! export mesh
  call meshObj%export()

  call hdf5%close()
  call hdf5%deallocateData()
  call obj%deallocateData()
  call dom%deallocateData()
  call param%deallocateData()
  call FPL_FINALIZE()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
  type( domain_ ) :: dom
  type( MatrixField_ ) :: obj, obj2
  type( HDF5File_ ) :: meshfile, hdf5
  type( ParameterList_ ) :: param
  integer( i4b ) :: ierr, tnodes
  call display( "TESTING INITIATE BY COPYING" )
  call FPL_INIT()
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call dom%initiate( meshfile )
  call meshfile%close()
  call meshfile%deallocateData()
  tnodes = dom%getTotalNodes()
  call param%initiate()
  call setMatrixFieldParam( param, "K", "UNSYM", 3, 2, FIELD_TYPE_NORMAL )
  call obj%initiate( param, dom )
  call obj2%initiate( obj )
  call hdf5%initiate(filename="./matrixField.h5", mode="NEW" )
  call hdf5%open()
  call obj2%export(hdf5=hdf5,group='')
  call hdf5%close()
  call hdf5%deallocateData()
  call obj%deallocateData()
  call obj2%deallocateData()
  call dom%deallocateData()
  call param%deallocateData()
  call FPL_FINALIZE()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( domain_ ) :: dom
  type( MatrixField_ ) :: obj
  type( HDF5File_ ) :: meshfile, hdf5
  type( ParameterList_ ) :: param
  integer( i4b ) :: ierr, tnodes
  call display( "TESTING INITIATE AND DEALLOCATEDATA" )
  CALL FPL_INIT()
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call dom%initiate( meshfile )
  call meshfile%close()
  call meshfile%deallocateData()
  tnodes = dom%getTotalNodes()
  call param%initiate()
  call setMatrixFieldParam( param, "K", "UNSYM", 3, 2, FIELD_TYPE_NORMAL )
  call obj%initiate( param, dom )
  CALL hdf5%initiate(filename="./matrixField.h5", mode="NEW" )
  CALL hdf5%open()
  CALL obj%export(hdf5=hdf5,group='')
  CALL hdf5%close()
  CALL hdf5%deallocateData()
  call obj%deallocateData()
  call dom%deallocateData()
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