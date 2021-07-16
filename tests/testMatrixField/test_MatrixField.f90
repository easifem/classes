
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

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! subroutine test3
!   type( domain_ ) :: dom
!   type( MatrixField_ ) :: obj
!   type( HDF5File_ ) :: meshfile
!   type( ParameterList_ ) :: param
!   integer( i4b ) :: ierr
!   real( DFP ), ALLOCATABLE :: realVec( : )

!   call display( "Testing set methods for normal data" )
!   CALL FPL_INIT()
!   CALL param%initiate()
!   ierr = param%set(key="name", value="U" )
!   ierr = param%set(key="fieldType", value=FIELD_TYPE_NORMAL)
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

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! subroutine test2
!   type( domain_ ) :: dom
!   type( MatrixField_ ) :: obj
!   type( HDF5File_ ) :: meshfile
!   type( ParameterList_ ) :: param
!   integer( i4b ) :: ierr

!   call display( "Testing Initiate and DeallocateData for normal data" )
!   CALL FPL_INIT()
!   CALL param%initiate()
!   ierr = param%set(key="name", value="U" )
!   ierr = param%set(key="fieldType", value=FIELD_TYPE_NORMAL)
!   call meshfile%initiate( filename="./mesh.h5", mode="READ" )
!   call meshfile%open()
!   call dom%initiate( meshfile )

!   call obj%initiate( param, dom )
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

subroutine test0
  type( domain_ ) :: dom
  type( MatrixField_ ) :: obj
  type( HDF5File_ ) :: meshfile
  type( ParameterList_ ) :: param
  type( DOF_ ) :: dofobj
  integer( i4b ) :: ierr, tnodes

  call display( "Testing Initiate and DeallocateData" )
  CALL FPL_INIT()
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call dom%initiate( meshfile )
  tnodes = dom%getTotalNodes()
  CALL initiate( obj=dofobj, names=['K'], spaceCompo=[2], timeCompo=[2], storageFMT=DOF_FMT, tNodes=[tNodes] )
  CALL param%initiate()
  ierr = param%set(key="name", value="K" )
  ierr = param%set(key="fieldType", value=FIELD_TYPE_CONSTANT)
  ierr = param%set(key="matrixProp", value="UNSYM" )
  call set( param, "dof", dofobj )
  call obj%initiate( param, dom )
  call obj%display( "Matrix field = ", unitNo=8 )
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