
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
  type( MatrixField_ ) :: Amat, Pmat
  type( LinSolver_ ) :: obj
  type( ParameterList_ ) :: param
  integer( i4b ) :: ierr, tnodes

  call display( "Testing Initiate and DeallocateData" )
  CALL FPL_INIT()
  CALL param%initiate()
  CALL setLinSolverParam( param=param, solverName=LIS_CG,&
    & preconditionOption=LEFT_PRECONDITION, convergenceIn=convergenceInRes, &
    & convergenceType=relativeConvergence, maxIter=100,relativeToRHS=.TRUE., &
    & KrylovSubspaceSize=20,rtol=1.0D-10,atol=1.0D-10 )
  CALL obj%initiate(param)
  CALL obj%Display("LinSolver : ")
  CALL obj%DeallocateData()
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