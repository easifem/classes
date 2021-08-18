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

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This program solves heat conduction equation.

PROGRAM main
USE EASIFEMBASE
USE EASIFEMCLASSES

TYPE( MatrixField_ ) :: Amat
TYPE( ScalarField_ ) :: rhs, sol, x
TYPE( VectorField_ ) :: coordfield
TYPE( LinSolver_ ) :: linsol
TYPE( Domain_ ) :: dom
CLASS( Mesh_ ), pointer :: meshPtr
TYPE( HDF5File_ ) :: meshfile, hdf5
TYPE( ElemShapeData_ ) :: elemsd
TYPE( QuadraturePoint_ ) :: quad
CLASS( ReferenceElement_ ), pointer :: refelem
TYPE( ParameterList_ ) :: param
TYPE( RealMatrix_ ) :: realmat
REAL( DFP ), POINTER :: nodecoordPtr( :, : )
INTEGER( I4B ) :: ierr, ii, tMesh, imesh
REAL( DFP ), ALLOCATABLE :: xij( :, : )
INTEGER( I4B ), ALLOCATABLE :: nptrs( : )

TYPE( CSRMatrix_ ) :: delme

!----------------------------------------------------------------------------
! MAIN PROGRAM
!----------------------------------------------------------------------------

CALL FPL_INIT; CALL param%initiate()
!> open meshfile
CALL meshfile%Initiate( filename="./mesh.h5", mode="READ" )
CALL meshfile%Open()
CALL hdf5%Initiate( filename="./diffusionData.h5", mode="NEW" )
CALL hdf5%Open()
!> initiate domain
CALL dom%Initiate(hdf5=meshfile, group="" )
!> get node coord
nodecoordPtr => dom%GetNodeCoordPointer()
CALL SetVectorFieldParam( param=param, name="coord", spaceCompo=3 )
CALL coordfield%Initiate( param=param, dom=dom )
CALL coordfield%Set( value = nodecoordPtr ); NULLIFY( nodecoordPtr )
!> sol
CALL SetScalarFieldParam( param=param, name="sol" )
CALL sol%initiate( param=param, dom=dom )
!> rhs
CALL SetScalarFieldParam( param=param, name="rhs" )
CALL rhs%initiate( param=param, dom=dom )
!> set parameters for matrix field
CALL setMatrixFieldParam( param=param, name="K", matrixProp="UNSYM", &
  & spaceCompo=1, timeCompo=1, fieldType=FIELD_TYPE_NORMAL )
CALL Amat%initiate( param, dom )
! > param for linear solver
CALL setLinSolverParam( param=param, solverName=LIS_CG,&
  & preconditionOption=LEFT_PRECONDITION, convergenceIn=convergenceInRes, &
  & convergenceType=relativeConvergence, maxIter=100,relativeToRHS=.FALSE., &
  & KrylovSubspaceSize=20,rtol=1.0D-5,atol=1.0D-5 )
!> initiate linear solver
CALL linsol%initiate(param)
!> start the loop around each mesh
tmesh = dom%getTotalMesh( dim=2 )
DO imesh = 1, tmesh
  meshPtr => dom%GetMeshPointer( dim=2, tag=imesh )
  refelem => meshPtr%GetRefElemPointer()
  quad = GaussLegendreQuadrature( refelem=refelem, order=refelem%order )
  CALL Initiate( obj=elemsd, quad=quad, refelem=refelem, &
    & ContinuityType=TypeH1, InterpolType=TypeLagrangeInterpolation )
  ! > start element loop
  DO ii = meshPtr%minElemNum, meshPtr%maxElemNum
    IF( .NOT. meshPtr%isElementPresent(ii) ) CYCLE
    nptrs = meshPtr%getConnectivity(ii)
    CALL coordfield%Get( value=xij, globalNode=nptrs )
    CALL SetValue(obj=elemsd,val=xij(1:2, :),N=elemsd%N,dNdXi=elemsd%dNdXi)
    realmat%val = DiffusionMatrix( Test=elemsd, Trial=elemsd )
    CALL Amat%set( globalNode = nptrs, val=realmat%val, storageFMT=FMT_DOF, &
      & scale=1.0_DFP )
  END DO
END DO
!> get columns of amatrix, multiply with the boundary condition, and add to rhs boundary condition is applied on bottom ( 1,2 ) and top (4,5)
tmesh = dom%getTotalMesh( dim=1 )
DO imesh = 1, tmesh
  IF( .NOT. ANY( imesh .EQ. [1,2] ) ) CYCLE
  meshPtr => dom%GetMeshPointer( dim=1, tag=imesh )
  nptrs = meshPtr%getNptrs( )
  DO ii = 1, SIZE( nptrs )
    CALL Amat%getColumn( globalNode=nptrs( ii ), idof=1, nodeFieldVal=rhs, &
      & scale=-1.0_DFP, addContribution= .TRUE. )
    CALL Amat%setRow( globalNode=nptrs( ii ), idof=1, scalarVal=0.0_DFP )
    CALL Amat%setColumn( globalNode=nptrs( ii ), idof=1, scalarVal=0.0_DFP )
    CALL Amat%set( rowNodeNum= nptrs(ii), colNodeNum=nptrs( ii ), rowDOF=1, &
      & colDOF=1, val=1.0_DFP )
  END DO
  CALL EqualLine( )
END DO
DO imesh = 1, tmesh
  IF( .NOT. ANY( imesh .EQ. [1,2] ) ) CYCLE
  meshPtr => dom%GetMeshPointer( dim=1, tag=imesh )
  nptrs = meshPtr%getNptrs( )
  DO ii = 1, SIZE( nptrs )
    CALL rhs%set( globalNode=nptrs( ii ), val = 1.0_DFP )
  END DO
  CALL EqualLine( )
END DO
!> boundary condition is applied on bottom ( 1,2 ) and top (4,5)
tmesh = dom%getTotalMesh( dim=1 )
DO imesh = 1, tmesh
  IF( .NOT. ANY( imesh .EQ. [4,5] ) ) CYCLE
  meshPtr => dom%GetMeshPointer( dim=1, tag=imesh )
  nptrs = meshPtr%getNptrs( )
  DO ii = 1, SIZE( nptrs )
    CALL Amat%getColumn( globalNode=nptrs( ii ), idof=1, nodeFieldVal=rhs, &
    & scale=-10.0_DFP, addContribution= .TRUE. )
    CALL Amat%setRow( globalNode=nptrs( ii ), idof=1, scalarVal=0.0_DFP )
    CALL Amat%setColumn( globalNode=nptrs( ii ), idof=1, scalarVal=0.0_DFP )
    CALL Amat%set( rowNodeNum= nptrs(ii), colNodeNum=nptrs( ii ), rowDOF=1, &
      & colDOF=1, val=1.0_DFP )
  END DO
END DO
DO imesh = 1, tmesh
  IF( .NOT. ANY( imesh .EQ. [4,5] ) ) CYCLE
  meshPtr => dom%GetMeshPointer( dim=1, tag=imesh )
  nptrs = meshPtr%getNptrs( )
  DO ii = 1, SIZE( nptrs )
    CALL rhs%set( globalNode=nptrs( ii ), val = 10.0_DFP )
  END DO
END DO
ierr = param%set("preconditionName", PRECOND_ILUD )
ierr = param%set("lfil", 20 )
ierr = param%set("mbloc", Amat%Size(1) )
ierr = param%set("droptol", 0.004_DFP  )
ierr = param%set("alpha", 1.0_DFP )
ierr = param%set("permtol", 0.1_DFP )

CALL getILUD( &
    & obj=Amat%mat, alpha=1.0_DFP, droptol=0.001_DFP, &
    & Pmat=delme )
CALL Spy( delme, "Pmat" )

! CALL Amat%setPrecondition( param )
! CALL Amat%export( hdf5,"/Amat" )
! CALL linsol%set( Amat )
! CALL linsol%solve( sol=sol, rhs=rhs )
! CALL Amat%SPY( "Amat" )
CALL linsol%DeallocateData()
CALL dom%DeallocateData(); CALL meshPtr%DeallocateData()
CALL meshfile%Close(); CALL meshfile%DeallocateData()
CALL hdf5%Close(); CALL hdf5%DeallocateData()
CALL param%DeallocateData(); CALL FPL_FINALIZE
END PROGRAM main