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

SUBMODULE(LinSolver_Class) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Display
  INTEGER( I4B ) :: I
  I = INPUT( option=unitno, default=stdout )
  CALL Display( msg, "#", unitNo=I )
  CALL Display( "# engine : NATIVE_SERIAL", unitNo=I )
  CALL Display(  obj%isInitiated, "# isInitiated : ", unitNo=I )
  CALL Display(  obj%solverName, "# solverName : ", unitNo=I )
  CALL Display(  obj%preconditionOption, "# preconditionOption : ", unitNo=I )
  CALL Display(  obj%convergenceIn, "# convergenceIn : ", unitNo=I )
  CALL Display(  obj%convergenceType, "# convergenceType : ", unitNo=I )
  CALL Display(  obj%maxIter, "# maxIter : ", unitNo=I )
  CALL Display(  obj%relativeToRHS, "# relativeToRHS : ", unitNo=I )
  CALL Display(  obj%KrylovSubspaceSize, "# KrylovSubspaceSize : ", unitNo=I )
  CALL Display(  obj%atol, "# atol : ", unitNo=I )
  CALL Display(  obj%rtol, "# rtol : ", unitNo=I )
  CALL Display(  obj%ierr, "# ierr : ", unitNo=I )
  CALL Display(  obj%iter, "# iter : ", unitNo=I )
  IF( ASSOCIATED( obj%Amat ) ) THEN
    CALL Display( "Amat is associated", unitNo=I)
  ELSE
    CALL Display( "Amat is NOT associated", unitNo=I)
  END IF
  IF( ALLOCATED( obj%RES ) ) &
    & CALL Display( "# obj%RES is allocated", unitNo=I )
  IF( ALLOCATED( obj%W ) ) &
    & CALL Display(  "# obj%W is allocated : ", unitNo=I )
  CALL Display(  obj%IPAR, "# IPAR : ", unitNo=I )
  CALL Display(  obj%FPAR, "# FPAR : ", unitNo=I )
END PROCEDURE ls_Display

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Import
  CHARACTER( LEN = * ), PARAMETER :: myName="ls_Import"
  TYPE( String ) :: dsetname, strval
  INTEGER( I4B ) :: solverName, preconditionOption, convergenceIn, &
    & convergenceType, maxIter, KrylovSubspaceSize
  LOGICAL( LGT ) :: relativeToRHS
  REAL( DFP ) :: absoluteTolerance, relativeTolerance
  TYPE( ParameterList_ ) :: param

  !> check
  IF( obj%isInitiated ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The object is already initiated, deallocate first!')
  END IF
  obj%isInitiated = .TRUE.

  !> print info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "IMPORTING LINEAR SOLVER")
  !> check
  IF( .NOT. hdf5%isOpen() ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file is not opened')
  END IF
  IF( .NOT. hdf5%isRead() ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file does not have read permission')
  END IF
  ! READ engine
  dsetname=trim(group)//"/engine"
  IF( .NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset engine should be present')
  END IF
  CALL hdf5%read(dsetname=dsetname%chars(),vals=strval)
  IF( TRIM(strval) .NE. "NATIVE_SERIAL" ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset engine should be equal to NATIVE_SERIAL')
  END IF

  ! READ solverName
  dsetname=trim(group)//"/solverName"
  IF( .NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset solverName should be present')
  END IF
  CALL hdf5%read(dsetname=dsetname%chars(),vals=strval)
  solverName = getLinSolverCodeFromName( trim(strval%chars()) )
  ! READ preconditionOption
  dsetname=trim(group)//"/preconditionOption"
  IF( .NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset preconditionOption should be present')
  END IF
  CALL hdf5%read(dsetname=dsetname%chars(),vals=strval)
  SELECT CASE( trim(strval%chars()) )
  CASE( "NONE" )
    preconditionOption = NO_PRECONDITION
  CASE( "LEFT" )
    preconditionOption = LEFT_PRECONDITION
  CASE( "RIGHT" )
    preconditionOption = RIGHT_PRECONDITION
  CASE( "LEFT_RIGHT" )
    preconditionOption = LEFT_RIGHT_PRECONDITION
  END SELECT
  ! READ convergenceIn
  dsetname=trim(group)//"/convergenceIn"
  IF( .NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset convergenceIn should be present')
  END IF
  CALL hdf5%read(dsetname=dsetname%chars(),vals=strval)
  SELECT CASE( trim(strval%chars()) )
  CASE( "RESIDUAL" )
    convergenceIn = convergenceInRes
  CASE( "SOLUTION" )
    convergenceIn = convergenceInSol
  END SELECT
  ! READ convergenceType
  dsetname=trim(group)//"/convergenceType"
  IF( .NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset convergenceType should be present')
  END IF
  CALL hdf5%read(dsetname=dsetname%chars(),vals=strval)
  SELECT CASE( trim(strval%chars()) )
  CASE( "ABSOLUTE" )
    convergenceType = absoluteConvergence
  CASE( "RELATIVE" )
    convergenceType = relativeConvergence
  END SELECT
  ! READ relativeToRHS
  IF( convergenceType .EQ. relativeConvergence ) THEN
    dsetname=trim(group)//"/relativeToRHS"
    IF( hdf5%pathExists(dsetname%chars()) ) THEN
      CALL hdf5%read(dsetname=dsetname%chars(),vals=relativeToRHS)
    ELSE
      relativeToRHS = .FALSE.
    END IF
  ELSE
    relativeToRHS = .FALSE.
  END IF
  ! READ maxIter
  dsetname=trim(group)//"/maxIter"
  IF( .NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset maxIter should be present')
  END IF
  CALL hdf5%read(dsetname=dsetname%chars(),vals=maxIter)
  ! READ KrylovSubspaceSize
  dsetname=trim(group)//"/KrylovSubspaceSize"
  IF( hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%read(dsetname=dsetname%chars(),vals=KrylovSubspaceSize)
  ELSE
    KrylovSubspaceSize = 20
  END IF
  ! READ relativeTolerance
  dsetname=trim(group)//"/relativeTolerance"
  IF( .NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset relativeTolerance should be present')
  END IF
  CALL hdf5%read(dsetname=dsetname%chars(),vals=relativeTolerance)
  ! READ absoluteTolerance
  dsetname=trim(group)//"/absoluteTolerance"
  IF( .NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset absoluteTolerance should be present')
  END IF
  CALL hdf5%read(dsetname=dsetname%chars(),vals=absoluteTolerance)

  CALL FPL_INIT; CALL param%initiate()
  CALL setLinSolverParam( &
    & param=param, &
    & solverName=solverName,&
    & preconditionOption=preconditionOption, &
    & convergenceIn=convergenceIn, &
    & convergenceType=convergenceType, &
    & maxIter=maxIter, &
    & relativeToRHS=relativeToRHS, &
    & KrylovSubspaceSize=KrylovSubspaceSize, &
    & rtol=relativeTolerance, &
    & atol=absoluteTolerance )
  CALL obj%Initiate(param)
  CALL param%Deallocate(); CALL FPL_FINALIZE
END PROCEDURE ls_Import

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Export
  CHARACTER( LEN = * ), PARAMETER :: myName="ls_Export"
  TYPE( String ) :: dsetname, strval

  !> check
  IF( .NOT. obj%isInitiated ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'The object is not initiated, initiate it first!')
  END IF

  !> print info
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "EXPORTING LINEAR SOLVER")
  !> check
  IF( .NOT. hdf5%isOpen() ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file is not opened')
  END IF
  IF( .NOT. hdf5%isWrite() ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file does not have write permission')
  END IF
  ! WRITE engine
  dsetname=trim(group)//"/engine"
  CALL hdf5%write(dsetname=dsetname%chars(),vals=obj%engine)
  ! WRITE solverName
  dsetname=trim(group)//"/solverName"
  strval = getLinSolverNameFromCode( obj%solverName )
  CALL hdf5%write(dsetname=dsetname%chars(),vals=strval)
  ! WRITE preconditionOption
  dsetname=trim(group)//"/preconditionOption"
  SELECT CASE( obj%preconditionOption )
  CASE( NO_PRECONDITION )
    strval = "NONE"
  CASE(  LEFT_PRECONDITION )
    strval = "LEFT"
  CASE( RIGHT_PRECONDITION )
    strval = "RIGHT"
  CASE( LEFT_RIGHT_PRECONDITION )
    strval = "LEFT_RIGHT"
  END SELECT
  CALL hdf5%write(dsetname=dsetname%chars(),vals=strval)
  ! WRITE convergenceIn
  dsetname=trim(group)//"/convergenceIn"
  SELECT CASE( obj%convergenceIn )
  CASE( convergenceInRes )
    strval="RESIDUAL"
  CASE( convergenceInSol )
    strval="SOLUTION"
  END SELECT
  CALL hdf5%write(dsetname=dsetname%chars(),vals=strval)
  ! WRITE convergenceType
  dsetname=trim(group)//"/convergenceType"
  SELECT CASE( obj%convergenceType )
  CASE( absoluteConvergence )
    strval = "ABSOLUTE"
  CASE( relativeConvergence )
    strval = "RELATIVE"
  END SELECT
  CALL hdf5%write(dsetname=dsetname%chars(),vals=strval)
  ! WRITE relativeToRHS
  dsetname=trim(group)//"/relativeToRHS"
  CALL hdf5%write(dsetname=dsetname%chars(),vals=obj%relativeToRHS)
  ! WRITE maxIter
  dsetname=trim(group)//"/maxIter"
  CALL hdf5%write(dsetname=dsetname%chars(),vals=obj%maxIter)
  ! WRITE KrylovSubspaceSize
  dsetname=trim(group)//"/KrylovSubspaceSize"
  CALL hdf5%write(dsetname=dsetname%chars(),vals=obj%KrylovSubspaceSize)
  ! WRITE relativeTolerance
  dsetname=trim(group)//"/relativeTolerance"
  CALL hdf5%write(dsetname=dsetname%chars(),vals=obj%rtol)
  ! WRITE absoluteTolerance
  dsetname=trim(group)//"/absoluteTolerance"
  CALL hdf5%write(dsetname=dsetname%chars(),vals=obj%atol)
END PROCEDURE ls_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods