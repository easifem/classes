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

SUBMODULE(LinSolver_Class) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                 getLinSolverCodeFromName
!----------------------------------------------------------------------------

MODULE PROCEDURE getLinSolverCodeFromName
  SELECT CASE( TRIM( name ) )
  CASE( "CG" ) !1
    ans = LIS_CG
  CASE( "BICG" ) !2
    ans = LIS_BICG
  CASE( "CGS" ) !3
    ans = LIS_CGS
  CASE( "BICGSTAB" ) !4
    ans = LIS_BICGSTAB
  CASE( "BICGSTABL" ) !5
    ans = LIS_BICGSTABL
  CASE( "GPBICG" ) !6
    ans = LIS_GPBICG
  CASE( "TFQMR" ) !7
    ans = LIS_TFQMR
  CASE( "OMN", "FOM", "ORTHOMIN" ) !8
    ans = LIS_OMN
  CASE( "GMRES", "GMR" ) !9
    ans = LIS_GMRES
  CASE( "JACOBI" ) !10
    ans = LIS_JACOBI
  CASE( "GS" ) !11
    ans = LIS_GS
  CASE( "SOR" ) !12
    ans = LIS_SOR
  CASE( "BICGSAFE" ) !13
    ans = LIS_BICGSAFE
  CASE( "CR" ) !14
    ans = LIS_CR
  CASE( "BICR" ) !15
    ans = LIS_BICR
  CASE( "CRS" ) !16
    ans = LIS_CRS
  CASE( "BICRSTAB" ) !17
    ans = LIS_BICRSTAB
  CASE( "GPBICR" ) !18
    ans = LIS_GPBICR
  CASE( "BICRSAFE" ) !19
    ans = LIS_BICRSAFE
  CASE( "FGMRES" ) !20
    ans = LIS_FGMRES
  CASE( "IDRS" ) !21
    ans = LIS_IDRS
  CASE( "IDR1" ) !22
    ans = LIS_IDR1
  CASE( "MINRES" ) !23
    ans = LIS_MINRES
  CASE( "COCG" ) !24
    ans = LIS_COCG
  CASE( "COCR" ) !25
    ans = LIS_COCR
  CASE( "CGNR", "CGN" ) !26
    ans = LIS_CGNR
  CASE( "DBICG" ) !27
    ans = LIS_DBICG
  CASE( "DQGMRES" ) !28
    ans = LIS_DQGMRES
  END SELECT
END PROCEDURE getLinSolverCodeFromName

!----------------------------------------------------------------------------
!                                                 getLinSolverNameFromCode
!----------------------------------------------------------------------------

MODULE PROCEDURE getLinSolverNameFromCode
  SELECT CASE( name )
    CASE( LIS_CG )
    ans = "CG" !1
    CASE( LIS_BICG )
    ans = "BICG" !2
    CASE( LIS_CGS )
    ans = "CGS" !3
    CASE( LIS_BICGSTAB )
    ans = "BICGSTAB" !4
    CASE( LIS_BICGSTABL )
    ans = "BICGSTABL" !5
    CASE( LIS_GPBICG )
    ans = "GPBICG" !6
    CASE( LIS_TFQMR )
    ans = "TFQMR" !7
    CASE( LIS_OMN )
    ans = "ORTHOMIN" !8
    CASE( LIS_GMRES )
    ans = "GMRES" !9
    CASE( LIS_JACOBI )
    ans = "JACOBI" !10
    CASE( LIS_GS )
    ans = "GS" !11
    CASE( LIS_SOR )
    ans = "SOR" !12
    CASE( LIS_BICGSAFE )
    ans = "BICGSAFE" !13
    CASE( LIS_CR )
    ans = "CR" !14
    CASE( LIS_BICR )
    ans = "BICR" !15
    CASE( LIS_CRS )
    ans = "CRS" !16
    CASE( LIS_BICRSTAB )
    ans = "BICRSTAB" !17
    CASE( LIS_GPBICR )
    ans = "GPBICR" !18
    CASE( LIS_BICRSAFE )
    ans = "BICRSAFE" !19
    CASE( LIS_FGMRES )
    ans = "FGMRES" !20
    CASE( LIS_IDRS )
    ans = "IDRS" !21
    CASE( LIS_IDR1 )
    ans = "IDR1" !22
    CASE( LIS_MINRES )
    ans = "MINRES" !23
    CASE( LIS_COCG )
    ans = "COCG" !24
    CASE( LIS_COCR )
    ans = "COCR" !25
    CASE( LIS_CGNR )
    ans = "CGNR" !26
    CASE( LIS_DBICG )
    ans = "DBICG" !27
    CASE( LIS_DQGMRES )
    ans = "DQGMRES" !28
  END SELECT
END PROCEDURE getLinSolverNameFromCode

!----------------------------------------------------------------------------
!                                                               addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_addSurrogate
  CALL e%addSurrogate( UserObj )
END PROCEDURE ls_addSurrogate

!----------------------------------------------------------------------------
!                                                     setPreconditionOption
!----------------------------------------------------------------------------

SUBROUTINE setPreconditionOption( IPAR, PRECOND_TYPE )
  INTEGER( I4B ), INTENT( INOUT ) :: IPAR( : )
  INTEGER( I4B ), INTENT( IN ) :: PRECOND_TYPE
  SELECT CASE( PRECOND_TYPE )
  CASE( NO_PRECONDITION )
    IPAR(2) = 0
  CASE( LEFT_PRECONDITION )
    IPAR(2) = 1
  CASE( RIGHT_PRECONDITION )
    IPAR(2) = 2
  CASE( LEFT_RIGHT_PRECONDITION )
    IPAR(2) = 3
  END SELECT
END SUBROUTINE setPreconditionOption

!----------------------------------------------------------------------------
!                                                     setKrylovSubspaceSize
!----------------------------------------------------------------------------

SUBROUTINE setKrylovSubspaceSize( IPAR, m )
  INTEGER( I4B ), INTENT( INOUT ) :: IPAR( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: m
  IPAR( 5 ) = INPUT( default=15, option=m )
END SUBROUTINE setKrylovSubspaceSize

!----------------------------------------------------------------------------
!                                                                 setMatIter
!----------------------------------------------------------------------------

SUBROUTINE setMaxIter( IPAR, maxIter )
  INTEGER( I4B ), INTENT( INOUT ) :: IPAR( : )
  INTEGER( I4B ), INTENT( IN ) :: maxIter
  IPAR( 6 ) = maxIter
END SUBROUTINE setMaxIter

!----------------------------------------------------------------------------
!                                                          setConvergenceType
!----------------------------------------------------------------------------

SUBROUTINE setConvergenceType( IPAR, convergenceIn, convergenceType, &
  & relativeToRHS )
  INTEGER( I4B ), INTENT( INOUT ) :: IPAR( : )
  INTEGER( I4B ), INTENT( IN ) :: convergenceIn
  INTEGER( I4B ), INTENT( IN ) :: convergenceType
  LOGICAL( LGT ), INTENT( IN )  :: relativeToRHS
  !
  IPAR( 3 ) = 1
  SELECT CASE( convergenceType )
  CASE( absoluteConvergence )
    IF( convergenceIn .EQ. convergenceInSol ) THEN
      IPAR( 3 ) = -1
    ELSE IF( convergenceIn .EQ. convergenceInRes ) THEN
      IPAR( 3 ) = 1
    END IF
  CASE( relativeConvergence )
    IF( convergenceIn .EQ. convergenceInSol ) THEN
      IF( relativeToRHS ) THEN
        IPAR( 3 ) = -2
      ELSE
        IPAR( 3 ) = -1
      END IF
    ELSE IF( convergenceIn .EQ. convergenceInRes ) THEN
      IF( relativeToRHS ) THEN
        IPAR( 3 ) = 2
      ELSE
        IPAR( 3 ) = 1
      END IF
    END IF
  END SELECT
END SUBROUTINE setConvergenceType

!----------------------------------------------------------------------------
!                                                       setRelativeTolerance
!----------------------------------------------------------------------------

SUBROUTINE setRelativeTolerance( FPAR, tol )
  REAL( DFP ), INTENT( INOUT ) :: FPAR( : )
  REAL( DFP ), INTENT( IN ) :: tol
  FPAR( 1 ) = tol
END SUBROUTINE

!----------------------------------------------------------------------------
!                                                       setRelativeTolerance
!----------------------------------------------------------------------------

SUBROUTINE setAbsoluteTolerance( FPAR, tol )
  REAL( DFP ), INTENT( INOUT ) :: FPAR( : )
  REAL( DFP ), INTENT( IN ) :: tol
  FPAR( 2 ) = tol
END SUBROUTINE

!----------------------------------------------------------------------------
!                                                         setLinSolverParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setLinSolverParam
  INTEGER( I4B ) :: ierr
  ierr = param%set( key="LinSolver/solverName", value=solverName )
  ierr = param%set( key="LinSolver/preconditionOption", value=preconditionOption )
  ierr = param%set( key="LinSolver/convergenceIn", value=convergenceIn )
  ierr = param%set( key="LinSolver/convergenceType", value=convergenceType )
  ierr = param%set( key="LinSolver/maxIter", value=maxIter )
  ierr = param%set( key="LinSolver/engine", value="NATIVE_SERIAL" )
  IF( PRESENT( relativeToRHS ) ) THEN
    ierr = param%set( key="LinSolver/relativeToRHS", value=.FALSE. )
  ELSE
    ierr = param%set( key="LinSolver/relativeToRHS", value=relativeToRHS )
  END IF
  IF( PRESENT( KrylovSubspaceSize ) ) THEN
    ierr = param%set( key="LinSolver/KrylovSubspaceSize", value=15_I4B )
  ELSE
    ierr = param%set( key="LinSolver/KrylovSubspaceSize", value=KrylovSubspaceSize )
  END IF
  IF( PRESENT( rtol ) ) THEN
    ierr = param%set( key="LinSolver/rtol", value=REAL(1.0E-8, DFP) )
  ELSE
    ierr = param%set( key="LinSolver/rtol", value=rtol )
  END IF
  IF( PRESENT( atol ) ) THEN
    ierr = param%set( key="LinSolver/atol", value=REAL(1.0E-8, DFP) )
  ELSE
    ierr = param%set( key="LinSolver/atol", value=atol )
  END IF
  !> hello
END PROCEDURE setLinSolverParam

!----------------------------------------------------------------------------
!                                                         getLinSolverParam
!----------------------------------------------------------------------------

MODULE PROCEDURE getLinSolverParam
  INTEGER( I4B ) :: ierr
  ierr = param%get( key="LinSolver/solverName", value=solverName )
  ierr = param%get( key="LinSolver/preconditionOption", &
    & value=preconditionOption )
  ierr = param%get( key="LinSolver/convergenceIn", value=convergenceIn )
  ierr = param%get( key="LinSolver/convergenceType", value=convergenceType )
  ierr = param%get( key="LinSolver/maxIter", value=maxIter )
  ierr = param%get( key="LinSolver/relativeToRHS", value=relativeToRHS )
  ierr = param%get( key="LinSolver/KrylovSubspaceSize", &
    & value=KrylovSubspaceSize )
  ierr = param%get( key="LinSolver/rtol", value=rtol )
  ierr = param%get( key="LinSolver/atol", value=atol )
END PROCEDURE getLinSolverParam

!----------------------------------------------------------------------------
!                                                       checkEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_checkEssentialParam
  CHARACTER( LEN = * ), PARAMETER :: myName = "ls_checkEssentialParam"

  IF( .NOT. param%isPresent(key="LinSolver/solverName") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'LinSolver/solverName should be present in param')

  IF( .NOT. param%isPresent(key="LinSolver/preconditionOption") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'LinSolver/preconditionOption should be present in param')

  IF( .NOT. param%isPresent(key="LinSolver/convergenceIn") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'LinSolver/convergenceIn should be present in param')

  IF( .NOT. param%isPresent(key="LinSolver/convergenceType") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'LinSolver/convergenceType should be present in param')

  IF( .NOT. param%isPresent(key="LinSolver/maxIter") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'LinSolver/maxIter should be present in param')

  IF( .NOT. param%isPresent(key="LinSolver/relativeToRHS") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'LinSolver/relativeToRHS should be present in param')

  IF( .NOT. param%isPresent(key="LinSolver/KrylovSubspaceSize") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'LinSolver/KrylovSubspaceSize should be present in param')

  IF( .NOT. param%isPresent(key="LinSolver/rtol") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'LinSolver/rtol should be present in param')

  IF( .NOT. param%isPresent(key="LinSolver/atol") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'LinSolver/atol should be present in param')
END PROCEDURE ls_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Initiate
  INTEGER( I4B ) :: solverName, preconditionOption, convergenceIn, &
    & convergenceType, maxIter, KrylovSubspaceSize
  REAL( DFP ) :: rtol, atol
  LOGICAL( LGT ) :: relativeToRHS
  CALL obj%checkEssentialParam(param)
  CALL getLinSolverParam( param=param, solverName=solverName, &
    & preconditionOption=preconditionOption, convergenceIn=convergenceIn, &
    & convergenceType=convergenceType, maxIter=maxIter, &
    & relativeToRHS=relativeToRHS, KrylovSubspaceSize=KrylovSubspaceSize, &
    & rtol=rtol, atol=atol )
  obj%isInitiated = .TRUE.
  obj%engine = "NATIVE_SERIAL"
  obj%ierr = 0
  obj%iter=0
  obj%solverName=solverName
  obj%preconditionOption = preconditionOption
  obj%convergenceIn = convergenceIn
  obj%convergenceType = convergenceType
  obj%maxIter = maxIter
  obj%relativeToRHS = relativeToRHS
  obj%KrylovSubspaceSize = KrylovSubspaceSize
  obj%atol = atol
  obj%rtol = rtol
  obj%IPAR = 0
  CALL setPreconditionOption( obj%IPAR, preconditionOption )
  CALL setConvergenceType( obj%IPAR, convergenceIn, convergenceType, &
  & relativeToRHS )
  obj%IPAR( 5 ) = KrylovSubspaceSize
  CALL setMaxIter( obj%IPAR, maxIter )
  obj%FPAR = 0.0_DFP
  CALL setRelativeTolerance( obj%FPAR, rtol )
  CALL setAbsoluteTolerance( obj%FPAR, atol )
  CALL Reallocate(obj%RES, maxIter)
END PROCEDURE ls_Initiate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Deallocate
  obj%engine=''
  obj%IPAR=0
  obj%FPAR=0.0_DFP
  obj%isInitiated = .FALSE.
  obj%ierr = 0
  obj%iter=0
  obj%solverName=0
  obj%preconditionOption = 0
  obj%convergenceIn = 0
  obj%convergenceType = 0
  obj%maxIter = 0
  obj%relativeToRHS = .FALSE.
  obj%KrylovSubspaceSize = 15
  obj%atol = 1.0E-8
  obj%rtol = 1.0E-8
  obj%globalNumColumn = 0
  obj%globalNumRow = 0
  obj%localNumColumn = 0
  obj%localNumRow = 0
  IF( ALLOCATED( obj%RES ) ) DEALLOCATE( obj%RES )
  IF( ALLOCATED( obj%W ) ) DEALLOCATE( obj%W )
  NULLIFY( obj%Amat )
END PROCEDURE ls_Deallocate

END SUBMODULE Constructor