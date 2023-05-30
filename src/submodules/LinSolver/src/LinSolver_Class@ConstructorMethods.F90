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

SUBMODULE(LinSolver_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     setPreconditionOption
!----------------------------------------------------------------------------

SUBROUTINE setPreconditionOption(IPAR, PRECOND_TYPE)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: PRECOND_TYPE
  SELECT CASE (PRECOND_TYPE)
  CASE (NO_PRECONDITION)
    IPAR(2) = 0
  CASE (LEFT_PRECONDITION)
    IPAR(2) = 1
  CASE (RIGHT_PRECONDITION)
    IPAR(2) = 2
  CASE (LEFT_RIGHT_PRECONDITION)
    IPAR(2) = 3
  END SELECT
END SUBROUTINE setPreconditionOption

!----------------------------------------------------------------------------
!                                                     setKrylovSubspaceSize
!----------------------------------------------------------------------------

SUBROUTINE setKrylovSubspaceSize(IPAR, m)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: m
  IPAR(5) = INPUT(default=15, option=m)
END SUBROUTINE setKrylovSubspaceSize

!----------------------------------------------------------------------------
!                                                                 setMatIter
!----------------------------------------------------------------------------

SUBROUTINE setMaxIter(IPAR, maxIter)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: maxIter
  IPAR(6) = maxIter
END SUBROUTINE setMaxIter

!----------------------------------------------------------------------------
!                                                          setConvergenceType
!----------------------------------------------------------------------------

SUBROUTINE setConvergenceType(IPAR, convergenceIn, convergenceType, &
  & relativeToRHS)
  INTEGER(I4B), INTENT(INOUT) :: IPAR(:)
  INTEGER(I4B), INTENT(IN) :: convergenceIn
  INTEGER(I4B), INTENT(IN) :: convergenceType
  LOGICAL(LGT), INTENT(IN) :: relativeToRHS
  !
  IPAR(3) = 1
  SELECT CASE (convergenceType)
  CASE (absoluteConvergence)
    IF (convergenceIn .EQ. convergenceInSol) THEN
      IPAR(3) = -1
    ELSE IF (convergenceIn .EQ. convergenceInRes) THEN
      IPAR(3) = 1
    END IF
  CASE (relativeConvergence)
    IF (convergenceIn .EQ. convergenceInSol) THEN
      IF (relativeToRHS) THEN
        IPAR(3) = -2
      ELSE
        IPAR(3) = -1
      END IF
    ELSE IF (convergenceIn .EQ. convergenceInRes) THEN
      IF (relativeToRHS) THEN
        IPAR(3) = 2
      ELSE
        IPAR(3) = 1
      END IF
    END IF
  END SELECT
END SUBROUTINE setConvergenceType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE setTolerance(FPAR, atol, rtol)
  REAL(DFP), INTENT(INOUT) :: fpar(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: atol
  REAL(DFP), OPTIONAL, INTENT(IN) :: rtol

  IF (PRESENT(atol)) THEN
    FPAR(2) = atol
  END IF
  IF (PRESENT(rtol)) THEN
    FPAR(1) = rtol
  END IF
END SUBROUTINE setTolerance

!----------------------------------------------------------------------------
!                                                       checkEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_checkEssentialParam
CHARACTER(*), PARAMETER :: myName = "ls_checkEssentialParam"

! name
IF (.NOT. param%isPresent(key=myprefix//"/solverName")) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/solverName should be present in param')

! preconditionOption
IF (.NOT. param%isPresent(key=myprefix//"/preconditionOption")) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/preconditionOption should be present in param')

! convergenceIn
IF (.NOT. param%isPresent(key=myprefix//"/convergenceIn")) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/convergenceIn should be present in param')

! convergenceType
IF (.NOT. param%isPresent(key=myprefix//"/convergenceType")) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/convergenceType should be present in param')

! maxIter
IF (.NOT. param%isPresent(key=myprefix//"/maxIter")) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/maxIter should be present in param')

! relativeToRHS
IF (.NOT. param%isPresent(key=myprefix//"/relativeToRHS")) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/relativeToRHS should be present in param')

! KrylovSubspaceSize
IF (.NOT. param%isPresent(key=myprefix//"/KrylovSubspaceSize")) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/KrylovSubspaceSize should be present in param')

! rtol
IF (.NOT. param%isPresent(key=myprefix//"/rtol")) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/rtol should be present in param')

! atol
IF (.NOT. param%isPresent(key=myprefix//"/atol")) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & myprefix//'/atol should be present in param')
END PROCEDURE ls_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Initiate
INTEGER(I4B) :: solverName, preconditionOption, convergenceIn, &
  & convergenceType, maxIter, KrylovSubspaceSize
REAL(DFP) :: rtol, atol
LOGICAL(LGT) :: relativeToRHS

CALL obj%checkEssentialParam(param)
CALL getAbstractLinSolverParam( &
  & param=param, &
  & prefix=myprefix, &
  & solverName=solverName, &
  & preconditionOption=preconditionOption, &
  & convergenceIn=convergenceIn, &
  & convergenceType=convergenceType, &
  & maxIter=maxIter, &
  & relativeToRHS=relativeToRHS, &
  & KrylovSubspaceSize=KrylovSubspaceSize, &
  & rtol=rtol, &
  & atol=atol)

CALL obj%SetParam(&
  & isInitiated=.TRUE., &
  & engine="NATIVE_SERIAL", &
  & ierr=0_I4B, &
  & iter=0_I4B, &
  & solverName=solverName, &
  & preconditionOption=preconditionOption, &
  & convergenceIn=convergenceIn, &
  & convergenceType=convergenceType, &
  & maxIter=maxIter, &
  & relativeToRHS=relativeToRHS, &
  & KrylovSubspaceSize=KrylovSubspaceSize, &
  & atol=atol, &
  & rtol=rtol &
  & )

obj%IPAR = 0
CALL setPreconditionOption(obj%IPAR, preconditionOption)
CALL setConvergenceType(obj%IPAR, convergenceIn, convergenceType, &
  & relativeToRHS)
obj%IPAR(5) = KrylovSubspaceSize
CALL setMaxIter(obj%IPAR, maxIter)
obj%FPAR = 0.0_DFP
CALL setTolerance(fpar=obj%fpar, rtol=rtol, atol=atol)
! CALL Reallocate(obj%RES, maxIter)
! CALL Reallocate(obj%dbcIndx, 0)
END PROCEDURE ls_Initiate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Deallocate
CALL AbstractLinSolverDeallocate(obj)
obj%ipar = 0
obj%fpar = 0.0_DFP
IF (ALLOCATED(obj%W)) DEALLOCATE (obj%W)
END PROCEDURE ls_Deallocate

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_final
CALL obj%DEALLOCATE()
END PROCEDURE ls_final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
