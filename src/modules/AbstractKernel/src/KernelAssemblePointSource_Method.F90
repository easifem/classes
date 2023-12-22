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

MODULE KernelAssemblePointSource_Method
USE GlobalData
USE Field
! USE BaseMethod
! USE BaseType
! USE FieldFactory
! USE Domain_Class
! USE Mesh_Class
! USE FiniteElement_Class
USE ExceptionHandler_Class, ONLY: e
! USE UserFunction_Class
USE NeumannBC_Class
IMPLICIT NONE

PRIVATE
PUBLIC :: KernelAssemblePointSource

CHARACTER(*), PARAMETER :: modName = "KernelAssemblePointSource_Method"

INTERFACE KernelAssemblePointSource
  MODULE PROCEDURE KernelAssemblePointSource1
END INTERFACE KernelAssemblePointSource

CONTAINS

!----------------------------------------------------------------------------
!                                                 KernelAssemblePointSource
!----------------------------------------------------------------------------

SUBROUTINE KernelAssemblePointSource1(rhs, nbcPtrs, reset, scale, times)
  CLASS(VectorField_), INTENT(INOUT) :: rhs
  !! rhs to assemble
  TYPE(NeumannBCPointer_), INTENT(INOUT) :: nbcPtrs(:)
  !! Neumann boundary condition pointers
  LOGICAL(LGT), INTENT(IN) :: reset
  !! Reset rhs before assembly
  REAL(DFP), INTENT(IN) :: scale
  !! scale to add
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !! time vector

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssemblePointSource1()"
  CLASS(NeumannBC_), POINTER :: nbc
  LOGICAL(LGT) :: problem, isSelectionByNodeNum, istimes
  INTEGER(I4B) :: tnbc, nbcNo, idof
  INTEGER(I4B), ALLOCATABLE :: nodeNum(:)
  REAL(DFP), ALLOCATABLE :: nodalValue(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL rhs%Set(VALUE=0.0_DFP)

  istimes = PRESENT(times)
  IF (istimes) THEN
    nbcNo = SIZE(times)
    problem = nbcNo .NE. 1_I4B
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: size of times should be 1.')
      RETURN
    END IF
  END IF

  tnbc = SIZE(nbcPtrs)
  NULLIFY (nbc)

  DO nbcNo = 1, tnbc
    nbc => NULL()
    nbc => nbcPtrs(nbcNo)%ptr

    problem = .NOT. ASSOCIATED(nbc)
    IF (problem) CYCLE
    CALL nbc%GetParam(isSelectionByNodeNum=isSelectionByNodeNum)
    problem = .NOT. isSelectionByNodeNum
    IF (problem) CYCLE

    CALL nbc%Get(nodeNum=nodeNum, nodalValue=nodalValue, times=times)
    idof = nbc%GetDOFNo()

    CALL rhs%Set(globalNode=nodeNum, VALUE=nodalValue(:, 1),  &
      & spaceCompo=idof, scale=scale, addContribution=.TRUE.)
  END DO

  NULLIFY (nbc)
  IF (ALLOCATED(nodeNum)) DEALLOCATE (nodeNum)
  IF (ALLOCATED(nodalValue)) DEALLOCATE (nodalValue)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelAssemblePointSource1

END MODULE KernelAssemblePointSource_Method
