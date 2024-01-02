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

MODULE KernelMatrixField_Method
USE GlobalData
USE Field
USE BaseMethod
USE BaseType
USE AbstractLinSolver_Class
USE FieldFactory
USE SolidMaterial_Class
USE Domain_Class
USE Mesh_Class
USE ExceptionHandler_Class, ONLY: e
USE AbstractKernelParam, ONLY: KernelProblemType
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "KernelMatrixField_Method"

PUBLIC :: KernelInitiateSpaceMatrix
PUBLIC :: KernelInitiateTangentMatrix

CONTAINS

!----------------------------------------------------------------------------
!                                                        InitiateMassMatrix
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateSpaceMatrix(mat, dom, nsd, engine,  &
  & problemType, name, matrixProp)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  TYPE(Domain_), INTENT(INOUT) :: dom
  INTEGER(I4B), INTENT(IN) :: nsd
  CHARACTER(*), INTENT(IN) :: engine
  INTEGER(I4B), INTENT(IN) :: problemType
  CHARACTER(*), INTENT(IN) :: name
  CHARACTER(*), INTENT(IN) :: matrixProp

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateSpaceMatrix()"
  TYPE(ParameterList_) :: param
  LOGICAL(LGT) :: problem
  INTEGER(I4B) :: spaceCompo

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  problem = nsd .EQ. 0_I4B
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: obj%nsd=0 found')
    RETURN
  END IF

  CALL param%Initiate()

  IF (problemType .EQ. kernelProblemType%scalar) THEN
    spaceCompo = 1
  ELSE IF (problemType .EQ. kernelProblemType%vector) THEN
    spaceCompo = nsd
  ELSE
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: Unknown problemType = '// &
      & tostring(problemType))
    RETURN
  END IF

  CALL SetMatrixFieldParam( &
    & param=param, &
    & name=name, &
    & matrixProp=matrixProp, &
    & spaceCompo=spaceCompo, &
    & timeCompo=1, &
    & fieldType=FIELD_TYPE_NORMAL, &
    & engine=engine)

  CALL mat%Initiate(param=param, dom=dom)
  CALL param%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelInitiateSpaceMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateTangentMatrix(mat, linsol, dom, nsd, nnt, engine,  &
  & name, matrixProp)
  CLASS(AbstractMatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractLinSolver_), INTENT(INOUT) :: linsol
  TYPE(Domain_), INTENT(INOUT) :: dom
  INTEGER(I4B), INTENT(IN) :: nsd
  INTEGER(I4B), INTENT(IN) :: nnt
  CHARACTER(*), INTENT(IN) :: engine
  CHARACTER(*), INTENT(IN) :: name
  CHARACTER(*), INTENT(IN) :: matrixProp
! Internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateTangentMatrix()"
  TYPE(ParameterList_) :: param
  LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  problem = nsd .EQ. 0_I4B .OR. nnt .EQ. 0_I4B

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: nsd=0 or nnt=0 found')
    RETURN
  END IF

  CALL param%Initiate()

  CALL SetMatrixFieldParam( &
    & param=param, &
    & name=name, &
    & matrixProp=matrixProp, &
    & spaceCompo=nsd, &
    & timeCompo=nnt, &
    & fieldType=FIELD_TYPE_NORMAL, &
    & engine=engine)

  CALL mat%Initiate(param=param, dom=dom)
  CALL linsol%Set(mat)

  CALL param%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelInitiateTangentMatrix

END MODULE KernelMatrixField_Method
