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

SUBMODULE(UserFunction_Class) GetFEVariableMethods
USE BaseType, ONLY: varopt => TypeFEVariableOpt, &
           TypeFEVariableScalar, TypeFEVariableVector, TypeFEVariableMatrix, &
           TypeFEVariableSpace, TypeFEVariableTime, TypeFEVariableSpaceTime, &
                    TypeFEVariableConstant
USE GlobalData, ONLY: CHAR_LF
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
USE FEVariable_Method, ONLY: NodalVariable

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFEVariable()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

SELECT CASE (obj%argType)

CASE (varopt%Constant)

  SELECT CASE (obj%returnType)

  CASE (varopt%Scalar)
    CALL Scalar_Constant_GetVariable(obj=obj, fevar=fevar)
  CASE (varopt%Vector)
    CALL Vector_Constant_GetVariable(obj=obj, fevar=fevar)
  CASE (varopt%Matrix)
    CALL Matrix_Constant_GetVariable(obj=obj, fevar=fevar)
  END SELECT

CASE (varopt%Space)

  SELECT CASE (obj%returnType)
  CASE (varopt%Scalar)
    CALL Scalar_Space_GetVariable(obj=obj, fevar=fevar, xij=xij)
  CASE (varopt%Vector)
    CALL Vector_Space_GetVariable(obj=obj, fevar=fevar, xij=xij)
  CASE (varopt%Matrix)
    CALL Matrix_Space_GetVariable(obj=obj, fevar=fevar, xij=xij)
  END SELECT

CASE (varopt%Time)

  SELECT CASE (obj%returnType)
  CASE (varopt%Scalar)
    CALL Scalar_Time_GetVariable(obj=obj, fevar=fevar, timeVec=times)
  CASE (varopt%Vector)
    CALL Vector_Time_GetVariable(obj=obj, fevar=fevar, timeVec=times)
  CASE (varopt%Matrix)
    CALL Matrix_Time_GetVariable(obj=obj, fevar=fevar, timeVec=times)
  END SELECT

CASE (varopt%SpaceTime)

  SELECT CASE (obj%returnType)
  CASE (varopt%Scalar)
    CALL Scalar_SpaceTime_GetVariable(obj=obj, fevar=fevar, xij=xij, &
                                      timeVec=times)
  CASE (varopt%Vector)
    CALL Vector_SpaceTime_GetVariable(obj=obj, fevar=fevar, xij=xij, &
                                      timeVec=times)
  CASE (varopt%Matrix)
    CALL Matrix_SpaceTime_GetVariable(obj=obj, fevar=fevar, xij=xij, &
                                      timeVec=times)
  END SELECT

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Scalar_Constant_GetVariable(obj, fevar)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: fevar

  ! Internal variable
  REAL(DFP) :: val
  CALL obj%Get(val=val)
  fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableConstant)
END SUBROUTINE Scalar_Constant_GetVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Scalar_Space_GetVariable(obj, fevar, xij)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: fevar
  REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)

  ! internal variable
  CHARACTER(*), PARAMETER :: myName = "Scalar_Constant_GetVariable()"
  REAL(DFP), ALLOCATABLE :: val(:)
  LOGICAL(LGT) :: isxij
  INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isxij = PRESENT(xij)
  IF (.NOT. isxij) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTENRAL ERROR] :: xij should be present.')
    RETURN
  END IF

  tsize = SIZE(xij, 2)
  CALL Reallocate(val, tsize)
  DO ii = 1, tsize
    CALL obj%Get(val=val(ii), args=xij(:, ii))
  END DO
  fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableSpace)
  IF (ALLOCATED(val)) DEALLOCATE (val)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE Scalar_Space_GetVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Scalar_Time_GetVariable(obj, fevar, timeVec)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: fevar
  REAL(DFP), OPTIONAL, INTENT(IN) :: timeVec(:)

  ! internal variable
  CHARACTER(*), PARAMETER :: myName = "Scalar_Time_GetVariable()"
  REAL(DFP), ALLOCATABLE :: val(:)
  LOGICAL(LGT) :: istimevec
  INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  istimevec = PRESENT(timeVec)

  IF (.NOT. istimevec) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTENRAL ERROR] :: timeVec should be present.')
    RETURN
  END IF

  tsize = SIZE(timeVec)
  CALL Reallocate(val, tsize)
  DO ii = 1, tsize
    CALL obj%Get(val=val(ii), args=timeVec(ii:ii))
  END DO
  fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableTime)
  IF (ALLOCATED(val)) DEALLOCATE (val)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE Scalar_Time_GetVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Scalar_SpaceTime_GetVariable(obj, fevar, xij, timeVec)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: fevar
  REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  REAL(DFP), OPTIONAL, INTENT(IN) :: timeVec(:)

  ! internal variable
  CHARACTER(*), PARAMETER :: myName = "Scalar_SpaceTime_GetVariable()"
  REAL(DFP), ALLOCATABLE :: val(:, :), args(:)
  LOGICAL(LGT) :: problem
  INTEGER(I4B) :: ii, tspace, ttime, jj, nsd

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  problem = (.NOT. PRESENT(xij)) .OR. (.NOT. PRESENT(timeVec))
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                     '[INTENRAL ERROR] :: xij and timeVec should be present.')
    RETURN
  END IF

  tspace = SIZE(xij, 2)
  ttime = SIZE(timeVec)
  nsd = SIZE(xij, 1)

  CALL Reallocate(val, tspace, ttime)
  CALL Reallocate(args, obj%numArgs)

  DO jj = 1, ttime
    args = timeVec(jj)
    DO ii = 1, tspace
      args(1:nsd) = xij(1:nsd, ii)
      CALL obj%Get(val=val(ii, jj), args=args)
    END DO
  END DO
  fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableSpaceTime)
  IF (ALLOCATED(val)) DEALLOCATE (val)
  IF (ALLOCATED(args)) DEALLOCATE (args)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE Scalar_SpaceTime_GetVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Vector_Constant_GetVariable(obj, fevar)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: fevar

  ! internal variable
  REAL(DFP), ALLOCATABLE :: val(:)

  CALL obj%Get(val=val)
  fevar = NodalVariable(val, TypeFEVariableVector, TypeFEVariableConstant)
  IF (ALLOCATED(val)) DEALLOCATE (val)
END SUBROUTINE Vector_Constant_GetVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Vector_Space_GetVariable(obj, fevar, xij)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: fevar
  REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)

  ! internal variable
  CHARACTER(*), PARAMETER :: myName = "Vector_Space_GetVariable()"
  REAL(DFP), ALLOCATABLE :: val(:, :), r1(:)
  LOGICAL(LGT) :: isxij
  INTEGER(I4B) :: nrow, ncol, jj

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isxij = PRESENT(xij)

  IF (.NOT. isxij) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTENRAL ERROR] :: xij should be present.')
    RETURN
  END IF

  nrow = obj%numReturns
  ncol = SIZE(xij, 2)
  CALL Reallocate(val, nrow, ncol)

  DO jj = 1, ncol
    CALL obj%Get(val=r1, args=xij(:, jj))
    val(1:nrow, jj) = r1(1:nrow)
  END DO

  fevar = NodalVariable(val, TypeFEVariableVector, TypeFEVariableSpace)
  IF (ALLOCATED(val)) DEALLOCATE (val)
  IF (ALLOCATED(r1)) DEALLOCATE (r1)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE Vector_Space_GetVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Vector_Time_GetVariable(obj, fevar, timeVec)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: fevar
  REAL(DFP), OPTIONAL, INTENT(IN) :: timeVec(:)

  ! internal variable
  CHARACTER(*), PARAMETER :: myName = "Vector_Time_GetVariable()"
  REAL(DFP), ALLOCATABLE :: val(:, :), r1(:)
  LOGICAL(LGT) :: istimevec
  INTEGER(I4B) :: nrow, ncol, jj

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  istimevec = PRESENT(timeVec)
  IF (.NOT. istimevec) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTENRAL ERROR] :: timeVec should be present.')
    RETURN
  END IF

  nrow = obj%numReturns
  ncol = SIZE(timeVec)
  CALL Reallocate(val, nrow, ncol)

  DO jj = 1, ncol
    CALL obj%Get(val=r1, args=timeVec(jj:jj))
    val(1:nrow, jj) = r1(1:nrow)
  END DO

  fevar = NodalVariable(val, TypeFEVariableVector, TypeFEVariableTime)
  IF (ALLOCATED(val)) DEALLOCATE (val)
  IF (ALLOCATED(r1)) DEALLOCATE (r1)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE Vector_Time_GetVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Vector_SpaceTime_GetVariable(obj, fevar, xij, timeVec)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: fevar
  REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  REAL(DFP), OPTIONAL, INTENT(IN) :: timeVec(:)

  ! internal variable
  CHARACTER(*), PARAMETER :: myName = "Vector_SpaceTime_GetVariable()"
  REAL(DFP), ALLOCATABLE :: val(:, :, :), r1(:), args(:)
  LOGICAL(LGT) :: isxij
  INTEGER(I4B) :: dim1, dim2, dim3, jj, kk, nsd

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isxij = PRESENT(xij)
  IF (.NOT. isxij) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                     '[INTENRAL ERROR] :: xij and timeVec should be present.')
    RETURN
  END IF
  dim1 = obj%numReturns
  dim2 = SIZE(xij, 2)
  nsd = SIZE(xij, 1)
  dim3 = SIZE(timeVec)
  CALL Reallocate(val, dim1, dim2, dim3)
  CALL Reallocate(args, obj%numArgs)

  DO kk = 1, dim3
    args = timeVec(kk)
    DO jj = 1, dim2
      args(1:nsd) = xij(1:nsd, jj)
      CALL obj%Get(val=r1, args=args)
      val(1:dim1, jj, kk) = r1(1:dim1)
    END DO
  END DO

  fevar = NodalVariable(val, TypeFEVariableVector, TypeFEVariableSpaceTime)
  IF (ALLOCATED(val)) DEALLOCATE (val)
  IF (ALLOCATED(r1)) DEALLOCATE (r1)
  IF (ALLOCATED(args)) DEALLOCATE (args)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE Vector_SpaceTime_GetVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Matrix_Constant_GetVariable(obj, fevar)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: fevar

  ! internal variable
  REAL(DFP), ALLOCATABLE :: val(:, :)

  CALL obj%Get(val=val)
  fevar = NodalVariable(val, TypeFEVariableMatrix, TypeFEVariableConstant)
  IF (ALLOCATED(val)) DEALLOCATE (val)
END SUBROUTINE Matrix_Constant_GetVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Matrix_Space_GetVariable(obj, fevar, xij)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: fevar
  REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)

  ! internal variable
  CHARACTER(*), PARAMETER :: myName = "Matrix_Space_GetVariable()"
  REAL(DFP), ALLOCATABLE :: val(:, :, :), r2(:, :), args(:)
  LOGICAL(LGT) :: isxij
  INTEGER(I4B) :: jj, dim1, dim2, dim3

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isxij = PRESENT(xij)
  IF (.NOT. isxij) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTENRAL ERROR] :: xij should be present.')
    RETURN
  END IF

  dim1 = obj%returnShape(1)
  dim2 = obj%returnShape(2)
  dim3 = SIZE(xij, 2)

  CALL Reallocate(val, dim1, dim2, dim3)
  CALL Reallocate(args, obj%numArgs)

  DO jj = 1, dim3
    args(1:obj%numArgs) = xij(1:obj%numArgs, jj)
    CALL obj%Get(val=r2, args=args)
    val(1:dim1, 1:dim2, jj) = r2(1:dim1, 1:dim2)
  END DO

  fevar = NodalVariable(val, TypeFEVariableMatrix, TypeFEVariableSpace)
  IF (ALLOCATED(val)) DEALLOCATE (val)
  IF (ALLOCATED(r2)) DEALLOCATE (r2)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE Matrix_Space_GetVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Matrix_Time_GetVariable(obj, fevar, timeVec)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: fevar
  REAL(DFP), OPTIONAL, INTENT(IN) :: timeVec(:)

  ! internal variable
  CHARACTER(*), PARAMETER :: myName = "Matrix_Time_GetVariable()"
  REAL(DFP), ALLOCATABLE :: val(:, :, :), r2(:, :)
  LOGICAL(LGT) :: istimevec
  INTEGER(I4B) :: jj, dim1, dim2, dim3

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  istimevec = PRESENT(timeVec)
  IF (.NOT. istimevec) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTENRAL ERROR] :: timeVec should be present.')
    RETURN
  END IF

  dim1 = obj%returnShape(1)
  dim2 = obj%returnShape(2)
  dim3 = SIZE(timeVec)
  CALL Reallocate(val, dim1, dim2, dim3)

  DO jj = 1, dim3
    CALL obj%Get(val=r2, args=timeVec(jj:jj))
    val(1:dim1, 1:dim2, jj) = r2(1:dim1, 1:dim2)
  END DO

  fevar = NodalVariable(val, TypeFEVariableMatrix, TypeFEVariableTime)
  IF (ALLOCATED(val)) DEALLOCATE (val)
  IF (ALLOCATED(r2)) DEALLOCATE (r2)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

END SUBROUTINE Matrix_Time_GetVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Matrix_SpaceTime_GetVariable(obj, fevar, xij, timeVec)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: fevar
  REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  REAL(DFP), OPTIONAL, INTENT(IN) :: timeVec(:)

  ! internal variable
  CHARACTER(*), PARAMETER :: myName = "Matrix_SpaceTime_GetVariable()"
  REAL(DFP), ALLOCATABLE :: val(:, :, :, :), r2(:, :), args(:)
  LOGICAL(LGT) :: problem
  INTEGER(I4B) :: ii, jj, dim1, dim2, dim3, dim4

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

  problem = (.NOT. PRESENT(xij)) .OR. (.NOT. PRESENT(timeVec))

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTENRAL ERROR] :: xij should be present.')
    RETURN
  END IF

  dim1 = obj%returnShape(1)
  dim2 = obj%returnShape(2)
  dim3 = SIZE(xij, 2)
  dim4 = SIZE(timeVec)

  CALL Reallocate(val, dim1, dim2, dim3, dim4)
  CALL Reallocate(args, obj%numArgs)

  DO jj = 1, dim4
    args = timeVec(jj)
    DO ii = 1, dim3
      args(1:obj%numArgs) = xij(1:obj%numArgs, ii)
      CALL obj%Get(val=r2, args=args)
      val(1:dim1, 1:dim2, ii, jj) = r2(1:dim1, 1:dim2)
    END DO
  END DO

  fevar = NodalVariable(val, TypeFEVariableMatrix, TypeFEVariableSpaceTime)
  IF (ALLOCATED(val)) DEALLOCATE (val)
  IF (ALLOCATED(r2)) DEALLOCATE (r2)
  IF (ALLOCATED(args)) DEALLOCATE (args)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

END SUBROUTINE Matrix_SpaceTime_GetVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetFEVariableMethods
