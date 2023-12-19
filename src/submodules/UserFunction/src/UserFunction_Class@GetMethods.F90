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

SUBMODULE(UserFunction_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    GetName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetName
ans = obj%name%chars()
END PROCEDURE obj_GetName

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetScalarValue
LOGICAL(LGT) :: isOK, isNotOK
CHARACTER(*), PARAMETER :: myName = "obj_GetScalarValue()"
#ifdef USE_LUA
TYPE(C_PTR) :: l
INTEGER(I4B) :: rc, nargs, nresults, iarg
LOGICAL(LGT) :: isFunction
#endif

isOK = obj%returnType .EQ. Scalar
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: The user function is not configured for '//  &
    & ' returnType = Scalar')
  RETURN
END IF

val = obj%scalarValue

IF (obj%isUserFunctionSet) THEN
  isOK = ASSOCIATED(obj%scalarFunction)
  IF (.NOT. isOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isUserFunctionSet is true '//  &
      & CHAR_LF//' but obj%scalarFunction is not ASSOCIATED.')
    RETURN
  END IF
  val = obj%scalarFunction(x=args)
END IF

#ifdef USE_LUA

IF (obj%isLuaScript) THEN
  nargs = obj%numArgs
  nresults = obj%numReturns

  isNotOK = nresults .NE. 1_I4B
  IF (isNotOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%numReturns should be 1'// &
      & CHAR_LF//' but it is '//tostring(obj%numReturns))
    RETURN
  END IF

  IF (PRESENT(args)) THEN
    isNotOK = nargs .NE. SIZE(args)
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: UserFunction_::numArgs( '//CHAR_LF// &
        & tostring(obj%numArgs)//' ) should be same as size of args ('// &
        & CHAR_LF//tostring(SIZE(args))//').')
      RETURN
    END IF

  ELSE

    isNotOK = nargs .NE. 0_I4B
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: UserFunction_::numArgs( '//CHAR_LF// &
        & tostring(obj%numArgs)//' ) should be equal to 0 when '// &
        & CHAR_LF//'args is not present.')
      RETURN
    END IF
  END IF

  l = lual_newstate()
  CALL lual_openlibs(l)
  rc = lual_dofile(l, obj%luaScript%chars())
  rc = lua_getglobal(l, obj%luaFunctionName%chars())
  isFunction = lua_isfunction(l, -1) .EQ. 1

  IF (.NOT. isFunction) THEN
    CALL lua_close(l)
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'//  &
      & CHAR_LF//'In the lua script'//obj%luaScript%chars()//  &
      & CHAR_LF//'lua function named '//obj%luaFunctionName%chars()//  &
      & CHAR_LF//' is not a function.')
    RETURN
  END IF

  DO iarg = 1, nargs
    CALL lua_pushnumber(l, REAL(args(iarg), kind=lua_number))
  END DO

  rc = lua_pcall(l, nargs, nresults, 0)
  IF (rc .NE. lua_ok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'//  &
      & CHAR_LF//'Some error occured while calling lua_pcall(); '//  &
      & CHAR_LF//tostring(rc))
    RETURN
  END IF
  val = REAL(lua_tonumber(l, -1), kind=DFP)
  CALL lua_pop(l, 1)
  CALL lua_close(l)
END IF

#else
IF (obj%isLuaScript) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP ERROR] :: Currently  lua script cannot be used for '//  &
    & ' UserFunction. ')
  RETURN
END IF
#endif

END PROCEDURE obj_GetScalarValue

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVectorValue1
CHARACTER(*), PARAMETER :: myName = "obj_GetVectorValue1()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

CALL Reallocate(val, obj%numReturns)
CALL obj%GetVectorValue(n=obj%numReturns, val=val, args=args)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_GetVectorValue1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVectorValue
LOGICAL(LGT) :: isOK, isNotOK
CHARACTER(*), PARAMETER :: myName = "obj_GetVectorValue()"

#ifdef USE_LUA
TYPE(C_PTR) :: l
INTEGER(I4B) :: rc, nargs, nresults, iarg
LOGICAL(LGT) :: isFunction
#endif

isOK = obj%returnType .EQ. Vector
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: The user function is not configured for '//  &
    & ' returnType = Vector')
  RETURN
END IF

isOK = obj%numReturns .EQ. n
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: error in numReturns.')
  RETURN
END IF

isOK = ALLOCATED(obj%vectorValue)
IF (isOK) THEN
#ifdef USE_BLAS95
  CALL Copy(y=val, x=obj%vectorValue)
#else
  val = obj%vectorValue
#endif
END IF

IF (obj%isUserFunctionSet) THEN
  isOK = ASSOCIATED(obj%vectorFunction)
  IF (.NOT. isOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isUserFunctionSet is true '//  &
      & CHAR_LF//' but obj%vectorFunction is not ASSOCIATED.')
    RETURN
  END IF
  val = obj%vectorFunction(x=args)
END IF

#ifdef USE_LUA

IF (obj%isLuaScript) THEN
  nargs = obj%numArgs
  nresults = obj%numReturns
  ! CALL Reallocate(val, obj%numReturns)

  IF (PRESENT(args)) THEN
    isNotOK = nargs .NE. SIZE(args)
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: UserFunction_::numArgs( '//  &
        & CHAR_LF//tostring(obj%numArgs)//  &
        & ' ) should be same as size of args ('// &
        & CHAR_LF//tostring(SIZE(args))//').')
      RETURN
    END IF

  ELSE

    isNotOK = nargs .NE. 0_I4B
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: UserFunction_::numArgs( '//CHAR_LF// &
        & tostring(obj%numArgs)//' ) should be equal to 0 when '// &
        & CHAR_LF//'args is not present.')
      RETURN
    END IF
  END IF

  l = lual_newstate()
  CALL lual_openlibs(l)
  rc = lual_dofile(l, obj%luaScript%chars())
  rc = lua_getglobal(l, obj%luaFunctionName%chars())
  isFunction = lua_isfunction(l, -1) .EQ. 1

  IF (.NOT. isFunction) THEN
    CALL lua_close(l)
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'//  &
      & CHAR_LF//'In the lua script'//obj%luaScript%chars()//  &
      & CHAR_LF//'lua function named '//obj%luaFunctionName%chars()//  &
      & CHAR_LF//' is not a function.')
    RETURN
  END IF

  DO iarg = 1, nargs
    CALL lua_pushnumber(l, REAL(args(iarg), kind=lua_number))
  END DO

  rc = lua_pcall(l, nargs, nresults, 0)
  IF (rc .NE. lua_ok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'//  &
      & CHAR_LF//'Some error occured while calling lua_pcall(); '//  &
      & CHAR_LF//tostring(rc))
    RETURN
  END IF

  DO iarg = nresults, 1, -1
    val(iarg) = REAL(lua_tonumber(l, nresults + iarg - 1), kind=DFP)
  END DO
  CALL lua_pop(l, nresults)
  CALL lua_close(l)
END IF

#else
IF (obj%isLuaScript) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP ERROR] :: Currently  lua script cannot be used for '//  &
    & ' UserFunction. ')
  RETURN
END IF
#endif

END PROCEDURE obj_GetVectorValue

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMatrixValue
LOGICAL(LGT) :: isOK, isNotOK
CHARACTER(*), PARAMETER :: myName = "obj_GetMatrixValue()"
INTEGER(I4B) :: myshape(2)
#ifdef USE_LUA
TYPE(C_PTR) :: l
INTEGER(I4B) :: rc, nargs, nresults, iarg
LOGICAL(LGT) :: isFunction
REAL(DFP), ALLOCATABLE :: dummyvec(:)
#endif

isOK = obj%returnType .EQ. Matrix
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: The user function is not configured for '//  &
    & ' returnType = Vector')
  RETURN
END IF

IF (ALLOCATED(obj%matrixValue)) THEN
  myshape = SHAPE(obj%matrixValue)
  CALL Reallocate(val, myshape(1), myshape(2))
  val = obj%matrixValue
END IF

IF (obj%isUserFunctionSet) THEN
  isOK = ASSOCIATED(obj%matrixFunction)
  IF (.NOT. isOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isUserFunctionSet '//  &
      & CHAR_LF//'is true but obj%matrixFunction is not ASSOCIATED.')
    RETURN
  END IF
  CALL Reallocate(val, obj%returnShape(1), obj%returnShape(2))
  val = obj%matrixFunction(x=args)
END IF

#ifdef USE_LUA

IF (obj%isLuaScript) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP Progress]')
  nargs = obj%numArgs
  nresults = obj%numReturns

  CALL Reallocate(dummyvec, nresults)

  IF (PRESENT(args)) THEN
    isNotOK = nargs .NE. SIZE(args)
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: UserFunction_::numArgs( '//  &
        & CHAR_LF//tostring(obj%numArgs)//  &
        & ' ) should be same as size of args ('// &
        & CHAR_LF//tostring(SIZE(args))//').')
      RETURN
    END IF

  ELSE

    isNotOK = nargs .NE. 0_I4B
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: UserFunction_::numArgs( '//CHAR_LF// &
        & tostring(obj%numArgs)//' ) should be equal to 0 when '// &
        & CHAR_LF//'args is not present.')
      RETURN
    END IF
  END IF

  l = lual_newstate()
  CALL lual_openlibs(l)
  rc = lual_dofile(l, obj%luaScript%chars())
  rc = lua_getglobal(l, obj%luaFunctionName%chars())
  isFunction = lua_isfunction(l, -1) .EQ. 1

  IF (.NOT. isFunction) THEN
    CALL lua_close(l)
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'//  &
      & CHAR_LF//'In the lua script'//obj%luaScript%chars()//  &
      & CHAR_LF//'lua function named '//obj%luaFunctionName%chars()//  &
      & CHAR_LF//' is not a function.')
    RETURN
  END IF

  DO iarg = 1, nargs
    CALL lua_pushnumber(l, REAL(args(iarg), kind=lua_number))
  END DO

  rc = lua_pcall(l, nargs, nresults, 0)
  IF (rc .NE. lua_ok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'//  &
      & CHAR_LF//'Some error occured while calling lua_pcall(); '//  &
      & CHAR_LF//tostring(rc))
    RETURN
  END IF

  DO iarg = nresults, 1, -1
    dummyvec(iarg) = REAL(lua_tonumber(l, nresults + iarg - 1), kind=DFP)
  END DO
  CALL lua_pop(l, nresults)
  CALL lua_close(l)

  CALL Reallocate(val, obj%returnShape(1), obj%returnShape(2))
  val = RESHAPE(dummyvec, obj%returnShape)
  IF (ALLOCATED(dummyvec)) THEN
    DEALLOCATE (dummyvec)
  END IF
END IF

#else
IF (obj%isLuaScript) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP ERROR] :: Currently  lua script cannot be used for '//  &
    & ' UserFunction. ')
  RETURN
END IF
#endif

END PROCEDURE obj_GetMatrixValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetArgType
ans = obj%argType
END PROCEDURE obj_GetArgType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetReturnType
ans = obj%returnType
END PROCEDURE obj_GetReturnType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNumReturns
ans = obj%numReturns
END PROCEDURE obj_GetNumReturns

!----------------------------------------------------------------------------
!                                                           GetReturnShape
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetReturnShape
ans = obj%returnShape
END PROCEDURE obj_GetReturnShape

!----------------------------------------------------------------------------
!                                                                     Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
CHARACTER(*), PARAMETER :: myName = "obj_GetFEVariable()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

SELECT CASE (obj%argType)

CASE (Constant)

  SELECT CASE (obj%returnType)
  CASE (Scalar)
    CALL Scalar_Constant_GetVariable(obj=obj, fevar=fevar)
  CASE (Vector)
    CALL Vector_Constant_GetVariable(obj=obj, fevar=fevar)
  CASE (Matrix)
    CALL Matrix_Constant_GetVariable(obj=obj, fevar=fevar)
  END SELECT

CASE (Space)

  SELECT CASE (obj%returnType)
  CASE (Scalar)
    CALL Scalar_Space_GetVariable(obj=obj, fevar=fevar, xij=xij)
  CASE (Vector)
    CALL Vector_Space_GetVariable(obj=obj, fevar=fevar, xij=xij)
  CASE (Matrix)
    CALL Matrix_Space_GetVariable(obj=obj, fevar=fevar, xij=xij)
  END SELECT

CASE (Time)

  SELECT CASE (obj%returnType)
  CASE (Scalar)
    CALL Scalar_Time_GetVariable(obj=obj, fevar=fevar, timeVec=timeVec)
  CASE (Vector)
    CALL Vector_Time_GetVariable(obj=obj, fevar=fevar, timeVec=timeVec)
  CASE (Matrix)
    CALL Matrix_Time_GetVariable(obj=obj, fevar=fevar, timeVec=timeVec)
  END SELECT

CASE (SpaceTime)

  SELECT CASE (obj%returnType)
  CASE (Scalar)
    CALL Scalar_SpaceTime_GetVariable(obj=obj, fevar=fevar, xij=xij,  &
      & timeVec=timeVec)
  CASE (Vector)
    CALL Vector_SpaceTime_GetVariable(obj=obj, fevar=fevar, xij=xij,  &
      & timeVec=timeVec)
  CASE (Matrix)
    CALL Matrix_SpaceTime_GetVariable(obj=obj, fevar=fevar, xij=xij,  &
      & timeVec=timeVec)
  END SELECT

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Scalar_Constant_GetVariable(obj, fevar)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(FEVariable_), INTENT(INOUT) :: fevar

  ! internal variable
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

  isxij = PRESENT(xij)
  IF (isxij) THEN
    tsize = SIZE(xij, 2)
    CALL Reallocate(val, tsize)
    DO ii = 1, tsize
      CALL obj%Get(val=val(ii), args=xij(:, ii))
    END DO
    fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableSpace)
    IF (ALLOCATED(val)) DEALLOCATE (val)
    RETURN
  END IF

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTENRAL ERROR] :: xij should be present.')

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

  istimevec = PRESENT(timeVec)
  IF (istimevec) THEN
    tsize = SIZE(timeVec)
    CALL Reallocate(val, tsize)
    DO ii = 1, tsize
      CALL obj%Get(val=val(ii), args=timeVec(ii:ii))
    END DO
    fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableTime)
    IF (ALLOCATED(val)) DEALLOCATE (val)
    RETURN
  END IF

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTENRAL ERROR] :: timeVec should be present.')

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
  LOGICAL(LGT) :: isxij, istimevec
  INTEGER(I4B) :: ii, tspace, ttime, jj, nsd

  isxij = PRESENT(xij)
  istimevec = PRESENT(timeVec)

  IF (isxij .AND. istimevec) THEN
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
    RETURN
  END IF

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTENRAL ERROR] :: xij and timeVec should be present.')

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

  isxij = PRESENT(xij)
  IF (isxij) THEN
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
    RETURN
  END IF

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTENRAL ERROR] :: xij should be present.')

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

  istimevec = PRESENT(timeVec)
  IF (istimevec) THEN
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
    RETURN
  END IF

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTENRAL ERROR] :: timeVec should be present.')

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

  isxij = PRESENT(xij)
  IF (isxij) THEN
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
    RETURN
  END IF

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTENRAL ERROR] :: xij and timeVec should be present.')
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

  isxij = PRESENT(xij)
  IF (isxij) THEN
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
    RETURN
  END IF

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTENRAL ERROR] :: xij should be present.')

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

  istimevec = PRESENT(timeVec)
  IF (istimevec) THEN
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
    RETURN
  END IF

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTENRAL ERROR] :: timeVec should be present.')

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
  LOGICAL(LGT) :: isxij, istimevec
  INTEGER(I4B) :: ii, jj, dim1, dim2, dim3, dim4

  isxij = PRESENT(xij)
  istimevec = PRESENT(timeVec)
  IF (isxij .AND. istimevec) THEN
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
    RETURN
  END IF

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTENRAL ERROR] :: xij should be present.')

END SUBROUTINE Matrix_SpaceTime_GetVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
