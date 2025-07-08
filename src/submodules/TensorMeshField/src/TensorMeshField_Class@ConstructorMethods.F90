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

SUBMODULE(TensorMeshField_Class) ConstructorMethods
USE GlobalData, ONLY: Constant, SpaceTime, Matrix, Nodal

USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt

USE AbstractMeshField_Class, ONLY: SetAbstractMeshFieldParam

USE Display_Method, ONLY: ToString

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetTensorMeshFieldParam
INTEGER(I4B) :: s(3), n
CHARACTER(*), PARAMETER :: myName = "SetTensorMeshFieldParam()"
LOGICAL(LGT) :: isok

isok = varType .NE. SpaceTime
CALL AssertError1(isok, myName, &
                  'For ScalarMeshField varType cannot be SpaceTime.'// &
                 ' In this situation you should use STTensorMeshField_Class.')

IF (fieldType .EQ. TypeField%constant) THEN
  n = 2; s(1:n) = [dim1, dim2]
ELSE
  n = 3; s(1:n) = [dim1, dim2, nns]
END IF

CALL SetAbstractMeshFieldParam(param=param, prefix=myPrefix, &
                           name=name, fieldType=fieldType, varType=constant, &
                      engine=engine, defineOn=defineOn, rank=Matrix, s=s(1:n))

END PROCEDURE SetTensorMeshFieldParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: returnType, argType, nns, varType, fieldType, &
                numReturns, dims(2)
TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

nns = mesh%GetMaxNNE()

returnType = func%GetReturnType()

isok = returnType .EQ. Matrix
CALL AssertError1(isok, myName, &
                  'Return type should be Matrix.')

argType = func%GetArgType()
numReturns = func%GetNumReturns()
dims = func%GetReturnShape()
fieldType = TypeField%normal
varType = argType
IF (argType .EQ. Constant) THEN
  fieldType = TypeField%constant
  varType = Constant
END IF

CALL param%Initiate()

CALL SetTensorMeshFieldParam(param=param, name=name, &
                        fieldType=fieldType, varType=varType, engine=engine, &
                          defineOn=Nodal, dim1=dims(1), dim2=dims(2), nns=nns)

CALL obj%Initiate(param=param, mesh=mesh)

CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    CALL obj(ii)%DEALLOCATE()
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE obj_Deallocate_Vector

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Ptr_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE obj_Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                                GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
