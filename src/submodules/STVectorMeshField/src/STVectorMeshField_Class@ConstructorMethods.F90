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

SUBMODULE(STVectorMeshField_Class) ConstructorMethods

USE GlobalData, ONLY: Constant, SpaceTime, Vector, Nodal

USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt

USE AbstractMeshField_Class, ONLY: SetAbstractMeshFieldParam

USE Display_Method, ONLY: ToString

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetSTVectorMeshFieldParam
INTEGER(I4B) :: s(3), n

IF (fieldType .EQ. TypeField%constant) THEN
  s = spaceCompo; n = 1
ELSE
  s = [spaceCompo, nns, nnt]; n = 3
END IF

CALL SetAbstractMeshFieldParam(param=param, prefix=myprefix, &
             name=name, fieldType=fieldType, varType=varType, engine=engine, &
                               defineOn=defineOn, rank=Vector, s=s(1:n))

END PROCEDURE SetSTVectorMeshFieldParam

!----------------------------------------------------------------------------
!                                                                 Initaite
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: returnType, argType, nns, varType, fieldType, numReturns
TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

nns = mesh%GetMaxNNE()

returnType = func%GetReturnType()
isok = returnType .EQ. Vector
CALL AssertError1(isok, myName, 'returnType should be Vector.')

argType = func%GetArgType()
numReturns = func%GetNumReturns()
fieldType = TypeField%normal
varType = argType
IF (argType .EQ. Constant) THEN
  fieldType = TypeField%constant
  varType = Constant
END IF

isok = PRESENT(nnt)
CALL AssertError1(isok, myName, &
                  'nnt should be present when varType is Time or SpaceTime.')

CALL param%Initiate()

CALL SetSTVectorMeshFieldParam(param=param, name=name, &
                        fieldType=fieldType, varType=varType, engine=engine, &
                      defineOn=Nodal, spaceCompo=numReturns, nns=nns, nnt=nnt)

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
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
