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

SUBMODULE(AbstractMeshField_Class) ConstructorMethods
USE GlobalData, ONLY: Constant, Space, Time, SpaceTime, &
                      Scalar, Vector, Matrix

USE FPL_Method, ONLY: Set, GetValue, CheckEssentialParam

USE ReallocateUtility, ONLY: Reallocate

! USE ScalarMeshField_Class, ONLY: SetScalarMeshFieldParam
! USE VectorMeshField_Class, ONLY: SetVectorMeshFieldParam
! USE TensorMeshField_Class, ONLY: SetTensorMeshFieldParam
! USE STScalarMeshField_Class, ONLY: SetSTScalarMeshFieldParam
! USE STVectorMeshField_Class, ONLY: SetSTVectorMeshFieldParam
! USE STTensorMeshField_Class, ONLY: SetSTTensorMeshFieldParam

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractMeshFieldParam
INTEGER(I4B) :: tsize

CALL Set(obj=param, prefix=prefix, key="name", VALUE=name, dataType=name)
CALL Set(obj=param, prefix=prefix, key="fieldType", VALUE=fieldType, &
         dataType=fieldType)
CALL Set(obj=param, prefix=prefix, key="engine", VALUE=engine, dataType=engine)
CALL Set(obj=param, prefix=prefix, key="defineOn", VALUE=defineOn, &
         dataType=defineOn)
CALL Set(obj=param, prefix=prefix, key="varType", VALUE=varType, &
         dataType=varType)
CALL Set(obj=param, prefix=prefix, key="rank", VALUE=rank, dataType=rank)
CALL Set(obj=param, prefix=prefix, key="s", VALUE=s, dataType=s)

tsize = SIZE(s)
CALL Set(obj=param, prefix=prefix, key="totalShape", VALUE=tsize, &
         dataType=tsize)

END PROCEDURE SetAbstractMeshFieldParam

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"

CALL CheckEssentialParam(obj=param, keys=AbstractMeshFieldEssential, &
                       prefix=obj%GetPrefix(), myName=myName, modName=modName)
!NOTE: CheckEssentialParam param is defined in easifemClasses FPL_Method
END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                           GetTotalRow
!----------------------------------------------------------------------------

FUNCTION GetTotalRow(rank, varType) RESULT(nrow)
  INTEGER(I4B), INTENT(IN) :: rank, varType
  INTEGER(I4B) :: nrow

  SELECT CASE (rank)

  CASE (Scalar)
    SELECT CASE (varType)
    CASE (Constant, Space, Time)
      nrow = 1
      ! one dimension, single entry
      ! one dimension, multiple entries in space
      ! one dimension, multiple entries in time
    CASE (SpaceTime)
      ! two dimensions, multiple entries in space-time
      nrow = 2
    END SELECT

  CASE (Vector)
    SELECT CASE (varType)
    CASE (Constant)
      ! one dimension, only vector components
      nrow = 1
    CASE (Space, Time)
      nrow = 2
      ! two dimension, vector components and space values
      ! two dimension, vector components and time values
    CASE (SpaceTime)
      ! two dimension, vector components, space and time values
      nrow = 3
    END SELECT

  CASE (Matrix)
    SELECT CASE (varType)
    CASE (Constant)
      ! two dimensions, matrix components
      nrow = 2
    CASE (Space, Time)
      ! three dimensions, matrix components and space values
      ! three dimensions, matrix components and time values
      nrow = 3
    CASE (SpaceTime)
      ! four dimensions, matrix components, space and time values
      nrow = 4
    END SELECT
  END SELECT
END FUNCTION GetTotalRow

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Ptr_Vector
INTEGER(I4B) :: ii, tsize
IF (ALLOCATED(obj)) THEN
  tsize = SIZE(obj)
  DO ii = 1, tsize
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE obj_Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%isInitiated = .FALSE.
obj%fieldType = TypeField%normal
obj%name = ""
obj%engine = ""
obj%tSize = 0
obj%s = 0
obj%defineOn = 0
obj%varType = 0
obj%rank = 0
IF (ALLOCATED(obj%val)) DEALLOCATE (obj%val)
obj%mesh => NULL()
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
TYPE(String) :: dSetname
INTEGER(I4B) :: ierr, nrow, totalShape
CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

CALL obj%DEALLOCATE()
CALL obj%CheckEssentialParam(param)
obj%isInitiated = .TRUE.
prefix = obj%GetPrefix()

! fieldType
obj%fieldType = TypeField%normal
CALL GetValue(obj=param, prefix=prefix, key="fieldType", VALUE=obj%fieldType)

! name
obj%name = prefix
CALL GetValue(obj=param, prefix=prefix, key="name", VALUE=obj%name)

! engine
CALL GetValue(obj=param, prefix=prefix, key="engine", VALUE=obj%engine)

! defineOn
CALL GetValue(obj=param, prefix=prefix, key="defineOn", VALUE=obj%defineOn)

! varType
CALL GetValue(obj=param, prefix=prefix, key="varType", VALUE=obj%varType)

! rank
CALL GetValue(obj=param, prefix=prefix, key="rank", VALUE=obj%rank)

nrow = GetTotalRow(rank=obj%rank, varType=obj%varType)

CALL GetValue(obj=param, prefix=prefix, key="totalShape", VALUE=totalShape)

IF (totalShape .GT. SIZE(obj%s)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: The size of s in param is '// &
                    ' more than the size of s in obj')
  RETURN
END IF

dsetname = TRIM(prefix)//"/s"
ierr = param%Get(key=dsetname%chars(), VALUE=obj%s(1:totalShape))

! tSize
IF (obj%fieldType .EQ. TypeField%constant) THEN
  obj%tSize = 1
ELSE
  obj%tSize = mesh%GetTotalElements()
END IF

! val
ierr = PRODUCT(obj%s(1:nrow))
CALL Reallocate(obj%val, ierr, obj%tSize)

! mesh
obj%mesh => mesh

prefix = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
obj%isInitiated = obj2%isInitiated
obj%fieldType = obj2%fieldType
obj%name = obj2%name
obj%engine = obj2%engine
obj%tSize = obj2%tSize
obj%s = obj2%s
obj%defineOn = obj2%defineOn
obj%varType = obj2%varType
obj%rank = obj2%rank
obj%mesh => obj2%mesh
IF (ALLOCATED(obj2%val)) obj%val = obj2%val
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                           Iniitate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
LOGICAL(LGT) :: isok
CLASS(UserFunction_), POINTER :: func

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

isok = material%IsMaterialPresent(name)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[INTERNAL ERROR] :: material name = '//name//" not found.")
  RETURN
END IF

func => NULL()
func => material%GetMaterialPointer(name)
isok = ASSOCIATED(func)
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: material pointer not found.')
  RETURN
END IF

CALL obj%Initiate(name=name, func=func, engine=engine, nnt=nnt, mesh=mesh)

NULLIFY (func)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                           Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
           '[WIP ERROR] :: This routine should be implemented by subprocess.')
END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Ptr_Vector_Scalar
INTEGER(I4B) :: ii, tsize
IF (ALLOCATED(obj)) THEN
  tsize = SIZE(obj)
  DO ii = 1, tsize
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE obj_Deallocate_Ptr_Vector_Scalar

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Ptr_Vector_Vector
INTEGER(I4B) :: ii, tsize
IF (ALLOCATED(obj)) THEN
  tsize = SIZE(obj)
  DO ii = 1, tsize
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE obj_Deallocate_Ptr_Vector_Vector

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Ptr_Vector_Tensor
INTEGER(I4B) :: ii, tsize
IF (ALLOCATED(obj)) THEN
  tsize = SIZE(obj)
  DO ii = 1, tsize
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE obj_Deallocate_Ptr_Vector_Tensor

END SUBMODULE ConstructorMethods
