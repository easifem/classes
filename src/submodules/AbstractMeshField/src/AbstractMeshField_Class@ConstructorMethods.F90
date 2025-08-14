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
USE FPL_Method, ONLY: Set, GetValue, CheckEssentialParam
USE ReallocateUtility, ONLY: Reallocate
USE SafeSizeUtility, ONLY: SafeSize
USE Display_Method, ONLY: ToString
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractMeshFieldParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetAbstractMeshFieldParam()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Set(obj=param, prefix=prefix, key="name", VALUE=name, dataType=name)
CALL Set(obj=param, prefix=prefix, key="fieldType", VALUE=fieldType, &
         dataType=fieldType)
CALL Set(obj=param, prefix=prefix, key="engine", VALUE=engine, &
         dataType=engine)
CALL Set(obj=param, prefix=prefix, key="defineOn", VALUE=defineOn, &
         dataType=defineOn)
CALL Set(obj=param, prefix=prefix, key="varType", VALUE=varType, &
         dataType=varType)
CALL Set(obj=param, prefix=prefix, key="rank", VALUE=rank, dataType=rank)
CALL Set(obj=param, prefix=prefix, key="s", VALUE=s, dataType=s)

tsize = SIZE(s)
CALL Set(obj=param, prefix=prefix, key="totalShape", VALUE=tsize, &
         dataType=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetAbstractMeshFieldParam

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL CheckEssentialParam(obj=param, keys=AbstractMeshFieldEssential, &
                         prefix=obj%GetPrefix(), myName=myName, &
                         modName=modName)
!note: CheckEssentialParam param is defined in easifemClasses FPL_Method

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .FALSE.
obj%fieldType = typefield%normal
obj%name = ""
obj%engine = ""
obj%tSize = 0
obj%defineOn = 0
obj%varType = 0
obj%rank = 0
obj%totalShape = 0
IF (ALLOCATED(obj%val)) DEALLOCATE (obj%val)
IF (ALLOCATED(obj%indxVal)) DEALLOCATE (obj%indxVal)
IF (ALLOCATED(obj%ss)) DEALLOCATE (obj%ss)
IF (ALLOCATED(obj%indxShape)) DEALLOCATE (obj%indxShape)
obj%mesh => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

TYPE(String) :: dsetname
INTEGER(I4B) :: ierr, nrow, s(MAX_RANK_FEVARIABLE)
CHARACTER(:), ALLOCATABLE :: prefix
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
CALL obj%CheckEssentialParam(param)
obj%isInit = .TRUE.
prefix = obj%GetPrefix()

! fieldType
obj%fieldType = typefield%normal
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

CALL GetValue(obj=param, prefix=prefix, key="totalShape", &
              VALUE=obj%totalShape)

#ifdef DEBUG_VER
isok = obj%totalShape .LE. SIZE(s)
CALL AssertError1(isok, myName, &
                  'The size of s in param is more than the size of s in obj.')
#endif

dsetname = TRIM(prefix)//"/s"
ierr = param%Get(key=dsetname%chars(), VALUE=s(1:obj%totalShape))

! tSize
IF (obj%fieldType .EQ. typefield%constant) THEN
  obj%tSize = 1
ELSE
  obj%tSize = mesh%GetTotalElements()
END IF

! val
CALL Reallocate(obj%indxVal, obj%tSize + 1)
obj%indxVal = 1

ierr = PRODUCT(s(1:nrow))
CALL Reallocate(obj%val, ierr * obj%tSize)

! indxShape
CALL Reallocate(obj%indxShape, obj%tSize + 1)
obj%indxShape = 1

CALL Reallocate(obj%ss, obj%totalShape * obj%tSize)

! mesh
obj%mesh => mesh

prefix = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif

INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = obj2%isInit
obj%fieldType = obj2%fieldType
obj%name = obj2%name
obj%engine = obj2%engine
obj%tSize = obj2%tSize
obj%defineOn = obj2%defineOn
obj%varType = obj2%varType
obj%rank = obj2%rank
obj%mesh => obj2%mesh

tsize = SafeSize(obj2%val)
CALL Reallocate(obj%val, tsize)
DO CONCURRENT(ii=1:tsize)
  obj%val(ii) = obj2%val(ii)
END DO

tsize = SafeSize(obj2%indxVal)
CALL Reallocate(obj%indxVal, tsize)
DO CONCURRENT(ii=1:tsize)
  obj%indxVal(ii) = obj2%indxVal(ii)
END DO

obj%totalShape = obj2%totalShape

CALL Reallocate(obj%ss, obj%totalShape)
DO CONCURRENT(ii=1:obj%totalShape)
  obj%ss(ii) = obj2%ss(ii)
END DO

tsize = SafeSize(obj2%indxShape)
CALL Reallocate(obj%indxShape, tsize)
DO CONCURRENT(ii=1:tsize)
  obj%indxShape(ii) = obj2%indxShape(ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                           Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[WIP ERROR] :: This routine should be implemented by child class.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate5()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
obj%fieldType = fieldType
obj%name = name
obj%engine = engine
obj%defineOn = defineOn
obj%varType = varType
obj%rank = rank
obj%totalShape = SIZE(s)

! tSize
IF (obj%fieldType .EQ. typefield%constant) THEN
  obj%tSize = 1
ELSE
  obj%tSize = mesh%GetTotalElements()
END IF

! indxVal
CALL Reallocate(obj%indxVal, obj%tSize + 1)
obj%indxVal = 1

! val
tsize = PRODUCT(s(1:obj%totalShape))
CALL Reallocate(obj%val, tsize * obj%tSize)

! indxShape
CALL Reallocate(obj%indxShape, obj%tSize + 1)
obj%indxShape = 1

! ss
CALL Reallocate(obj%ss, obj%totalShape * obj%tSize)

! mesh
obj%mesh => mesh

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate5

!----------------------------------------------------------------------------
!                                                           Iniitate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
LOGICAL(LGT) :: isok
#endif

CLASS(UserFunction_), POINTER :: func

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = material%IsMaterialPresent(name)
CALL AssertError1(isok, myname, 'Material name = '//name//" not found.")
#endif

func => NULL()
func => material%GetMaterialPointer(name)

#ifdef DEBUG_VER
isok = ASSOCIATED(func)
CALL AssertError1(isok, myname, 'Material pointer not found.')
#endif

CALL obj%Initiate(name=name, func=func, engine=engine, nnt=nnt, mesh=mesh)

NULLIFY (func)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"
#include "./include/GetTotalRow.F90"

END SUBMODULE ConstructorMethods
