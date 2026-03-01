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
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
USE SafeSizeUtility, ONLY: SafeSize
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "AbstractMeshField_Class@ConstructorMethods"
#endif

CONTAINS

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

obj%isInit = math%no
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
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
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

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                   Iniitate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
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
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
#endif

INTEGER(I4B) :: rank, nns, varType, fieldType, &
                spaceCompo, dims(2), s(4), tsize, rank

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nns = mesh%GetMaxNNE()
rank = func%GetReturnType()
varType = func%GetArgType()
spaceCompo = func%GetNumReturns()
dims = func%GetReturnShape()

fieldType = typefield%normal
! IF (varType .EQ. typefield%constant) fieldType = varType
! I have commented above line as it causes issue when we have multiple
! domains with constant varType

CALL AbstractMeshFieldGetShapeAndSize( &
  rank=rank, varType=varType, s=s, tsize=tsize, nns=nns, &
  spaceCompo=spaceCompo, dim1=dims(1), dim2=dims(2), nnt=nnt)

CALL obj%Initiate( &
  name=name, fieldType=fieldType, varType=varType, &
  engine=engine, defineOn=typefield%nodal, &
  rank=rank, s=s(1:tsize), mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = math%yes
obj%fieldType = fieldType
obj%name = name
obj%engine = engine
obj%defineOn = defineOn
obj%varType = varType
obj%rank = rank
obj%totalShape = SIZE(s)

#ifdef DEBUG_VER
CALL AssertError3(obj%totalShape, fevaropt%maxRank, myName, &
                  "a=obj%totalShape, b=fevaropt%maxRank")
#endif

obj%maxShape(1:obj%totalShape) = s(1:obj%totalShape)

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
END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate5()"
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

CALL obj%Initiate(name=name, func=func, engine=engine, nnt=nnt, &
                  quadField=quadField)

NULLIFY (func)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate5

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate6()"
#endif

INTEGER(I4B) :: rank, nns, varType, fieldType, &
                spaceCompo, dims(2), s(4), tsize, rank
CLASS(AbstractMesh_), POINTER :: mesh

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

mesh => quadField%GetMeshPointer()

! nns should be maximum number of quadrature points in quadField
nns = quadField%GetMaxNNE()
rank = func%GetReturnType()
varType = func%GetArgType()
spaceCompo = func%GetNumReturns()
dims = func%GetReturnShape()

fieldType = typefield%normal

CALL AbstractMeshFieldGetShapeAndSize( &
  rank=rank, varType=varType, s=s, tsize=tsize, nns=nns, &
  spaceCompo=spaceCompo, dim1=dims(1), dim2=dims(2), nnt=nnt)

CALL obj%Initiate( &
  name=name, fieldType=fieldType, varType=varType, &
  engine=engine, defineOn=typefield%quadrature, &
  rank=rank, s=s(1:tsize), mesh=mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate6

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"
#include "./include/GetTotalRow.F90"

END SUBMODULE ConstructorMethods
