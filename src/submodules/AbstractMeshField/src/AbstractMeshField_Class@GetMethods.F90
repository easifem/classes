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

SUBMODULE(AbstractMeshField_Class) GetMethods
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Shape
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Shape()"
#endif

INTEGER(I4B) :: iel

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

iel = obj%mesh%GetLocalElemNumber(globalelement=globalElement, &
                                  islocal=islocal)

SELECT CASE (obj%rank)

CASE (fevaropt%scalar)

  SELECT CASE (obj%vartype)

  CASE (typefield%constant)

    ALLOCATE (ans(1))
    ans(1) = obj%ss(obj%indxShape(iel))

  CASE (typefield%space, typefield%Time)

    ALLOCATE (ans(1))
    ans(1) = obj%ss(obj%indxShape(iel))

  CASE (typefield%spaceTime)

    ALLOCATE (ans(2))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
                      "No case found for vartype="//ToString(obj%vartype))
#endif

  END SELECT

CASE (fevaropt%vector)

  SELECT CASE (obj%vartype)

  CASE (typefield%constant)

    ALLOCATE (ans(1))
    ans(1) = obj%ss(obj%indxShape(iel))

  CASE (typefield%space, typefield%time)

    ALLOCATE (ans(2))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)

  CASE (typefield%spaceTime)

    ALLOCATE (ans(3))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)
    ans(3) = obj%ss(obj%indxShape(iel) + 2)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
                      "No case found for vartype="//ToString(obj%vartype))
#endif

  END SELECT

CASE (fevaropt%matrix)

  SELECT CASE (obj%vartype)

  CASE (typefield%constant)

    ALLOCATE (ans(2))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)

  CASE (typefield%space, typefield%time)

    ALLOCATE (ans(3))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)
    ans(3) = obj%ss(obj%indxShape(iel) + 2)

  CASE (typefield%spaceTime)

    ALLOCATE (ans(4))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)
    ans(3) = obj%ss(obj%indxShape(iel) + 2)
    ans(4) = obj%ss(obj%indxShape(iel) + 3)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
                      "No case found for vartype="//ToString(obj%vartype))
#endif

  END SELECT

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    "No case found for rank="//ToString(obj%rank))
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Shape

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get()"
#endif
INTEGER(I4B) :: iel, ii, a, b

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%fieldType .EQ. TypeField%constant) THEN
  iel = 1
ELSE
  iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                    islocal=islocal)
END IF

a = obj%indxVal(iel)
b = obj%indxVal(iel + 1)

fevar%len = b - a
fevar%capacity = MAX(fevar%len, fevar%capacity)

CALL Reallocate(fevar%val, fevar%capacity)

DO ii = a, b - 1
  fevar%val(ii - a + 1) = obj%val(ii)
END DO

a = obj%indxShape(iel)
b = obj%indxShape(iel + 1) - 1
DO ii = a, b
  fevar%s(ii - a + 1) = obj%ss(ii)
END DO

fevar%defineOn = obj%defineOn
fevar%varType = obj%varType
fevar%rank = obj%rank

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get

!----------------------------------------------------------------------------
!                                                                  GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPrefix()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = ""

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                              ScalarMeshFieldGetShapeAndSize
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarMeshFieldGetShapeAndSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarMeshFieldGetShapeAndSize"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (varType)
CASE (typefield%constant)
  tsize = 1
  s(1:tsize) = 1

CASE (typefield%time)

#ifdef DEBUG_VER
  isok = PRESENT(nnt)
  CALL AssertError1(isok, myName, &
                    'nnt must be present when varType is time')
#endif

  tsize = 1
  s(1:tsize) = nnt

CASE (typefield%space)

#ifdef DEBUG_VER
  isok = PRESENT(nns)
  CALL AssertError1(isok, myName, &
                    'nns must be present when varType is space')
#endif
  tsize = 1
  s(1:tsize) = nns

CASE (typefield%spaceTime)

#ifdef DEBUG_VER
  isok = PRESENT(nns)
  CALL AssertError1(isok, myName, &
                    'nns must be present when varType is spaceTime')

  isok = PRESENT(nnt)
  CALL AssertError1(isok, myName, &
                    'nntmust be present when varType is spaceTime')
#endif

  tsize = 2
  s(1) = nns
  s(2) = nnt

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for varType: '//ToString(varType))
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ScalarMeshFieldGetShapeAndSize

!----------------------------------------------------------------------------
!                                              VectorMeshFieldGetShapeAndSize
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorMeshFieldGetShapeAndSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "VectorMeshFieldGetShapeAndSize"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = PRESENT(spaceCompo)
CALL AssertError1(isok, myName, &
                  'spaceCompo must be present for vector mesh field')
#endif

SELECT CASE (varType)
CASE (typefield%constant)
  tsize = 1
  s(1) = spaceCompo

CASE (typefield%time)

#ifdef DEBUG_VER
  isok = PRESENT(nnt)
  CALL AssertError1(isok, myName, &
                    'nnt must be present when varType is time')
#endif

  tsize = 2
  s(1) = spaceCompo
  s(2) = nnt

CASE (typefield%space)

#ifdef DEBUG_VER
  isok = PRESENT(nns)
  CALL AssertError1(isok, myName, &
                    'nns must be present when varType is space')
#endif
  tsize = 2
  s(1) = spaceCompo
  s(2) = nns

CASE (typefield%spaceTime)

#ifdef DEBUG_VER
  isok = PRESENT(nns)
  CALL AssertError1(isok, myName, &
                    'nns must be present when varType is spaceTime')

  isok = PRESENT(nnt)
  CALL AssertError1(isok, myName, &
                    'nntmust be present when varType is spaceTime')
#endif

  tsize = 3
  s(1) = spaceCompo
  s(2) = nns
  s(3) = nnt

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for varType: '//ToString(varType))
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VectorMeshFieldGetShapeAndSize

!----------------------------------------------------------------------------
!                                              TensorMeshFieldGetShapeAndSize
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorMeshFieldGetShapeAndSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "TensorMeshFieldGetShapeAndSize"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = PRESENT(dim1)
CALL AssertError1(isok, myName, &
                  'dim1 must be present for tensor mesh field')

isok = PRESENT(dim2)
CALL AssertError1(isok, myName, &
                  'dim2 must be present for tensor mesh field')
#endif

SELECT CASE (varType)
CASE (typefield%constant)
  tsize = 2
  s(1) = dim1
  s(2) = dim2

CASE (typefield%time)

#ifdef DEBUG_VER
  isok = PRESENT(nnt)
  CALL AssertError1(isok, myName, &
                    'nnt must be present when varType is time')
#endif

  tsize = 3
  s(1) = dim1
  s(2) = dim2
  s(3) = nnt

CASE (typefield%space)

#ifdef DEBUG_VER
  isok = PRESENT(nns)
  CALL AssertError1(isok, myName, &
                    'nns must be present when varType is space')
#endif
  tsize = 3
  s(1) = dim1
  s(2) = dim2
  s(3) = nns

CASE (typefield%spaceTime)

#ifdef DEBUG_VER
  isok = PRESENT(nns)
  CALL AssertError1(isok, myName, &
                    'nns must be present when varType is spaceTime')

  isok = PRESENT(nnt)
  CALL AssertError1(isok, myName, &
                    'nntmust be present when varType is spaceTime')
#endif

  tsize = 4
  s(1) = dim1
  s(2) = dim2
  s(3) = nns
  s(4) = nnt

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for varType: '//ToString(varType))
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE TensorMeshFieldGetShapeAndSize

!----------------------------------------------------------------------------
!                                            AbstractMeshFieldGetShapeAndSize
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractMeshFieldGetShapeAndSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "AbstractMeshFieldGetShapeAndSize"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (rank)

! ScalarMeshField
CASE (typefield%scalar)

  CALL ScalarMeshFieldGetShapeAndSize(varType=varType, s=s, tsize=tsize, &
                                      nns=nns, nnt=nnt)

CASE (typefield%vector)

  CALL VectorMeshFieldGetShapeAndSize(varType=varType, s=s, tsize=tsize, &
                                      spaceCompo=spaceCompo, nns=nns, nnt=nnt)

CASE (typefield%matrix)

  CALL TensorMeshFieldGetShapeAndSize(varType=varType, s=s, tsize=tsize, &
                                      dim1=dim1, dim2=dim2, nns=nns, nnt=nnt)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for rank: '//ToString(rank))
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE AbstractMeshFieldGetShapeAndSize

!----------------------------------------------------------------------------
!                                                               IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsInitiated()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%isInit

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
