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
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractMeshField_Class@GetMethods"
#endif

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

  CASE DEFAULT

#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
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

  CASE DEFAULT

#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
                      "No case found for vartype="// &
                      ToString(obj%vartype))
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

  CASE DEFAULT

#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
                      "No case found for vartype="// &
                      ToString(obj%vartype))
#endif

  END SELECT

CASE DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "No case found for rank="// &
                    ToString(obj%rank))
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
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get_()"
#endif
INTEGER(I4B) :: iel, ii, a, b
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! fevar%defineOn = obj%defineOn
#ifdef DEBUG_VER
CALL AssertError2(fevar%defineOn, obj%defineOn, myName, &
                  "a=fevar%defineOn, b=obj%defineOn.")
#endif

! #ifdef DEBUG_VER
! CALL AssertError2(fevar%varType, obj%varType, myName, &
!                   "a=fevar%varType, b=obj%varType.")
! #endif
fevar%varType = obj%varType

! fevar%rank = obj%rank
#ifdef DEBUG_VER
CALL AssertError2(fevar%rank, obj%rank, myName, &
                  "a=fevar%rank, b=obj%rank")
#endif

isok = obj%fieldType .EQ. TypeField%constant
IF (isok) THEN
  iel = 1
ELSE
  iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                    islocal=islocal)
END IF

a = obj%indxVal(iel)
b = obj%indxVal(iel + 1)

fevar%len = b - a

#ifdef DEBUG_VER
CALL AssertError3(fevar%len, fevar%capacity, myName, &
                  "a=fevar%len, b=fevar%capacity")
#endif

! CALL Reallocate(fevar%val, fevar%capacity)

DO ii = a, b - 1
  fevar%val(ii - a + 1) = obj%val(ii)
END DO

a = obj%indxShape(iel)
b = obj%indxShape(iel + 1) - 1
DO ii = a, b
  fevar%s(ii - a + 1) = obj%ss(ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get_

!----------------------------------------------------------------------------
!                                              ScalarMeshFieldGetShapeAndSize
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarMeshFieldGetShapeAndSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarMeshFieldGetShapeAndSize()"
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

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
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

CASE DEFAULT
#ifdef DEBUG_VER
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

CASE DEFAULT

#ifdef DEBUG_VER
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

  CALL VectorMeshFieldGetShapeAndSize( &
    varType=varType, s=s, tsize=tsize, spaceCompo=spaceCompo, nns=nns, &
    nnt=nnt)

CASE (typefield%matrix)

  CALL TensorMeshFieldGetShapeAndSize(varType=varType, s=s, tsize=tsize, &
                                      dim1=dim1, dim2=dim2, nns=nns, nnt=nnt)

CASE DEFAULT

#ifdef DEBUG_VER
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
!                                                             GetMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMeshPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => obj%mesh

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMeshPointer

!----------------------------------------------------------------------------
!                                                                   GetMaxNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxNNE
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxNNE()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = (obj%varType .EQ. fevaropt%constant) &
       .OR. (obj%varType .EQ. fevaropt%time)

IF (isok) THEN
  ans = 1

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

SELECT CASE (obj%rank)
CASE (fevaropt%scalar)

  SELECT CASE (obj%varType)
  CASE (fevaropt%space, fevaropt%spaceTime)
    ans = obj%maxShape(1)
  CASE DEFAULT
#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, "no case found for obj%varType")
#endif
  END SELECT

CASE (fevaropt%vector)

  SELECT CASE (obj%varType)
  CASE (fevaropt%space, fevaropt%spaceTime)
    ans = obj%maxShape(2)
  CASE DEFAULT
#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, "no case found for obj%varType")
#endif
  END SELECT

CASE (fevaropt%matrix)

  SELECT CASE (obj%varType)
  CASE (fevaropt%space, fevaropt%spaceTime)
    ans = obj%maxShape(3)
  CASE DEFAULT
#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, "no case found for obj%varType")
#endif
  END SELECT

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "no case found for obj%rank")
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxNNE

!----------------------------------------------------------------------------
!                                                        GetSpaceVectorField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpaceVectorField_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSpaceVectorField_()"
#endif
INTEGER(I4B) :: iel, ii, a, b, s(fevaropt%maxRank)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = fevaropt%vector .EQ. obj%rank
CALL AssertError1(isok, myName, &
                  "obj%rank is not vector.")
#endif

#ifdef DEBUG_VER
isok = fevaropt%space .EQ. obj%varType
CALL AssertError1(isok, myName, &
                  "obj%varType is not space.")
#endif

isok = obj%fieldType .EQ. TypeField%constant
IF (isok) THEN
  iel = 1
ELSE
  iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                    islocal=islocal)
END IF

a = obj%indxShape(iel)
b = obj%indxShape(iel + 1) - 1

#ifdef DEBUG_VER
isok = (b - a + 1 .EQ. math%two_i)
CALL AssertError1(isok, myName, &
                  "error in getting shape of data")
#endif

DO ii = a, b
  s(ii - a + 1) = obj%ss(ii)
END DO

nrow = s(1)
ncol = s(2)

a = obj%indxVal(iel)
b = obj%indxVal(iel + 1) - 1

ans(1:nrow, 1:ncol) = RESHAPE(obj%val(a:b), s(1:2))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetSpaceVectorField_

!----------------------------------------------------------------------------
!                                                        GetSpaceScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpaceScalarField_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSpaceScalarField_()"
#endif
INTEGER(I4B) :: iel, a, b
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = fevaropt%scalar .EQ. obj%rank
CALL AssertError1(isok, myName, &
                  "obj%rank is not Scalar.")
#endif

#ifdef DEBUG_VER
isok = fevaropt%space .EQ. obj%varType
CALL AssertError1(isok, myName, &
                  "obj%varType is not space.")
#endif

isok = obj%fieldType .EQ. TypeField%constant
IF (isok) THEN
  iel = 1
ELSE
  iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                    islocal=islocal)
END IF

#ifdef DEBUG_VER
a = obj%indxShape(iel)
b = obj%indxShape(iel + 1) - 1

isok = (b - a + 1 .EQ. math%one_i)
CALL AssertError1(isok, myName, &
                  "error in getting shape of data")
#endif

a = obj%indxVal(iel)
b = obj%indxVal(iel + 1) - 1
tsize = b - a + 1

ans(1:tsize) = obj%val(a:b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetSpaceScalarField_

!----------------------------------------------------------------------------
!                                                     GetConstantScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConstantScalarField_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConstantScalarField_()"
INTEGER(I4B) :: b, tsize
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: iel, a

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = fevaropt%scalar .EQ. obj%rank
CALL AssertError1(isok, myName, &
                  "obj%rank is not Scalar.")
#endif

#ifdef DEBUG_VER
isok = fevaropt%constant .EQ. obj%varType
CALL AssertError1(isok, myName, &
                  "obj%varType is not Constant.")
#endif

isok = obj%fieldType .EQ. TypeField%constant
IF (isok) THEN
  iel = 1
ELSE
  iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                    islocal=islocal)
END IF

#ifdef DEBUG_VER
a = obj%indxShape(iel)
b = obj%indxShape(iel + 1) - 1

isok = (b - a + 1 .EQ. math%one_i)
CALL AssertError1(isok, myName, &
                  "error in getting shape of data")
#endif

a = obj%indxVal(iel)

#ifdef DEBUG_VER
b = obj%indxVal(iel + 1) - 1
tsize = b - a + 1
CALL AssertError2(tsize, math%one_i, myName, &
                  "error in getting size of data, a=tsize, b=1")
#endif

ans = obj%val(a)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetConstantScalarField_

!----------------------------------------------------------------------------
!                                                                    GetRank
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRank
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetRank()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%rank

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetRank

!----------------------------------------------------------------------------
!                                                           GetTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalElements()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%tSize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalElements

!----------------------------------------------------------------------------
!                                                                 GetVarType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVarType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetVarType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%varType

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetVarType

!----------------------------------------------------------------------------
!                                                              GetTotalShape
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalShape
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalShape()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%totalShape

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalShape

!----------------------------------------------------------------------------
!                                                              GetShape
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetShape
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetShape()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: iel, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
#endif

isok = obj%fieldType .EQ. TypeField%constant
IF (isok) THEN
  iel = 1
ELSE
  iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                    islocal=islocal)
END IF

tsize = 0
DO ii = obj%indxShape(iel), obj%indxShape(iel + 1) - 1
  tsize = tsize + 1
  ans(tsize) = obj%ss(ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetShape

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
