! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(MeshField_Class) GetMethods
USE Display_Method, ONLY: ToString
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPrefix"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = myprefix

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
CASE (TypeFieldOpt%constant)
  tsize = 1
  s(1:tsize) = 1

CASE (TypeFieldOpt%time)

#ifdef DEBUG_VER
  isok = PRESENT(nnt)
  CALL AssertError1(isok, myName, &
                    'nnt must be present when varType is time')
#endif

  tsize = 1
  s(1:tsize) = nnt

CASE (TypeFieldOpt%space)

#ifdef DEBUG_VER
  isok = PRESENT(nns)
  CALL AssertError1(isok, myName, &
                    'nns must be present when varType is space')
#endif
  tsize = 1
  s(1:tsize) = nns

CASE (TypeFieldOpt%spaceTime)

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
CASE (TypeFieldOpt%constant)
  tsize = 1
  s(1) = spaceCompo

CASE (TypeFieldOpt%time)

#ifdef DEBUG_VER
  isok = PRESENT(nnt)
  CALL AssertError1(isok, myName, &
                    'nnt must be present when varType is time')
#endif

  tsize = 2
  s(1) = spaceCompo
  s(2) = nnt

CASE (TypeFieldOpt%space)

#ifdef DEBUG_VER
  isok = PRESENT(nns)
  CALL AssertError1(isok, myName, &
                    'nns must be present when varType is space')
#endif
  tsize = 2
  s(1) = spaceCompo
  s(2) = nns

CASE (TypeFieldOpt%spaceTime)

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
CASE (TypeFieldOpt%constant)
  tsize = 2
  s(1) = dim1
  s(2) = dim2

CASE (TypeFieldOpt%time)

#ifdef DEBUG_VER
  isok = PRESENT(nnt)
  CALL AssertError1(isok, myName, &
                    'nnt must be present when varType is time')
#endif

  tsize = 3
  s(1) = dim1
  s(2) = dim2
  s(3) = nnt

CASE (TypeFieldOpt%space)

#ifdef DEBUG_VER
  isok = PRESENT(nns)
  CALL AssertError1(isok, myName, &
                    'nns must be present when varType is space')
#endif
  tsize = 3
  s(1) = dim1
  s(2) = dim2
  s(3) = nns

CASE (TypeFieldOpt%spaceTime)

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
!                                                    MeshFieldGetShapeAndSize
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshFieldGetShapeAndSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "MeshFieldGetShapeAndSize"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (rank)

! ScalarMeshField
CASE (TypeFieldOpt%scalar)

  CALL ScalarMeshFieldGetShapeAndSize(varType=varType, s=s, tsize=tsize, &
                                      nns=nns, nnt=nnt)

CASE (TypeFieldOpt%vector)

  CALL VectorMeshFieldGetShapeAndSize(varType=varType, s=s, tsize=tsize, &
                                      spaceCompo=spaceCompo, nns=nns, nnt=nnt)

CASE (TypeFieldOpt%matrix)

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
END PROCEDURE MeshFieldGetShapeAndSize

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
