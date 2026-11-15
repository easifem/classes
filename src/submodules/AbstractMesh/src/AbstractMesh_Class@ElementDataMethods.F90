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

SUBMODULE(AbstractMesh_Class) ElementDataMethods
USE AbstractMeshUtility, ONLY: InitiateElementToElements3D
USE AbstractMeshUtility, ONLY: InitiateElementToElements2D
USE AbstractMeshUtility, ONLY: InitiateElementToElements1D
USE BaseType, ONLY: TypeRefelemOpt
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: ToString
USE NodeData_Class, ONLY: TypeNode
USE NodeData_Class, ONLY: NodeData_SetNodeType
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "AbstractMesh_Class@ElementDataMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                 InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateElementToElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateElementToElements()"
#endif
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ALLOCATED(obj%elementData)
CALL AssertError1(isok, myName, "elementData not allocated.")
#endif

IF (obj%isElementToElementsInitiated) RETURN
obj%isElementToElementsInitiated = .TRUE.

SELECT CASE (obj%xidim)
CASE (0_I4B)

CASE (1_I4B)
  CALL InitiateElementToElements1D(elementData=obj%elementData, &
                                   tNodesInMesh=obj%tNodes, &
                                   showTime=obj%showTime, &
                                   local_nptrs=obj%local_nptrs)

CASE (2_I4B)
  isok = .NOT. obj%isFaceConnectivityInitiated
  IF (isok) CALL obj%InitiateFaceConnectivity()

  CALL InitiateElementToElements3D(elementData=obj%elementData, &
                                   tFaceInMesh=obj%tFaces, &
                                   showTime=obj%showTime)

CASE (3_I4B)
  isok = .NOT. obj%isFaceConnectivityInitiated
  IF (isok) CALL obj%InitiateFaceConnectivity()

  CALL InitiateElementToElements3D(elementData=obj%elementData, &
                                   tFaceInMesh=obj%tFaces, &
                                   showTime=obj%showTime)

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "No case found for xidim.")
#endif

END SELECT

CALL MarkInternalNodes(obj=obj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_InitiateElementToElements

!----------------------------------------------------------------------------
!                                                         MarkInternalNodes
!----------------------------------------------------------------------------

SUBROUTINE MarkInternalNodes(obj)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj

  INTEGER(I4B) :: ii, jj, tsize, tElements, kk, ll, iface
  LOGICAL(LGT) :: isok
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "MarkInternalNodes()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (obj%xidim .EQ. 0) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  tElements = obj%GetTotalElements()

  loop1: DO ii = 1, tElements
    isok = obj%isBoundaryElement(ii, isLocal=.TRUE.)
    IF (.NOT. isok) CYCLE loop1

    tsize = SIZE(obj%elementData(ii)%ptr%boundaryData)
    DO jj = 1, tsize

      iface = obj%elementData(ii)%ptr%boundaryData(jj)
      nptrs = &
        obj%GetFacetConnectivity(globalElement=ii, &
                                 iface=iface, isLocal=.TRUE.)
      DO kk = 1, SIZE(nptrs)
        ll = obj%GetLocalNodeNumber(globalNode=nptrs(kk), islocal=.FALSE.)
        ! obj%nodeData(ll)%ptr%nodeType = TypeNode%domainBoundary
        CALL NodeData_SetNodeType(obj%nodeData(ll)%ptr, &
                                  TypeNode%domainBoundary)
      END DO
    END DO
  END DO loop1

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE MarkInternalNodes

!----------------------------------------------------------------------------
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ElementDataMethods
