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
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: Display, ToString
USE ReferenceElement_Method, ONLY: REFELEM_MAX_FACES => PARAM_REFELEM_MAX_FACES
USE AbstractMeshUtility, ONLY: InitiateElementToElements3D, &
  & InitiateElementToElements2D, &
  & InitiateElementToElements1D
USE NodeData_Class, ONLY: TypeNode
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                 InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateElementToElements
CHARACTER(*), PARAMETER :: myName = "obj_InitiateElementToElements()"
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

problem = .NOT. ALLOCATED(obj%elementData)

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractMesh_::obj%elementData is not allocated')
  RETURN
END IF

IF (obj%isElementToElementsInitiated) RETURN
obj%isElementToElementsInitiated = .TRUE.

SELECT CASE (obj%xidim)
CASE (0_I4B)

CASE (1_I4B)

  CALL InitiateElementToElements1D(elementData=obj%elementData, &
                             tNodesInMesh=obj%tNodes, showTime=obj%showTime, &
                                   local_nptrs=obj%local_nptrs)

CASE (2_I4B)

  problem = .NOT. obj%isFaceConnectivityInitiated
  IF (problem) CALL obj%InitiateFaceConnectivity()

  CALL InitiateElementToElements3D(elementData=obj%elementData, &
                                tFaceInMesh=obj%tFaces, showTime=obj%showTime)

CASE (3_I4B)

  problem = .NOT. obj%isFaceConnectivityInitiated
  IF (problem) CALL obj%InitiateFaceConnectivity()

  CALL InitiateElementToElements3D(elementData=obj%elementData, &
                                tFaceInMesh=obj%tFaces, showTime=obj%showTime)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found for xidim '  &
    & //ToString(obj%xidim))
END SELECT

CALL MarkInternalNodes(obj=obj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_InitiateElementToElements

!----------------------------------------------------------------------------
!                                                         MarkInternalNodes
!----------------------------------------------------------------------------

SUBROUTINE MarkInternalNodes(obj)
  CLASS(AbstractMesh_), INTENT(INOUT) :: obj

  INTEGER(I4B) :: ii, jj, tsize, tElements, kk, ll
  LOGICAL(LGT) :: isok
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "MarkInternalNodes()"
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

  IF (obj%xidim .EQ. 0) RETURN

  tElements = obj%GetTotalElements()

  DO ii = 1, tElements

    isok = obj%isBoundaryElement(ii, isLocal=.TRUE.)
    IF (.NOT. isok) CYCLE

    tsize = SIZE(obj%elementData(ii)%boundaryData)
    DO jj = 1, tsize
      nptrs = obj%GetFacetConnectivity(globalElement=ii,  &
        & iface=obj%elementData(ii)%boundaryData(jj),  &
        & isLocal=.TRUE.)
      DO kk = 1, SIZE(nptrs)
        ll = obj%GetLocalNodeNumber(globalNode=nptrs(kk), islocal=.FALSE.)
        obj%nodeData(ll)%nodeType = TypeNode%domainBoundary
      END DO
    END DO

  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

END SUBROUTINE MarkInternalNodes

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ElementDataMethods
