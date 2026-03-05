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

SUBMODULE(AbstractDomain_Class) MeshDataMethods
USE GlobalData, ONLY: stdout
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE Kdtree2_Module, ONLY: Kdtree2_create
USE CPUTime_Class, ONLY: CPUTime_
USE ElemData_Class, ONLY: BOUNDARY_ELEMENT
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "AbstractDomain_Class@MeshDataMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                            InitiateKdtree
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateKdtree
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateKdtree()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: nsd

#ifdef DEBUG_VER
TYPE(CPUTime_) :: TypeCPUTime
#endif

#ifdef DEBUG_VER
IF (obj%showTime) CALL TypeCPUTime%SetStartTime()
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DeallocateKdtree()

#ifdef DEBUG_VER
isok = ALLOCATED(obj%nodeCoord)
CALL AssertError1(isok, myName, &
                  "obj%nodeCoord not allocated")
#endif

nsd = obj%nsd
obj%kdtree => Kdtree2_Create(input_data=obj%nodeCoord(1:nsd, :), &
                             dim=nsd, sort=math%no, &
                             rearrange=math%yes)

ALLOCATE (obj%kdresult(obj%tNodes))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

#ifdef DEBUG_VER
IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL Display(modName//" : "//myName// &
               " : time : "// &
               ToString(TypeCPUTime%GetTime()), unitno=stdout)
END IF
#endif
END PROCEDURE obj_InitiateKdtree

!----------------------------------------------------------------------------
!                                                     InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToElements()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "This routine should be implemented by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                     InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToNodes
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateExtraNodeToNodes()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "This routine should be implemented by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_InitiateNodeToNodes

!----------------------------------------------------------------------------
!                                                  InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateElementToElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateElementToElements()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "This routine should be implemented by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_InitiateElementToElements

!----------------------------------------------------------------------------
!                                                      InitiateBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateBoundaryData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateBoundaryData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "This routine should be implemented by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_InitiateBoundaryData

!----------------------------------------------------------------------------
!                                                     InitiateFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateFacetElements()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "This routine should be implemented by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_InitiateFacetElements

!----------------------------------------------------------------------------
!                                                   InitiateExtraNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateExtraNodeToNodes
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateExtraNodeToNodes()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(math%no, myName, &
                  "This routine should be implemented by child classes.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_InitiateExtraNodeToNodes

!----------------------------------------------------------------------------
!                                                        SetFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFacetElementType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFacetElementType()"
#endif
CLASS(AbstractMesh_), POINTER :: masterMesh
INTEGER(I4B) :: kk, iel, iface, telements
INTEGER(I4B), ALLOCATABLE :: faceID(:)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

masterMesh => obj%GetMeshPointer(dim=obj%nsd)

CALL masterMesh%GetParam(isBoundaryDataInitiated=isok)
IF (.NOT. isok) CALL masterMesh%InitiateBoundaryData()

telements = masterMesh%GetTotalElements()

DO iel = 1, telements
  isok = masterMesh%isBoundaryElement(globalElement=iel, &
                                      islocal=math%yes)
  IF (.NOT. isok) CYCLE

  faceID = masterMesh%GetBoundaryElementData(globalElement=iel, &
                                             islocal=math%yes)

  DO iface = 1, SIZE(faceID)

    kk = faceID(iface)

    CALL masterMesh%SetFacetElementType( &
      globalElement=iel, iface=kk, facetElementType=BOUNDARY_ELEMENT, &
      islocal=math%yes)

  END DO

END DO

NULLIFY (masterMesh)

IF (ALLOCATED(faceID)) DEALLOCATE (faceID)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFacetElementType

!----------------------------------------------------------------------------
!                                                      SetDomainFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetDomainFacetElement
! this routine does nothing at this level
! because we do not have difference between boundary element and
! domain boundary element
END PROCEDURE obj_SetDomainFacetElement

!----------------------------------------------------------------------------
!                                                                 SetMeshMap
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMeshmap
! this routine does nothing at abstract level
END PROCEDURE obj_SetMeshmap

!----------------------------------------------------------------------------
!                                                       SetMeshFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMeshFacetElement
! this routine does nothing at abstract level
END PROCEDURE obj_SetMeshFacetElement

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE MeshDataMethods
