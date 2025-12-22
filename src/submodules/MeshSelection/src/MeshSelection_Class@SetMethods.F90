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

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This module defines a data type for mesh selection

SUBMODULE(MeshSelection_Class) SetMethods
USE IntVector_Method, ONLY: Append, RemoveDuplicates, IsAllocated
USE BoundingBox_Method, ONLY: BoundingBox_Append => Append
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE Display_Method, ONLY: ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Add()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL addmeshid(obj, meshid, dim)
CALL addelemnum(obj, elemnum, dim)
CALL addnodenum(obj, nodenum, dim)
CALL addbox(obj, box, dim)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Add

!----------------------------------------------------------------------------
!                                                                 AddMeshID
!----------------------------------------------------------------------------

SUBROUTINE addmeshid(obj, meshid, dim)
  CLASS(MeshSelection_), INTENT(INOUT) :: obj
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshid(:), dim

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "addmeshid()"
#endif

  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isok = PRESENT(dim) .AND. PRESENT(meshID)
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  obj%ms(1) = .TRUE.
  SELECT CASE (dim)
  CASE (0)
    CALL Append(obj%pointMeshID, meshID)
  CASE (1)
    CALL Append(obj%curveMeshID, meshID)
  CASE (2)
    CALL Append(obj%surfaceMeshID, meshID)
  CASE (3)
    CALL Append(obj%volumeMeshID, meshID)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      'No case found for dim = '//ToString(dim))
#endif

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE addmeshid

!----------------------------------------------------------------------------
!                                                                addelemnum
!----------------------------------------------------------------------------

SUBROUTINE addelemnum(obj, elemnum, dim)
  CLASS(MeshSelection_), INTENT(INOUT) :: obj
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemnum(:), dim

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "addelemnum()"
#endif

  LOGICAL(LGT) :: isok

  isok = PRESENT(dim) .AND. PRESENT(elemnum)
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  obj%ms(2) = .TRUE.
  SELECT CASE (dim)
  CASE (0)
    CALL Append(obj%pointElemNum, elemnum)
  CASE (1)
    CALL Append(obj%curveElemNum, elemnum)
  CASE (2)
    CALL Append(obj%surfaceElemNum, elemnum)
  CASE (3)
    CALL Append(obj%volumeElemNum, elemnum)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      'No case found for dim = '//ToString(dim))
#endif
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE addelemnum

!----------------------------------------------------------------------------
!                                                                addnodenum
!----------------------------------------------------------------------------

SUBROUTINE addnodenum(obj, nodenum, dim)
  CLASS(MeshSelection_), INTENT(INOUT) :: obj
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: nodenum(:), dim

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "addnodenum()"
#endif
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isok = PRESENT(nodeNum)
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  obj%ms(3) = .TRUE.
  isok = .NOT. PRESENT(dim)
  IF (isok) THEN
    CALL Append(obj%nodeNum, nodeNum)

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  SELECT CASE (dim)
  CASE (0)
    CALL Append(obj%pointNodeNum, nodeNum)
  CASE (1)
    CALL Append(obj%curveNodeNum, nodeNum)
  CASE (2)
    CALL Append(obj%surfaceNodeNum, nodeNum)
  CASE (3)
    CALL Append(obj%volumeNodeNum, nodeNum)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      'No case found for dim = '//ToString(dim))
#endif

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE addnodenum

!----------------------------------------------------------------------------
!                                                                 addbox
!----------------------------------------------------------------------------

SUBROUTINE addbox(obj, box, dim)
  CLASS(MeshSelection_), INTENT(INOUT) :: obj
  TYPE(BoundingBox_), OPTIONAL, INTENT(IN) :: box(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "addbox()"
#endif

  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isok = PRESENT(dim) .AND. PRESENT(box)
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  obj%ms(4) = .TRUE.
  SELECT CASE (dim)
  CASE (0)
    CALL BoundingBox_Append(obj%pointBox, box)
  CASE (1)
    CALL BoundingBox_Append(obj%curveBox, box)
  CASE (2)
    CALL BoundingBox_Append(obj%surfaceBox, box)
  CASE (3)
    CALL BoundingBox_Append(obj%volumeBox, box)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      'No case found for dim = '//ToString(dim))
#endif
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE addbox

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = IsAllocated(obj%pointMeshID)
IF (isok) CALL RemoveDuplicates(obj%pointMeshID)

isok = IsAllocated(obj%curveMeshID)
IF (isok) CALL RemoveDuplicates(obj%curveMeshID)

isok = IsAllocated(obj%surfaceMeshID)
IF (isok) CALL RemoveDuplicates(obj%surfaceMeshID)

isok = IsAllocated(obj%volumeMeshID)
IF (isok) CALL RemoveDuplicates(obj%volumeMeshID)

isok = IsAllocated(obj%pointElemNum)
IF (isok) CALL RemoveDuplicates(obj%pointElemNum)

isok = IsAllocated(obj%curveElemNum)
IF (isok) CALL RemoveDuplicates(obj%curveElemNum)

isok = IsAllocated(obj%surfaceElemNum)
IF (isok) CALL RemoveDuplicates(obj%surfaceElemNum)

isok = IsAllocated(obj%volumeElemNum)
IF (isok) CALL RemoveDuplicates(obj%volumeElemNum)

isok = IsAllocated(obj%pointNodeNum)
IF (isok) CALL Append(obj%nodeNum, obj%pointNodeNum)

isok = IsAllocated(obj%curveNodeNum)
IF (isok) CALL Append(obj%nodeNum, obj%curveNodeNum)

isok = IsAllocated(obj%surfaceNodeNum)
IF (isok) CALL Append(obj%nodeNum, obj%surfaceNodeNum)

isok = IsAllocated(obj%volumeNodeNum)
IF (isok) CALL Append(obj%nodeNum, obj%volumeNodeNum)

isok = IsAllocated(obj%nodeNum)
IF (isok) CALL RemoveDuplicates(obj%nodeNum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!                                                         MeshSelectionSet
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
#endif

INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(obj)
DO ii = 1, tsize
  CALL obj(ii)%Set()
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                         SetMaterialToMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterialToMesh1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myname = "obj_SetMaterialToMesh1()"
#endif

CLASS(AbstractMesh_), POINTER :: mesh
INTEGER(I4B), POINTER :: intptr(:)
INTEGER(I4B) :: tsize, ii, iel
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

mesh => dom%GetMeshPointer(dim=dim)

#ifdef DEBUG_VER
abool = ASSOCIATED(mesh)
CALL AssertError1(abool, myname, "Mesh pointer is not associated")
#endif

! isSelectionByMeshID
intptr => NULL()
CALL obj%GetMeshIDPointer(dim=dim, ans=intptr, tsize=tsize)

DO ii = 1, tsize
  CALL mesh%SetMaterial(entityNum=intptr(ii), material=material, &
                        medium=medium)
END DO

! isSelectionByElemNum
intptr => NULL()
CALL obj%GetElemNumPointer(dim=dim, ans=intptr, tsize=tsize)

DO ii = 1, tsize
  iel = intptr(ii)
  CALL mesh%SetMaterial(medium=medium, material=material, &
                        globalElement=iel, islocal=.FALSE.)
END DO

mesh => NULL()
intptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetMaterialToMesh1

!----------------------------------------------------------------------------
!                                                          SetMaterialToMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterialToMesh2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMaterialToMesh2()"
#endif

INTEGER(I4B) :: dim

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dim = dom%GetNSD()
CALL obj%SetMaterialToMesh1(dom=dom, dim=dim, medium=medium, &
                            material=material)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetMaterialToMesh2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
