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

MODULE GmshModelOccMesh_Class
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshModelOccMesh_
PUBLIC :: TypeGmshModelOccMesh
PUBLIC :: GmshModelOccMeshPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelOccMesh_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(Obj) :: SetSize => obj_SetSize
END TYPE GmshModelOccMesh_

!----------------------------------------------------------------------------
!                                                       TypeGmshModelOccMesh
!----------------------------------------------------------------------------

TYPE(GmshModelOccMesh_), PARAMETER :: TypeGmshModelOccMesh = &
                                      GmshModelOccMesh_()

!----------------------------------------------------------------------------
!                                                    GmshModelOccMeshPointer
!----------------------------------------------------------------------------

TYPE :: GmshModelOccMeshPointer_
  CLASS(GmshModelOccMesh_), POINTER :: Ptr => NULL()
END TYPE GmshModelOccMeshPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: gmsh model occ mesh
!
!# SetSize
!
! Set size of mesh.
!
INTERFACE
  MODULE FUNCTION obj_SetSize(obj, dimTags, meshSize) RESULT(ans)
    CLASS(GmshModelOccMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dimTags(:)
    REAL(DFP), INTENT(IN) :: meshSize
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetSize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshModelOccMesh_Class
