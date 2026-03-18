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

MODULE GmshModelMesh_Class
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshModelMesh_
PUBLIC :: TypeGmshModelMesh
PUBLIC :: GmshModelMeshPointer_

!----------------------------------------------------------------------------
!                                                             GmshModelMesh_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Gmsh model mesh
!
!# GmshModelMesh_
!
! Gmsh model mesh.
!
TYPE :: GmshModelMesh_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(Obj) :: Generate => obj_Generate
  PROCEDURE, PUBLIC, PASS(Obj) :: Partition => obj_Partition
  PROCEDURE, PUBLIC, PASS(Obj) :: Unpartition => obj_Unpartition
  PROCEDURE, PUBLIC, PASS(Obj) :: Optimize => obj_Optimize
  PROCEDURE, PUBLIC, PASS(obj) :: Recombine => obj_Recombine
  PROCEDURE, PUBLIC, PASS(obj) :: Refine => obj_Refine
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrder => obj_SetOrder
END TYPE GmshModelMesh_

!----------------------------------------------------------------------------
!                                                          TypeGmshModelMesh
!----------------------------------------------------------------------------

TYPE(GmshModelMesh_), PARAMETER :: TypeGmshModelMesh = GmshModelMesh_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelMeshPointer_
  CLASS(GmshModelMesh_), POINTER :: ptr => NULL()
END TYPE GmshModelMeshPointer_

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Initiate gmsh model mesh
!
!# Initiate
!
! Initiate gmsh model mesh.
!
INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj)
    CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Generate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Generate mesh
!
!# Generate
!
! Generate mesh.

INTERFACE
  MODULE FUNCTION obj_Generate(obj, dim) RESULT(ans)
    CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION obj_Generate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Partition
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary:         Partition
!
!# Partition
!
! Partition of mesh.
INTERFACE
  MODULE FUNCTION obj_Partition(obj, numPart) RESULT(ans)
    CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: numPart
    INTEGER(I4B) :: ans
  END FUNCTION obj_Partition
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Unpartition
!
!# Unpartition
!
! Unpartition.
!
INTERFACE
  MODULE FUNCTION obj_Unpartition(obj) RESULT(ans)
    CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_Unpartition
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Optimize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Optimize
!
!# Optimize
!
! Optimize the mesh.
!

INTERFACE
  MODULE FUNCTION obj_Optimize(obj, method, force, niter, dimTags) &
    RESULT(ans)
    CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: method
    INTEGER(I4B), INTENT(IN) :: force, niter
    INTEGER(I4B), INTENT(IN) :: dimTags(:)
    INTEGER(I4B) :: ans
  END FUNCTION obj_Optimize
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Recombine
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Recombine the mesh
!
!# Recombine
!
! Recombining the mesh.
!
INTERFACE
  MODULE FUNCTION obj_Recombine(obj) RESULT(ans)
    CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_Recombine
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Refine
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2026-03-18
! summary: Refine the mesh
!
!# Refine
!
! Refine the mesh.

INTERFACE
  MODULE FUNCTION obj_Refine(obj) RESULT(ans)
    CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_Refine
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   SetOrder
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Set order the mesh
!
!# SetOrder
!
! Set the order the mesh.

INTERFACE
  MODULE FUNCTION obj_SetOrder(obj, order) RESULT(ans)
    CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetOrder
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshModelMesh_Class
