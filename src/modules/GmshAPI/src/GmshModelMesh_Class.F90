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
USE Utility, ONLY: Reallocate
USE GmshInterface
USE CInterface, ONLY: C_PTR_TO_INT_VEC
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "GMSHMODELMESH_CLASS"
INTEGER(C_INT) :: ierr
!$OMP THREADPRIVATE(ierr)
INTEGER(I4B), PARAMETER :: maxStrLen = 256

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelMesh_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => mesh_Initiate
  PROCEDURE, PUBLIC, PASS(Obj) :: Generate => mesh_Generate
  PROCEDURE, PUBLIC, PASS(Obj) :: Partition => mesh_Partition
  PROCEDURE, PUBLIC, PASS(Obj) :: Unpartition => mesh_Unpartition
  PROCEDURE, PUBLIC, PASS(Obj) :: Optimize => mesh_Optimize
  PROCEDURE, PUBLIC, PASS(obj) :: Recombine => mesh_Recombine
  PROCEDURE, PUBLIC, PASS(obj) :: Refine => mesh_Refine
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrder => mesh_SetOrder
END TYPE GmshModelMesh_

PUBLIC :: GmshModelMesh_
TYPE(GmshModelMesh_), PUBLIC, PARAMETER :: TypeGmshModelMesh = &
  & GmshModelMesh_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelMeshPointer_
  CLASS(GmshModelMesh_), POINTER :: Ptr => NULL()
END TYPE

PUBLIC :: GmshModelMeshPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE mesh_Initiate(obj)
  CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
END SUBROUTINE mesh_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_Generate(obj, dim) RESULT(ans)
  CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: dim
  INTEGER(I4B) :: ans
  !
  CALL gmshModelMeshGenerate(dim, ierr)
  ans = INT(ierr, I4B)
END FUNCTION mesh_Generate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_Partition(obj, numPart) &
  & RESULT(ans)
  CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: numPart
  INTEGER(I4B) :: ans
  CALL gmshModelMeshPartition(numPart, ierr)
  ans = INT(ierr, I4B)
END FUNCTION mesh_Partition

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_Unpartition(obj) RESULT(ans)
  CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B) :: ans
  CALL gmshModelMeshUnpartition(ierr)
  ans = INT(ierr, I4B)
END FUNCTION mesh_Unpartition

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_Optimize(obj, method, force, niter, dimTags) &
  & RESULT(ans)
  CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
  CHARACTER(LEN=*), INTENT(IN) :: method
  INTEGER(I4B), INTENT(IN) :: force, niter
  INTEGER(I4B), INTENT(IN) :: dimTags(:)
  INTEGER(I4B) :: ans
  !
  CHARACTER(LEN=maxStrLen), TARGET :: method_
  method_ = TRIM(method)//C_NULL_CHAR
  CALL gmshModelMeshOptimize(C_LOC(method_), force, niter, &
    & dimTags, SIZE(dimTags, KIND=C_SIZE_T), ierr)
  ans = INT(ierr, I4B)
END FUNCTION mesh_Optimize

!----------------------------------------------------------------------------
!                                                                 Recombine
!----------------------------------------------------------------------------

FUNCTION mesh_Recombine(obj) RESULT(ans)
  CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B) :: ans
  CALL gmshModelMeshRecombine(ierr)
  ans = INT(ierr, I4B)
END FUNCTION mesh_Recombine

!----------------------------------------------------------------------------
!                                                                 Refine
!----------------------------------------------------------------------------

FUNCTION mesh_Refine(obj) RESULT(ans)
  CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B) :: ans
  CALL gmshModelMeshRefine(ierr)
  ans = INT(ierr, I4B)
END FUNCTION mesh_Refine

!----------------------------------------------------------------------------
!                                                                 SetOrder
!----------------------------------------------------------------------------

FUNCTION mesh_SetOrder(obj, order) RESULT(ans)
  CLASS(GmshModelMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: order
  INTEGER(I4B) :: ans
  !> main
  CALL gmshModelMeshSetOrder(order, ierr)
  ans = INT(ierr, I4B)
END FUNCTION mesh_SetOrder

END MODULE GmshModelMesh_Class
