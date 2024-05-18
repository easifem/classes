! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

MODULE FEDOF_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: DOF_, BaseContinuity_, BaseInterpolation_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE
PRIVATE

PUBLIC :: FEDOF_
PUBLIC :: FEDOFPointer_
CHARACTER(*), PARAMETER :: modName = "FEDOF_Class"

!----------------------------------------------------------------------------
!                                                                   FEDOF_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: FEDOF data type

TYPE, EXTENDS(DOF_) :: FEDOF_
  CLASS(BaseContinuity_), ALLOCATABLE :: baseContinuity
  !! continuity or conformity of basis defined on reference
  !! element, following values are allowed
  !! H1, HCurl, HDiv, DG
  CLASS(BaseInterpolation_), ALLOCATABLE :: baseInterpolation
  !! Type of basis functions used for interpolation on reference
  !! element, Following values are allowed
  !! LagrangeInterpolation
  !! HermitInterpolation
  !! SerendipityInterpolation
  !! HierarchyInterpolation
  !! OrthogonalInterpolation
  CLASS(AbstractMesh_), POINTER :: mesh => NULL()
  !! Pointer to domain

  INTEGER(I4B), ALLOCATABLE :: edgeIA(:), edgeJA(:)
  !! sparsity for edge, the size of edgeJA is equal to the total number of
  !! degrees of freedom on edge,
  !! the size of edgeIA is equal to the total number of edges + 1
  !! The degrees of freedom of iedge is stored in
  !! edgeJA(edgeIA(iedge):edgeIA(iedge+1)-1)

  INTEGER(I4B), ALLOCATABLE :: faceIA(:), faceJA(:)
  !! sparsity for face, the size of faceJA is equal to the total number of
  !! degrees of freedom on face,
  !! the size of faceIA is equal to the total number of faces + 1
  !! The degrees of freedom of iface is stored in
  !! faceJA(faceIA(iface):faceIA(iface+1)-1)

  INTEGER(I4B), ALLOCATABLE :: cellIA(:), cellJA(:)
  !! sparsity for cell, the size of cellJA is equal to the total number of
  !! degrees of freedom on cell,
  !! the size of cellIA is equal to the total number of cells + 1
  !! The degrees of freedom of icell is stored in
  !! cellJA(cellIA(icell):cellIA(icell+1)-1)

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: GetVertexDOF => obj_GetVertexDOF
  PROCEDURE, PUBLIC, PASS(obj) :: GetEdgeDOF => obj_GetEdgeDOF
  PROCEDURE, PUBLIC, PASS(obj) :: GetFaceDOF => obj_GetFaceDOF
  PROCEDURE, PUBLIC, PASS(obj) :: GetCellDOF => obj_GetCellDOF

END TYPE FEDOF_

!----------------------------------------------------------------------------
!                                                             FEDOFPointer_
!----------------------------------------------------------------------------

TYPE :: FEDOFPointer_
  TYPE(FEDOF_), POINTER :: ptr => NULL()
END TYPE FEDOFPointer_

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate an instance of fe dof

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, baseContinuity, baseInterpolation, &
                                  order, mesh)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    INTEGER(I4B), INTENT(IN) :: order
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate an instance of fe dof

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, baseContinuity, baseInterpolation, &
                                  order, mesh)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    INTEGER(I4B), INTENT(IN) :: order(:)
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetConnectivity_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Get the connectivity

INTERFACE
 MODULE SUBROUTINE obj_GetConnectivity_(obj, ans, tsize, opt, globalElement, &
                                         islocal)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! connectivity of element
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of data written in con
    CHARACTER(*), INTENT(IN) :: opt
    !! opt = Vertex
    !! opt = Edge
    !! opt = Face
    !! opt = Cell
    !! opt = All
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if islocal true then globalElement is local element number
  END SUBROUTINE obj_GetConnectivity_
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetVertexDOF@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetVertexDOF(obj, globalNode, ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
  END SUBROUTINE obj_GetVertexDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetEdgeDOF@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetEdgeDOF(obj, globalEdge, ans, tsize)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalEdge
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetEdgeDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetFaceDOF@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetFaceDOF(obj, globalFace, ans, tsize)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalFace
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetFaceDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetCellDOF@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetCellDOF(obj, globalCell, ans, tsize)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalCell
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetCellDOF
END INTERFACE

END MODULE FEDOF_Class
