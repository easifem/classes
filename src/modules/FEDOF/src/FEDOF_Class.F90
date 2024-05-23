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
USE GlobalData, ONLY: DFP, I4B, LGT, INT8
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_

IMPLICIT NONE
PRIVATE

PUBLIC :: FEDOF_
PUBLIC :: FEDOFPointer_
CHARACTER(*), PARAMETER :: modName = "FEDOF_Class"
CHARACTER(*), PARAMETER :: myprefix = "FEDOF"

!----------------------------------------------------------------------------
!                                                                   FEDOF_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: FEDOF data type

TYPE :: FEDOF_
  INTEGER(I4B) :: tdof = 0
  !! Total number of degrees of freedom
  INTEGER(I4B) :: tNodes = 0
  !! Total number of nodes
  INTEGER(I4B) :: tEdges = 0
  !! Total number of edges
  INTEGER(I4B) :: tFaces = 0
  !! Total number of faces
  INTEGER(I4B) :: tCells = 0
  !! Total number of cells
  CHARACTER(:), ALLOCATABLE :: baseContinuity
  !! continuity or conformity of basis defined on reference
  !! element, following values are allowed
  !! H1, HCurl, HDiv, DG
  CHARACTER(:), ALLOCATABLE :: baseInterpolation
  !! Type of basis functions used for interpolation on reference
  !! element, Following values are allowed
  !! LagrangeInterpolation
  !! HermitInterpolation
  !! SerendipityInterpolation
  !! HierarchyInterpolation
  !! OrthogonalInterpolation
  CLASS(AbstractMesh_), POINTER :: mesh => NULL()
  !! Pointer to domain

  INTEGER(INT8), ALLOCATABLE :: cellOrder(:)
  !! Order of each cell
  INTEGER(INT8), ALLOCATABLE :: faceOrder(:)
  !! order of each face
  INTEGER(INT8), ALLOCATABLE :: edgeOrder(:)
  !! order of each edge

  INTEGER(I4B), ALLOCATABLE :: edgeIA(:)
  !! sparsity for edge, the size of edgeJA is equal to the total number of
  !! degrees of freedom on edge,
  !! the size of edgeIA is equal to the total number of edges + 1
  !! The degrees of freedom of iedge is stored in
  !! edgeJA(edgeIA(iedge):edgeIA(iedge+1)-1)

  INTEGER(I4B), ALLOCATABLE :: faceIA(:)
  !! sparsity for face, the size of faceJA is equal to the total number of
  !! degrees of freedom on face,
  !! the size of faceIA is equal to the total number of faces + 1
  !! The degrees of freedom of iface is stored in
  !! faceJA(faceIA(iface):faceIA(iface+1)-1)

  INTEGER(I4B), ALLOCATABLE :: cellIA(:)
  !! sparsity for cell, the size of cellJA is equal to the total number of
  !! degrees of freedom on cell,
  !! the size of cellIA is equal to the total number of cells + 1
  !! The degrees of freedom of icell is stored in
  !! cellJA(cellIA(icell):cellIA(icell+1)-1)

CONTAINS

  !SET:
  !@ConstructorMethods
  PROCEDURE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate FEDOF by using homogeneous order
  PROCEDURE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate FEDOF by using inhomogeneous order
  PROCEDURE, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate FEDOF from ParameterList
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2, Initiate3
  !! Generic method for initiating FEDOF

  PROCEDURE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Copy
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy

  PROCEDURE, PASS(obj) :: SetCellOrder => obj_SetCellOrder
  PROCEDURE, PASS(obj) :: SetFaceOrder => obj_SetFaceOrder
  PROCEDURE, PASS(obj) :: SetEdgeOrder => obj_SetEdgeOrder

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data

  !IO:
  !@IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the contents of FEDOF

  !GET:
  !@GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetVertexDOF => obj_GetVertexDOF
  !! Get vertex degrees of freedom

  PROCEDURE, PUBLIC, PASS(obj) :: GetEdgeDOF => obj_GetEdgeDOF
  !! Get edge degrees of freedom

  PROCEDURE, PUBLIC, PASS(obj) :: GetFaceDOF => obj_GetFaceDOF
  !! Get face degrees of freedom

  PROCEDURE, PUBLIC, PASS(obj) :: GetCellDOF => obj_GetCellDOF
  !! Get cell degrees of freedom

  PROCEDURE, PASS(obj) :: GetTotalDOF1 => obj_GetTotalDOF1
  !! Retuns the total degrees of freedom in FEDOF
  PROCEDURE, PASS(obj) :: GetTotalDOF2 => obj_GetTotalDOF2
  !! Retuns the total dof of an element
  GENERIC, PUBLIC :: GetTotalDOF => GetTotalDOF1, GetTotalDOF2
  !! Generic mehthod for getting the total dof

  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  !! Get the prefix for setting the data

  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity_ => obj_GetConnectivity_
  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity => obj_GetConnectivity

END TYPE FEDOF_

!----------------------------------------------------------------------------
!                                                             FEDOFPointer_
!----------------------------------------------------------------------------

TYPE :: FEDOFPointer_
  TYPE(FEDOF_), POINTER :: ptr => NULL()
END TYPE FEDOFPointer_

!----------------------------------------------------------------------------
!                                           SetFEDOFParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Set the essential parameters for constructing the FEDOF

INTERFACE
MODULE SUBROUTINE SetFEDOFParam( param, baseContinuity, baseInterpolation, orderFile )
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity or conformity of basis defined on reference
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! Type of basis functions used for interpolation on reference
    CHARACTER(*), INTENT(IN) :: orderFile
    !! file containing the order of each element
  END SUBROUTINE SetFEDOFParam
END INTERFACE

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
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate an instance of fe dof

INTERFACE
  MODULE SUBROUTINE obj_Initiate3(obj, param, mesh)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE obj_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Copy@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Copy

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    CLASS(FEDOF_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Deallocate the data

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Get the connectivity (function)

INTERFACE
  MODULE FUNCTION obj_GetConnectivity(obj, opt, globalElement, islocal) &
    RESULT(ans)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    !! FEDOF object
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
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! connectivity of element
  END FUNCTION obj_GetConnectivity
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
  MODULE SUBROUTINE obj_GetEdgeDOF(obj, globalEdge, ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalEdge
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
  END SUBROUTINE obj_GetEdgeDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetFaceDOF@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetFaceDOF(obj, globalFace, ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalFace
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
  END SUBROUTINE obj_GetFaceDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetCellDOF@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetCellDOF(obj, globalCell, ans, tsize, islocal)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalCell
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
  END SUBROUTINE obj_GetCellDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-21
! summary: Returns total number of dof in the FEDOF

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF1(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalDOF1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-21
! summary: Returns total number of dof in the FEDOF

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF2(obj, globalElement, islocal) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalDOF2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Get the prefix for setting essential parameters

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(FEDOF_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-19
! summary: Display the content of FE DOF

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetCellOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Set the cell order

INTERFACE
  MODULE SUBROUTINE obj_SetCellOrder(obj, cellOrder)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: cellOrder(:)
  END SUBROUTINE obj_SetCellOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetFaceOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Set the face order

INTERFACE
  MODULE SUBROUTINE obj_SetFaceOrder(obj, cellOrder)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: cellOrder(:)
  END SUBROUTINE obj_SetFaceOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetEdgeOrder@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Set the Edge order

INTERFACE
  MODULE SUBROUTINE obj_SetEdgeOrder(obj, cellOrder)
    CLASS(FEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: cellOrder(:)
  END SUBROUTINE obj_SetEdgeOrder
END INTERFACE

END MODULE FEDOF_Class
