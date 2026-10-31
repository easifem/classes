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

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Broken H1 spaces on Line elements using Lagrange polynomials
!
! Defines the finite element type, its pointer wrapper and the procedures
! declared in this module. Implementations reside in the corresponding H1DGFE
! submodule.
!
MODULE LineH1DGLagrangeFE_Class
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE LineH1LagrangeFE_Class, ONLY: LineH1LagrangeFE_
IMPLICIT NONE
PRIVATE

PUBLIC :: LineH1DGLagrangeFE_
PUBLIC :: LineH1DGLagrangeFEPointer_
PUBLIC :: LineH1DGLagrangeFEPointer

!----------------------------------------------------------------------------
!                                                        LineH1DGLagrangeFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Broken H1 spaces on line elements for Lagrange basis
!
! Extends LineH1LagrangeFE_ to handle broken H1 spaces on line element.
! Polynomial family is Lagrange basis.
!
! Everything is inherited from parent. Nothing new is defined.
!
TYPE, EXTENDS(LineH1LagrangeFE_) :: LineH1DGLagrangeFE_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: FixOrientOfNptrs => obj_FixOrientOfNptrs
  PROCEDURE, PUBLIC, PASS(obj) :: SetOrientation => obj_SetOrientation
END TYPE LineH1DGLagrangeFE_

!----------------------------------------------------------------------------
!                                             LineH1DGLagrangeFEPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-31
! summary: Nullable LineH1DGLagrangeFE pointer wrapper
!
! Contains a polymorphic ptr initialized to NULL(). The wrapper does not
! establish ownership of its target; retain an owning reference when target
! allocations must be deallocated.
!
TYPE :: LineH1DGLagrangeFEPointer_
  CLASS(LineH1DGLagrangeFE_), POINTER :: ptr => NULL()
END TYPE LineH1DGLagrangeFEPointer_

!----------------------------------------------------------------------------
!                                       LineH1DGLagrangeFEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-31
! summary: Allocate an uninitialized finite element target
!
! Allocates a new target of the concrete finite element type without calling
! Initiate. The caller owns the allocation: call its DEALLOCATE binding to
! reset state, then use intrinsic DEALLOCATE on the target when finished.
!
INTERFACE LineH1DGLagrangeFEPointer
  MODULE FUNCTION obj_LineH1DGLagrangeFEPointer1() RESULT(ans)
    TYPE(LineH1DGLagrangeFE_), POINTER :: ans
    !! Allocated finite element target returned to the caller.
  END FUNCTION obj_LineH1DGLagrangeFEPointer1
END INTERFACE LineH1DGLagrangeFEPointer

!----------------------------------------------------------------------------
!                                                  FixOrientOfNptrs@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Do not reorder connectivity
!
! We do not need reodering in case of DG. So we are overloading this method.
! This method does nothing.

INTERFACE
  MODULE SUBROUTINE obj_FixOrientOfNptrs(obj, nptrs, tsize)
    CLASS(LineH1DGLagrangeFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! Node connectivity; the first tsize entries form the active buffer.
    INTEGER(I4B), INTENT(IN) :: tsize
    !! Number of active connectivity entries.
  END SUBROUTINE obj_FixOrientOfNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                             SetOrientation
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-31
! summary: We do not need orientation in DG.
!
! This method sets unit orientation, we do not need
! orientation in case of DG
! vikaWe do not need orientation in DG.
!
! This method sets unit orientation, we do not need
! orientation in case of DG. That is why this routine
! overloads this method from [[LineH1FE_]].
INTERFACE
  MODULE SUBROUTINE obj_SetOrientation( &
    obj, cellOrient, faceOrient, edgeOrient, tCell, tFace, tEdge, errCheck)
    CLASS(LineH1DGLagrangeFE_), INTENT(INOUT) :: obj
    !! Finite element object of the declared class.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
    !! Cell orientation data; conventions depend on element topology.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
    !! Facet orientations, normally three rows per facet.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
    !! Edge orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCell
    !! Active size of cell order or orientation data.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFace
    !! Active number of face columns.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdge
    !! Active size of edge data.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
    !! Optional DEBUG validation control forwarded where supported.
  END SUBROUTINE obj_SetOrientation
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LineH1DGLagrangeFE_Class
