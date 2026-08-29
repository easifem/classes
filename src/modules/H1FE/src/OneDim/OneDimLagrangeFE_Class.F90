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

MODULE OneDimLagrangeFE_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE BaseType, ONLY: QuadraturePoint_, ElemShapedata_
USE ExceptionHandler_Class, ONLY: e
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE
PRIVATE

PUBLIC :: OneDimLagrangeFE_
PUBLIC :: OneDimLagrangeFEPointer_
PUBLIC :: OneDimLagrangeFEPointer
PUBLIC :: OneDimLagrangeFE
PUBLIC :: FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                          OneDimLagrangeFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary: OneDimLagrangeFE is lagrange fe for one dimensional cases

TYPE, EXTENDS(AbstractOneDimFE_) :: OneDimLagrangeFE_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  !! Get local element shape data

  PROCEDURE, PASS(obj) :: GetTimeDOFValueFromSTFunction => &
    obj_GetTimeDOFValueFromSTFunction
  !! Get time degree of freedom values from space-time function
  PROCEDURE, PASS(obj) :: GetTimeDOFValueFromConstant => &
    obj_GetTimeDOFValueFromConstant
  !! Get time degree of freedom values from space-time function
  PROCEDURE, PUBLIC, PASS(obj) :: GetDOFValueFromTimeFunction => &
    obj_GetDOFValueFromTimeFunction
  !! Get  degree of freedom values from Time- function
  PROCEDURE, PUBLIC, PASS(obj) :: GetDOFValueFromSpaceFunction => &
    obj_GetDOFValueFromSpaceFunction
  !! Get  degree of freedom values from space- function
  PROCEDURE, PUBLIC, PASS(obj) :: GetDOFValueFromConstant => &
    obj_GetDOFValueFromConstant
  !! Get  degree of freedom values from space- function
END TYPE OneDimLagrangeFE_

!----------------------------------------------------------------------------
!                                                   OneDimLagrangeFEPointer_
!----------------------------------------------------------------------------

TYPE :: OneDimLagrangeFEPointer_
  CLASS(OneDimLagrangeFE_), POINTER :: ptr => NULL()
END TYPE OneDimLagrangeFEPointer_

!----------------------------------------------------------------------------
!                                          GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-15
! summary:  Get local element shape data shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(OneDimLagrangeFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!                                            OneDimLagrangeFEPointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  Empty constructor

INTERFACE OneDimLagrangeFEPointer
  MODULE FUNCTION obj_OneDimLagrangeFEPointer1() RESULT(ans)
    TYPE(OneDimLagrangeFE_), POINTER :: ans
  END FUNCTION obj_OneDimLagrangeFEPointer1
END INTERFACE OneDimLagrangeFEPointer

!----------------------------------------------------------------------------
!                                                   OneDimLagrangeFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE OneDimLagrangeFEPointer
  MODULE FUNCTION obj_OneDimLagrangeFEPointer2( &
    baseContinuity, ipType, basisType, order, alpha, beta, lambda) &
    RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! base continuity of the finite element
    !! read in BasisOpt_Class
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type, It is required when
    !! baseInterpol is LagrangePolynomial.
    !! Read more infor in BasisOpt_Class
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Basis type:
    !! Legendre, Lobatto, Ultraspherical, Jacobi, Monomial
    INTEGER(I4B), INTENT(IN) :: order
    !! Isotropic Order of finite element
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameters
    TYPE(OneDimLagrangeFE_), POINTER :: ans
  END FUNCTION obj_OneDimLagrangeFEPointer2
END INTERFACE OneDimLagrangeFEPointer

!----------------------------------------------------------------------------
!                                                   OneDimLagrangeFE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary: Constructor method

INTERFACE OneDimLagrangeFE
  MODULE FUNCTION obj_OneDimLagrangeFE( &
    baseContinuity, ipType, basisType, order, alpha, beta, lambda) &
    RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! base continuity of the finite element
    !! read in BasisOpt_Class
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type, It is required when
    !! baseInterpol is LagrangePolynomial.
    !! Read more infor in BasisOpt_Class
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Basis type:
    !! Legendre, Lobatto, Ultraspherical, Jacobi, Monomial
    INTEGER(I4B), INTENT(IN) :: order
    !! Isotropic Order of finite element
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameters
    TYPE(OneDimLagrangeFE_) :: ans
  END FUNCTION obj_OneDimLagrangeFE
END INTERFACE OneDimLagrangeFE

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-24
! summary:  Deallocate a vector of OneDimLagrangeFE

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(OneDimLagrangeFE_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of OneDimLagrangeFEPointer_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(OneDimLagrangeFEPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                    GetTimeDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-02
! summary: Get time dof value for a constant function

INTERFACE
  MODULE SUBROUTINE obj_GetTimeDOFValueFromConstant( &
    obj, elemsd, times, ans, tsize, massMat, ipiv, funcValue, &
    onlyFaceBubble, icompo)
    CLASS(OneDimLagrangeFE_), INTENT(INOUT) :: obj
    !! Abstract finite elemenet
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! element shape function defined inside the cell
    REAL(DFP), INTENT(IN) :: times(:)
    !! nodal coordinates of reference element
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! nodal coordinates of interpolation points
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! data written in xij
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! mass matrix
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! pivot indices for LU decomposition of mass matrix
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! function values at quadrature points will be stored here
    !! used internally, size should be atleast elemsd%nips
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: onlyFaceBubble
    !! if true then we include only face bubble, that is,
    !! only include internal face bubble.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! tVertices are needed when onlyFaceBubble is true
    !! tVertices are total number of vertex degree of
    !! freedom
  END SUBROUTINE obj_GetTimeDOFValueFromConstant
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetDOFValue@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-01
! summary: Get time degree of freedom values from space-time function

INTERFACE
  MODULE SUBROUTINE obj_GetTimeDOFValueFromSTFunction( &
    obj, elemsd, x, nsd, times, func, ans, tsize, massMat, ipiv, funcValue, &
    onlyFaceBubble, icompo)
    CLASS(OneDimLagrangeFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! time element shape data
    REAL(DFP), INTENT(IN) :: x(:)
    !! a space point coordinate
    INTEGER(I4B), INTENT(IN) :: nsd
    !! number of space dimensions
    REAL(DFP), INTENT(IN) :: times(:)
    !! time element coordinates
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! user defined functions quadrature values of function
    !! It should be space-time function with 4 argumnets
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! returned dof values
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of returned dof values
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! mass matrix used internally
    !! size should be atleast elemsd%nns x elemsd%nns
    INTEGER(I4B), INTENT(OUT) :: ipiv(:)
    !! size should be atleast elemsd%nns
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! function values at quadrature points will be stored here
    !! used internally, size should be atleast elemsd%nips
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! if true then only inside dof are returned
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
  END SUBROUTINE obj_GetTimeDOFValueFromSTFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetDOFValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-02
! summary: Get time dof value for a constant function

INTERFACE
  MODULE SUBROUTINE obj_GetDOFValueFromConstant( &
    obj, elemsd, times, ans, tsize, massMat, ipiv, funcValue, &
    onlyFaceBubble, icompo)
    CLASS(OneDimLagrangeFE_), INTENT(INOUT) :: obj
    !! Abstract finite elemenet
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! element shape function defined inside the cell
    REAL(DFP), INTENT(IN) :: times(:)
    !! nodal coordinates of reference element
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! nodal coordinates of interpolation points
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! data written in xij
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! mass matrix
    INTEGER(I4B), INTENT(INOUT) :: ipiv(:)
    !! pivot indices for LU decomposition of mass matrix
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! function values at quadrature points will be stored here
    !! used internally, size should be atleast elemsd%nips
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: onlyFaceBubble
    !! if true then we include only face bubble, that is,
    !! only include internal face bubble.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: icompo
    !! tVertices are needed when onlyFaceBubble is true
    !! tVertices are total number of vertex degree of
    !! freedom
  END SUBROUTINE obj_GetDOFValueFromConstant
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetDOFValue@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-01
! summary: Get time degree of freedom values from space-time function

INTERFACE
  MODULE SUBROUTINE obj_GetDOFValueFromSpaceFunction( &
    obj, elemsd, x, func, ans, tsize, massMat, ipiv, funcValue, &
    onlyFaceBubble)
    CLASS(OneDimLagrangeFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! space element shape data
    REAL(DFP), INTENT(IN) :: x(:)
    !! These are nodal coordinates of vertices of elements
    !! We only have two vertices
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! User defined function of space, it should have 1 argument
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! returned dof values
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of returned dof values
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! mass matrix used internally
    !! size should be atleast elemsd%nns x elemsd%nns
    INTEGER(I4B), INTENT(OUT) :: ipiv(:)
    !! size should be atleast elemsd%nns
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! function values at quadrature points will be stored here
    !! used internally, size should be atleast elemsd%nips
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! if true then only inside dof are returned
  END SUBROUTINE obj_GetDOFValueFromSpaceFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetDOFValueFromTimeFunction@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-01
! summary: Get time degree of freedom values from space-time function

INTERFACE
  MODULE SUBROUTINE obj_GetDOFValueFromTimeFunction( &
    obj, elemsd, times, func, ans, tsize, massMat, ipiv, &
    funcValue, onlyFaceBubble)
    CLASS(OneDimLagrangeFE_), INTENT(INOUT) :: obj
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd
    !! time element shape data
    REAL(DFP), INTENT(IN) :: times(:)
    !! These are nodal coordinates of vertices of time elements
    !! We only have two vertices, [t1, t2]
    TYPE(UserFunction_), INTENT(INOUT) :: func
    !! User defined function of time, it should have 1 argument
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! returned dof values
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of returned dof values
    REAL(DFP), INTENT(INOUT) :: massMat(:, :)
    !! mass matrix used internally
    !! size should be atleast elemsd%nns x elemsd%nns
    INTEGER(I4B), INTENT(OUT) :: ipiv(:)
    !! size should be atleast elemsd%nns
    REAL(DFP), INTENT(INOUT) :: funcValue(:)
    !! function values at quadrature points will be stored here
    !! used internally, size should be atleast elemsd%nips
    LOGICAL(LGT), INTENT(IN) :: onlyFaceBubble
    !! if true then only inside dof are returned
  END SUBROUTINE obj_GetDOFValueFromTimeFunction
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE OneDimLagrangeFE_Class
