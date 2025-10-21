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

MODULE OneDimFEFactoryUtility
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE ExceptionHandler_Class, ONLY: e
USE tomlf, ONLY: toml_table

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "OneDimFEFactory"

PUBLIC :: OneDimFEFactory

!----------------------------------------------------------------------------
!                                                           OneDimFEFactory
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Internal routine for creating pointer form baseContinuity
! and baseInterpolation

INTERFACE
  MODULE FUNCTION InternalOneDimFEFactory(baseContinuity, baseInterpolation) &
    RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    CLASS(AbstractOneDimFE_), POINTER :: ans
  END FUNCTION InternalOneDimFEFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                           OneDimFEFactory
!----------------------------------------------------------------------------

INTERFACE OneDimFEFactory
  MODULE FUNCTION OneDimFEFactory1( &
    baseContinuity, baseInterpolation, ipType, basisType, alpha, beta, &
    lambda, order, fetype, quadratureType, quadratureOrder, quadratureNips, &
    quadratureAlpha, quadratureBeta, quadratureLambda) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Continuity or Conformity of basis function.
    !! This parameter is used to determine the nodal coordinates of
    !! reference element, when xij is not present.
    !! If xij is present then this parameter is ignored
    !! H1* (default), HDiv, HCurl, DG
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! Basis function family used for interpolation.
    !! This parameter is used to determine the nodal coordinates of
    !! reference element, when xij is not present.
    !! If xij is present then this parameter is ignored
    !! LagrangeInterpolation, LagrangePolynomial
    !! SerendipityInterpolation, SerendipityPolynomial
    !! HierarchyInterpolation, HierarchyPolynomial
    !! OrthogonalInterpolation, OrthogonalPolynomial
    !! HermitInterpolation, HermitPolynomial
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! Interpolation point type, It is required when
    !! baseInterpol is LagrangePolynomial
    !! Default ipType is Equidistance
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Basis type: Legendre, Lobatto, Ultraspherical,
    !! Jacobi, Monomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameters
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Isotropic Order of finite element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element type
    !! Default is Scalar
    !! For HDiv and Hcurl it should be Vector
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType
    !! Quadrature type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder
    !! Accuracy of quadrature rule
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(1)
    !! Number of integration points
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda
    !! Ultraspherical parameter for quadrature
    CLASS(AbstractOneDimFE_), POINTER :: ans
  END FUNCTION OneDimFEFactory1
END INTERFACE OneDimFEFactory

!----------------------------------------------------------------------------
!                                                            OneDimFEFactory
!----------------------------------------------------------------------------

INTERFACE OneDimFEFactory
  MODULE FUNCTION OneDimFEFactory2(table) RESULT(ans)
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(AbstractOneDimFE_), POINTER :: ans
  END FUNCTION OneDimFEFactory2
END INTERFACE OneDimFEFactory

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE OneDimFEFactoryUtility
