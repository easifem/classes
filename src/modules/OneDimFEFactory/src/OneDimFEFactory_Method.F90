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

MODULE OneDimFEFactory_Method
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE LagrangeOneDimFE_Class, ONLY: LagrangeOneDimFE_
USE HierarchicalOneDimFE_Class, ONLY: HierarchicalOneDimFE_
USE OrthogonalOneDimFE_Class, ONLY: OrthogonalOneDimFE_
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
USE StringUtility, ONLY: UpperCase

USE tomlf, ONLY: toml_table

IMPLICIT NONE

CHARACTER(*), PARAMETER :: modName = "OneDimFEFactory"

PRIVATE
PUBLIC :: OneDimFEFactory

INTERFACE OneDimFEFactory
  MODULE PROCEDURE OneDimFEFactory1, OneDimFEFactory2, &
    OneDimFEFactory3
END INTERFACE OneDimFEFactory

CONTAINS

!----------------------------------------------------------------------------
!                                                          OneDimFEFactory
!----------------------------------------------------------------------------

FUNCTION OneDimFEFactory1(param) RESULT(ans)
  TYPE(ParameterList_) :: param
  CLASS(AbstractOneDimFE_), POINTER :: ans

  ! internal variables

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "OneDimFEFactory1()"
#endif

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END FUNCTION OneDimFEFactory1

!----------------------------------------------------------------------------
!                                                           OneDimFEFactory
!----------------------------------------------------------------------------

FUNCTION OneDimFEFactory2(baseContinuity, baseInterpolation, ipType, &
              basisType, alpha, beta, lambda, order, fetype, quadratureType, &
           quadratureOrder, quadratureNips, quadratureAlpha, quadratureBeta, &
                          quadratureLambda) RESULT(ans)
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

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "OneDimFEFactory1()"
  CHARACTER(LEN=4) :: baseInterpolation0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  baseInterpolation0 = UpperCase(baseInterpolation(1:4))

  SELECT CASE (baseInterpolation0)
  CASE ("LAGR")
    ALLOCATE (LagrangeOneDimFE_ :: ans)
  CASE ("HIER", "HEIR")
    ALLOCATE (HierarchicalOneDimFE_ :: ans)
  CASE ("ORTHO")
    ALLOCATE (OrthogonalOneDimFE_ :: ans)
  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for baseInterpolation')
    RETURN
  END SELECT

  CALL ans%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    order=order, feType=feType, ipType=ipType, &
                    basisType=basisType, alpha=alpha, &
                    beta=beta, lambda=lambda, &
                    quadratureType=quadratureType, &
                    quadratureOrder=quadratureOrder, &
                    quadratureNips=quadratureNips, &
                    quadratureAlpha=quadratureAlpha, &
                    quadratureBeta=quadratureBeta, &
                    quadratureLambda=quadratureLambda)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END FUNCTION OneDimFEFactory2

!----------------------------------------------------------------------------
!                                                            OneDimFEFactory
!----------------------------------------------------------------------------

FUNCTION OneDimFEFactory3(table) RESULT(ans)
  TYPE(toml_table), INTENT(INOUT) :: table
  CLASS(AbstractOneDimFE_), POINTER :: ans

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "OneDimFEFactory3()"
#endif

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END FUNCTION OneDimFEFactory3

!----------------------------------------------------------------------------
!                                                            OneDimFEFactory
!----------------------------------------------------------------------------

END MODULE OneDimFEFactory_Method
