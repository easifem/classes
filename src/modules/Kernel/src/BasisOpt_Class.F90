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

MODULE BasisOpt_Class

USE GlobalData, ONLY: I4B, DFP, LGT

USE BaseType, ONLY: ipopt => TypeInterpolationOpt, &
                    polyopt => TypePolynomialOpt, &
                    QuadraturePoint_

IMPLICIT NONE

PRIVATE

PUBLIC :: BasisOpt_, TypeBasisOpt

!----------------------------------------------------------------------------
!                                                             BasisOpt_Class
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-11
! summary: This class contains data necessary for forming basis functions

TYPE :: BasisOpt_
  CHARACTER(2) :: baseContinuity = "H1"
  !! conformity of basis functions
  CHARACTER(4) :: baseInterpolation = "LAGR"
  !! interpolation of basis functions
  INTEGER(I4B) :: quadratureType = ipopt%GaussLegendre
  !! quadrature type
  INTEGER(I4B) :: basisType = polyopt%monomial
  !! basis type in case baseInterpolation is Lagrange
  INTEGER(I4B) :: ipType = ipopt%equidistance
  !! interpolation type incase baseInterpolation is Lagrange
  REAL(DFP) :: alpha(3) = 0.0_DFP
  !! Jacobi polynomial parameter, x, y, z
  REAL(DFP) :: beta(3) = 0.0_DFP
  !! Jacobi polynomial parametera, x, y, z
  REAL(DFP) :: lambda(3) = 0.5_DFP
  !! Ultraspherical polynomial parameter, x, y, z
  CHARACTER(128) :: quadratureType_char = "GAUSSLEGENDRE"
  !! quadrature type
  CHARACTER(128) :: basisType_chatr = "MONOMIAL"
  !! basis type in string format
  CHARACTER(128) :: ipType_char = "EQUIDISTANCE"
  !! interpolation type in string format
  TYPE(QuadraturePoint_) :: qp
  !! QuadraturePoint
END TYPE BasisOpt_

!----------------------------------------------------------------------------
!                                                              TypeBasisOpt
!----------------------------------------------------------------------------

TYPE(BasisOpt_), PARAMETER :: TypeBasisOpt = BasisOpt_()

!----------------------------------------------------------------------------
!                                                                   Methods
!----------------------------------------------------------------------------

END MODULE BasisOpt_Class
