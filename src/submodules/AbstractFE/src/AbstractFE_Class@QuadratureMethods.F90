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

SUBMODULE(AbstractFE_Class) QuadratureMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    GetQuadraturePoints1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints1
CHARACTER(*), PARAMETER :: myName = "GetQuadraturePoints1"
INTEGER(I4B) :: order0(3), nips0(3), quadratureType0(3)
REAL(DFP) :: alpha0(3), beta0(3), lambda0(3)

IF (PRESENT(order) .AND. PRESENT(nips)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[WRONG ARGUMENTS] both nips and order cannot be present'//  &
    & ' either give nips (number of integration points) or '//  &
    & ' give order (order of integrand)')
  RETURN
END IF

SELECT CASE (SIZE(quadratureType))
CASE (1)
  quadratureType0 = quadratureType(1)
CASE (2)
  quadratureType0(1) = quadratureType(1)
  quadratureType0(2:) = quadratureType(2)
CASE (3)
  quadratureType0 = quadratureType
END SELECT

IF (PRESENT(alpha)) THEN
  SELECT CASE (SIZE(alpha))
  CASE (1)
    alpha0 = alpha(1)
  CASE (2)
    alpha0(1) = alpha(1)
    alpha0(2:) = alpha(2)
  CASE (3)
    alpha0 = alpha
  END SELECT
END IF

IF (PRESENT(beta)) THEN
  SELECT CASE (SIZE(beta))
  CASE (1)
    beta0 = beta(1)
  CASE (2)
    beta0(1) = beta(1)
    beta0(2:) = beta(2)
  CASE (3)
    beta0 = beta
  END SELECT
END IF

IF (PRESENT(lambda)) THEN
  SELECT CASE (SIZE(lambda))
  CASE (1)
    lambda0 = lambda(1)
  CASE (2)
    lambda0(1) = lambda(1)
    lambda0(2:) = lambda(2)
  CASE (3)
    lambda0 = lambda
  END SELECT
END IF

IF (PRESENT(order)) THEN

  SELECT CASE (SIZE(order))
  CASE (1)
    order0 = order(1)
  CASE (2)
    order0(1) = order(1)
    order0(2:) = order(2)
  CASE (3)
    order0 = order
  END SELECT

  CALL Initiate( &
    & obj=quad,  &
    & refelem=obj%refelem0,  &
    & p=order0(1),  &
    & q=order0(2),  &
    & r=order0(3),  &
    & quadratureType1=quadratureType0(1),  &
    & quadratureType2=quadratureType0(2),  &
    & quadratureType3=quadratureType0(3),  &
    & alpha1=alpha0(1), beta1=beta0(1), lambda1=lambda0(1), &
    & alpha2=alpha0(2), beta2=beta0(2), lambda2=lambda0(2), &
    & alpha3=alpha0(3), beta3=beta0(3), lambda3=lambda0(3) &
    & )
  RETURN
END IF

IF (PRESENT(nips)) THEN

  SELECT CASE (SIZE(nips))
  CASE (1)
    nips0 = nips(1)
  CASE (2)
    nips0(1) = nips(1)
    nips0(2:) = nips(2)
  CASE (3)
    nips0 = nips
  END SELECT

  CALL Initiate( &
    & obj=quad,  &
    & refelem=obj%refelem0,  &
    & nipsx=nips0(1:1),  &
    & nipsy=nips0(2:2),  &
    & nipsz=nips0(3:3),  &
    & quadratureType1=quadratureType0(1),  &
    & quadratureType2=quadratureType0(2),  &
    & quadratureType3=quadratureType0(3),  &
    & alpha1=alpha0(1), beta1=beta0(1), lambda1=lambda0(1), &
    & alpha2=alpha0(2), beta2=beta0(2), lambda2=lambda0(2), &
    & alpha3=alpha0(3), beta3=beta0(3), lambda3=lambda0(3) &
    & )
  RETURN
END IF
END PROCEDURE obj_GetQuadraturePoints1

END SUBMODULE QuadratureMethods
