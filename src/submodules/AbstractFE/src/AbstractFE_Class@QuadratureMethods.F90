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
USE QuadraturePoint_Method, ONLY: Initiate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints1
CALL Initiate(obj=quad, elemType=obj%elemType, domainName=obj%refelemDomain, &
              order=order, quadratureType=quadratureType, alpha=alpha, &
              beta=beta, lambda=lambda, xij=obj%refelemCoord(1:obj%xidim, :))
END PROCEDURE obj_GetQuadraturePoints1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints2
CALL Initiate(obj=quad, elemType=obj%elemType, domainName=obj%refelemDomain, &
              p=p, q=q, r=r, quadratureType1=quadratureType1, &
           quadratureType2=quadratureType2, quadratureType3=quadratureType3, &
              alpha1=alpha1, alpha2=alpha2, alpha3=alpha3, &
              beta1=beta1, beta2=beta2, beta3=beta3, &
              lambda1=lambda1, lambda2=lambda2, lambda3=lambda3, &
              xij=obj%refelemCoord(1:obj%xidim, :))
END PROCEDURE obj_GetQuadraturePoints2

!----------------------------------------------------------------------------
!                                                    GetQuadraturePoints1
!----------------------------------------------------------------------------

END SUBMODULE QuadratureMethods
