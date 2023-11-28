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

SUBMODULE(LinearPoroElasticModel_Class) GetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     lpem_GetElasticParam
!----------------------------------------------------------------------------

MODULE PROCEDURE lpem_GetElasticParam
IF (PRESENT(PoissonRatio)) PoissonRatio = obj%nu
IF (PRESENT(ShearModulus)) ShearModulus = obj%G
IF (PRESENT(YoungsModulus)) YoungsModulus = obj%E
IF (PRESENT(lambda)) lambda = obj%lambda
END PROCEDURE lpem_GetElasticParam

!----------------------------------------------------------------------------
!                                                               lpem_GetC
!----------------------------------------------------------------------------

MODULE PROCEDURE lpem_GetC
C = obj%C
END PROCEDURE lpem_GetC

!----------------------------------------------------------------------------
!                                                              lpem_GetInvC
!----------------------------------------------------------------------------

MODULE PROCEDURE lpem_GetInvC
InvC = obj%InvC
END PROCEDURE lpem_GetInvC

!----------------------------------------------------------------------------
!                                                         GetElasticityType
!----------------------------------------------------------------------------

MODULE PROCEDURE lpem_GetElasticityType
ans = obj%elasticityType
END PROCEDURE lpem_GetElasticityType

!----------------------------------------------------------------------------
!                                                               GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE lpem_GetParam
IF (PRESENT(elasticityType)) elasticityType = obj%elasticityType
IF (PRESENT(nu)) nu = obj%nu
IF (PRESENT(G)) G = obj%G
IF (PRESENT(youngsModulus)) youngsModulus = obj%E
IF (PRESENT(lambda)) lambda = obj%lambda
IF (PRESENT(C)) C = obj%C
IF (PRESENT(invC)) invC = obj%invC
END PROCEDURE lpem_GetParam

END SUBMODULE GetMethods
