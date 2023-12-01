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

SUBMODULE(LinearPoroElasticModel_Class) SetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE lpem_SetParam
IF (PRESENT(elasticityType)) obj%elasticityType = elasticityType
IF (PRESENT(nu)) obj%nu = nu
IF (PRESENT(G)) obj%G = G
IF (PRESENT(youngsModulus)) obj%E = youngsModulus
IF (PRESENT(lambda)) obj%lambda = lambda
IF (PRESENT(C)) obj%C = C
IF (PRESENT(invC)) obj%invC = invC
END PROCEDURE lpem_SetParam

END SUBMODULE SetMethods
