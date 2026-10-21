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

MODULE VStfemAlgorithm2_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: math => TypeMathOpt
USE AbstractStfemAlgorithm2_Class, ONLY: AbstractStfemAlgorithm2_
IMPLICIT NONE
PRIVATE

PUBLIC :: VStfemAlgorithm2_

!----------------------------------------------------------------------------
!                                                           VStfemAlgorithm2_
!----------------------------------------------------------------------------

!> author: Vikas Sharma
! date: 2025-12-12
! summary:  Displacement based time discontinuous Galerkin algorithm
!
!# VStfemAlgorithm2_
!

TYPE, EXTENDS(AbstractStfemAlgorithm2_) :: VStfemAlgorithm2_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
END TYPE VStfemAlgorithm2_

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate VStfemAlgorithm2
!
! Initiate VStfemAlgorithm2. Default value of scalingOpt is "M"

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, elemsd, facetElemsd, scalingOpt)
    CLASS(VStfemAlgorithm2_), INTENT(INOUT) :: obj
    TYPE(ElemShapeData_), INTENT(IN) :: elemsd, facetElemsd
    CHARACTER(1), OPTIONAL, INTENT(IN) :: scalingOpt
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VStfemAlgorithm2_Class
