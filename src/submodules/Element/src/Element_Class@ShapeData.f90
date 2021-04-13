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

SUBMODULE( Element_Class ) ShapeData
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 H1Lagrange
!----------------------------------------------------------------------------

MODULE PROCEDURE get_elemsd_H1_Lagrange
  CALL initiate( Obj = ElemSD, Quad = Quad, &
    & RefElem = Obj%RefElem, &
    & ContinuityType= typeH1, &
    & InterpolType = TypeLagrangeInterpolation )
  CALL setValue( Obj = ElemSD, Val = XiJ, N =ElemSD%N, dNdXi=ElemSD%dNdXi )
END PROCEDURE get_elemsd_H1_Lagrange
END SUBMODULE ShapeData