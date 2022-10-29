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

SUBMODULE(LagrangeFE_Class) Methods
USE BaseMethod
USE RefElementFactory
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Initiate
INTEGER(I4B), PARAMETER :: dofType(4) = FE_DOF_POINT_EVAL
INTEGER(I4B) :: nsd
REAL(DFP), ALLOCATABLE :: xij(:, :)
!!
!! initiate reference element
!!
CALL obj%Deallocate()
obj%refelem => RefElement_Pointer(elemType=elemType)
nsd = XiDimension(elemType)
CALL obj%refelem%Initiate(nsd=nsd)
!!
CALL obj%SetParam( &
  & nsd=nsd, &
  & order=order, &
  & ipType=ipType, &
  & feType=H1_LAGRANGE, &
  & dofType=dofType, &
  & transformType=FE_TRANSFORM_IDENTITY &
  & )
!!
!! generate lattice point
!!
xij = obj%refelem%GetInterpolationPoint(order=order, ipType=ipType, layout="VEFC")
!!
!! Generate shape functions
!!
SELECT CASE (nsd)
CASE (1_I4B)
  obj%oneD = LagrangeSpace1D( &
    & x=xij(1, :), &
    & order=order, &
    & varname="x")
CASE (2_I4B)
  obj%twoD = LagrangeSpace2D( &
    & x=xij, &
    & order=order, &
    & varname1="x", &
    & varname2="y", &
    & elemType=elemType)
CASE (3_I4B)
  obj%threeD = LagrangeSpace3D( &
    & x=xij, &
    & order=order, &
    & varname1="x", &
    & varname2="y", &
    & varname3="z", &
    & elemType=elemType)
END SELECT
END PROCEDURE fe_Initiate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Deallocate
CALL AbstractFEDeallocate(obj)
CALL obj%oneD%Deallocate()
CALL obj%twoD%Deallocate()
CALL obj%threeD%Deallocate()
END PROCEDURE fe_Deallocate

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Final
CALL obj%Deallocate()
END PROCEDURE fe_Final

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Display
INTEGER(I4B) :: nsd
CALL AbstractFEDisplay(obj=obj, msg=msg, unitno=unitno)
nsd = obj%refelem%GetNSD()
SELECT CASE (nsd)
CASE (1)
  CALL obj%oneD%Display(msg="LagrangeShapeFunctions=", unitno=unitno)
CASE (2)
  CALL obj%twoD%Display(msg="LagrangeShapeFunctions=", unitno=unitno)
CASE (3)
  CALL obj%threeD%Display(msg="LagrangeShapeFunctions=", unitno=unitno)
END SELECT
END PROCEDURE fe_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
