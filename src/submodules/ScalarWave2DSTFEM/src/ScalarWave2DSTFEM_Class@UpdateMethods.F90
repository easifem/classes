! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(ScalarWave2DSTFEM_Class) UpdateMethods

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Update
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Update
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Update()"
#endif

INTEGER(I4B) :: nnt, ii
REAL(DFP) :: scale, dt, dts

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

dt = obj%algoParam%elemLength(obj%algoParam%currentTimeStep)
dts = dt * dt
obj%algoParam%currentTime = obj%algoParam%currentTime + dt
obj%algoParam%currentTimeStep = obj%algoParam%currentTimeStep + 1

nnt = obj%algoParam%nnt

CALL obj%a0%Set(VALUE=zero)
CALL obj%v0%Set(VALUE=zero)
CALL obj%u0%SCAL(obj%algoParam%at_right)

DO ii = 1, nnt
  CALL obj%sol%Get(VALUE=obj%tmpVecs(ii)%ptr, timeCompo=ii)
END DO

DO ii = 1, nnt
  scale = obj%algoParam%timeShapeFuncGradBndy(ii, 2)
  CALL obj%a0%AXPY(x=obj%tmpVecs(ii)%ptr, scale=scale)

  scale = obj%algoParam%timeShapeFuncBndy(ii, 2)
  CALL obj%v0%AXPY(x=obj%tmpVecs(ii)%ptr, scale=scale)

  scale = obj%algoParam%bt_right(ii) * dt
  CALL obj%u0%AXPY(x=obj%tmpVecs(ii)%ptr, scale=scale)
END DO

DO ii = 1, nnt
  CALL obj%forceVecs(ii)%ptr%Set(VALUE=zero)
END DO

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_Update

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE UpdateMethods
