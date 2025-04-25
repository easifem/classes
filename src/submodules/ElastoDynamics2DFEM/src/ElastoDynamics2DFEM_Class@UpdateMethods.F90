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

SUBMODULE(ElastoDynamics2DFEM_Class) UpdateMethods

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

REAL(DFP) :: scale, dt, dts
LOGICAL(LGT) :: isok

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

dt = obj%timeStepSize(obj%currentTimeStep)
dts = dt * dt
obj%currentTime = obj%currentTime + dt
obj%currentTimeStep = obj%currentTimeStep + 1

CALL obj%force1%COPY(obj%force2)

!----------------------------------------------------------------------------
! Update displacement
!----------------------------------------------------------------------------

CALL obj%rhs%Set(VALUE=zero)

scale = obj%algoParam%dis(1)
isok = .NOT. obj%algoParam%dis_zero(1)
IF (isok) &
  CALL obj%rhs%AXPY(x=obj%u0, scale=scale)

scale = obj%algoParam%dis(2) * dt
isok = .NOT. obj%algoParam%dis_zero(2)
IF (isok) &
  CALL obj%rhs%AXPY(x=obj%v0, scale=scale)

scale = obj%algoParam%dis(3) * dts
isok = .NOT. obj%algoParam%dis_zero(3)
IF (isok) &
  CALL obj%rhs%AXPY(x=obj%a0, scale=scale)

scale = obj%algoParam%dis(4)
isok = .NOT. obj%algoParam%dis_zero(4)
IF (isok) &
  CALL obj%rhs%AXPY(x=obj%sol, scale=scale)

!----------------------------------------------------------------------------
! Update v0
!----------------------------------------------------------------------------

CALL obj%tmp1%Set(VALUE=zero)

scale = obj%algoParam%vel(1) / dt
isok = .NOT. obj%algoParam%vel_zero(1)
IF (isok) &
  CALL obj%tmp1%AXPY(x=obj%u0, scale=scale)

scale = obj%algoParam%vel(2)
isok = .NOT. obj%algoParam%vel_zero(2)
IF (isok) &
  CALL obj%tmp1%AXPY(x=obj%v0, scale=scale)

scale = obj%algoParam%vel(3) * dt
isok = .NOT. obj%algoParam%vel_zero(3)
IF (isok) &
  CALL obj%tmp1%AXPY(x=obj%a0, scale=scale)

scale = obj%algoParam%vel(4) / dt
isok = .NOT. obj%algoParam%vel_zero(4)
IF (isok) &
  CALL obj%tmp1%AXPY(x=obj%sol, scale=scale)

!----------------------------------------------------------------------------
! Update a0
!----------------------------------------------------------------------------

CALL obj%force2%Set(VALUE=zero)

scale = obj%algoParam%acc(1) / dts
isok = .NOT. obj%algoParam%acc_zero(1)
IF (isok) &
  CALL obj%force2%AXPY(x=obj%u0, scale=scale)

scale = obj%algoParam%acc(2) / dt
isok = .NOT. obj%algoParam%acc_zero(2)
IF (isok) &
  CALL obj%force2%AXPY(x=obj%v0, scale=scale)

scale = obj%algoParam%acc(3)
isok = .NOT. obj%algoParam%acc_zero(3)
IF (isok) &
  CALL obj%force2%AXPY(x=obj%a0, scale=scale)

scale = obj%algoParam%acc(4) / dts
isok = .NOT. obj%algoParam%acc_zero(4)
IF (isok) &
  CALL obj%force2%AXPY(x=obj%sol, scale=scale)

!----------------------------------------------------------------------------
! Update u0, v0, a0, and force1
!----------------------------------------------------------------------------

CALL obj%u0%Copy(obj%rhs)
CALL obj%v0%Copy(obj%tmp1)
CALL obj%a0%Copy(obj%force2)
CALL obj%force2%Set(VALUE=zero)
CALL obj%rhs%Set(VALUE=zero)

CALL obj%sol%Set(VALUE=zero)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_Update

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE UpdateMethods
