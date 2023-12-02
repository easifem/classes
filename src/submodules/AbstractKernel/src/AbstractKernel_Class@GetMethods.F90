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

SUBMODULE(AbstractKernel_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE ak_GetPrefix
CHARACTER(*), PARAMETER :: myName = "ak_GetPrefix"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This method should be implemented by the '//  &
  & ' children classes of AbstractKernel_')
END PROCEDURE ak_GetPrefix

!----------------------------------------------------------------------------
!                                            KernelGetCoordinateSystemName
!----------------------------------------------------------------------------

MODULE PROCEDURE KernelGetCoordinateSystemName
SELECT CASE (uid)
CASE (KERNEL_1D_H)
  ans = "1D_H"
CASE (KERNEL_1D_V)
  ans = "1D_V"
CASE (KERNEL_2D)
  ans = "2D"
CASE (KERNEL_2D_AXISYM)
  ans = "AXISYM"
CASE (KERNEL_PLANE_STRAIN)
  ans = "PLANE_STRAIN"
CASE (KERNEL_PLANE_STRESS)
  ans = "PLANE_STRESS"
CASE (KERNEL_3D)
  ans = "3D"
CASE (KERNEL_CARTESIAN)
  ans = "CARTESTIAN"
CASE (KERNEL_CYLINDRICAL)
  ans = "CYLINDRICAL"
CASE (KERNEL_SPHERICAL)
  ans = "SPHERICAL"
END SELECT
END PROCEDURE KernelGetCoordinateSystemName

!----------------------------------------------------------------------------
!                                            KernelGetCoordinateSystemID
!----------------------------------------------------------------------------

MODULE PROCEDURE KernelGetCoordinateSystemID
SELECT CASE (TRIM(name))
CASE ("1D_H")
  ans = KERNEL_1D_H
CASE ("1D_V")
  ans = KERNEL_1D_V
CASE ("2D")
  ans = KERNEL_2D
CASE ("AXISYM")
  ans = KERNEL_2D_AXISYM
CASE ("PLANE_STRAIN")
  ans = KERNEL_PLANE_STRAIN
CASE ("PLANE_STRESS")
  ans = KERNEL_PLANE_STRESS
CASE ("3D")
  ans = KERNEL_3D
CASE ("CARTESIAN")
  ans = KERNEL_CARTESIAN
CASE ("CYLINDRICAL")
  ans = KERNEL_CYLINDRICAL
CASE ("SPHERICAL")
  ans = KERNEL_SPHERICAL
END SELECT
END PROCEDURE KernelGetCoordinateSystemID

!----------------------------------------------------------------------------
!                                                     KernelGetNSDFromID
!----------------------------------------------------------------------------

MODULE PROCEDURE KernelGetNSDFromID
SELECT CASE (uid)
CASE (KERNEL_1D_H, KERNEL_1D_V)
  ans = 1
CASE (KERNEL_2D, KERNEL_2D_AXISYM, KERNEL_PLANE_STRAIN, &
  & KERNEL_PLANE_STRESS)
  ans = 2
CASE DEFAULT
  ans = 3
END SELECT
END PROCEDURE KernelGetNSDFromID

!----------------------------------------------------------------------------
!                                                   KernelGetNSDFromName
!----------------------------------------------------------------------------

MODULE PROCEDURE KernelGetNSDFromName
SELECT CASE (TRIM(name))
CASE ("1D_H", "1D_V")
  ans = 1
CASE ("2D", "AXISYM", "PLANE_STRAIN", "PLANE_STRESS")
  ans = 2
CASE DEFAULT
  ans = 3
END SELECT
END PROCEDURE KernelGetNSDFromName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
