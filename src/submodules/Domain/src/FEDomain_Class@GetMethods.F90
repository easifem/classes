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

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This submodule contains methods for domain object

SUBMODULE(FEDomain_Class) GetMethods
USE ReallocateUtility
USE InputUtility
USE BoundingBox_Method
USE F95_BLAS, ONLY: Copy
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             IsNodePresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsNodePresent
ans = .TRUE.
IF (globalNode .GT. obj%maxNptrs .OR. globalNode .LT. obj%minNptrs) THEN
  ans = .FALSE.
  RETURN
END IF

SELECT CASE (obj%nsd)
CASE (0)
  ans = obj%meshPoint%IsNodePresent(globalNode)
CASE (1)
  ans = obj%meshCurve%IsNodePresent(globalNode)
CASE (2)
  ans = obj%meshSurface%IsNodePresent(globalNode)
CASE (3)
  ans = obj%meshVolume%IsNodePresent(globalNode)
END SELECT
END PROCEDURE obj_IsNodePresent

!----------------------------------------------------------------------------
!                                                          isElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsElementPresent
INTEGER(I4B) :: dim0

dim0 = Input(default=obj%nsd, option=dim)
SELECT CASE (dim0)
CASE (3)
  ans = obj%meshVolume%IsElementPresent(globalElement=globalElement)
CASE (2)
  ans = obj%meshSurface%IsElementPresent(globalElement=globalElement)
CASE (1)
  ans = obj%meshCurve%IsElementPresent(globalElement=globalElement)
CASE (0)
  ans = obj%meshPoint%IsElementPresent(globalElement=globalElement)
END SELECT

END PROCEDURE obj_IsElementPresent

!----------------------------------------------------------------------------
!                                                          getConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
INTEGER(I4B) :: dim0

dim0 = Input(default=obj%nsd, option=dim)

SELECT CASE (dim0)
CASE (3)
  ans = obj%meshVolume%GetConnectivity(globalElement=globalElement)
CASE (2)
  ans = obj%meshSurface%GetConnectivity(globalElement=globalElement)
CASE (1)
  ans = obj%meshCurve%GetConnectivity(globalElement=globalElement)
CASE (0)
  ans = obj%meshPoint%GetConnectivity(globalElement=globalElement)
END SELECT

END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements1
SELECT CASE (obj%nsd)
CASE (3)
  ans = obj%meshVolume%GetNodeToElements(globalNode=globalNode)
CASE (2)
  ans = obj%meshSurface%GetNodeToElements(globalNode=globalNode)
CASE (1)
  ans = obj%meshCurve%GetNodeToElements(globalNode=globalNode)
CASE (0)
  ans = obj%meshPoint%GetNodeToElements(globalNode=globalNode)
END SELECT
END PROCEDURE obj_GetNodeToElements1

!----------------------------------------------------------------------------
!                                                         getNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeToElements2
SELECT CASE (obj%nsd)
CASE (3)
  ans = obj%meshVolume%GetNodeToElements(globalNode=globalNode)
CASE (2)
  ans = obj%meshSurface%GetNodeToElements(globalNode=globalNode)
CASE (1)
  ans = obj%meshCurve%GetNodeToElements(globalNode=globalNode)
CASE (0)
  ans = obj%meshPoint%GetNodeToElements(globalNode=globalNode)
END SELECT
END PROCEDURE obj_GetNodeToElements2
MODULE PROCEDURE obj_GetNptrs
SELECT CASE (dim)
CASE (3)
  ans = obj%meshVolume%GetNptrs()
CASE (2)
  ans = obj%meshSurface%GetNptrs()
CASE (1)
  ans = obj%meshCurve%GetNptrs()
CASE (0)
  ans = obj%meshPoint%GetNptrs()
END SELECT
END PROCEDURE obj_GetNptrs

!----------------------------------------------------------------------------
!                                                                   GetNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNptrs_
SELECT CASE (dim)
CASE (3)
  CALL obj%meshVolume%GetNptrs_(nptrs=nptrs)
CASE (2)
  CALL obj%meshSurface%GetNptrs_(nptrs=nptrs)
CASE (1)
  CALL obj%meshCurve%GetNptrs_(nptrs=nptrs)
CASE (0)
  CALL obj%meshPoint%GetNptrs_(nptrs=nptrs)
END SELECT
END PROCEDURE obj_GetNptrs_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
