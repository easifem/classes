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

SUBMODULE(AbstractFE_Class) GetMethods
USE BaseMethod
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_GetLocalElemShapeData
CHARACTER(*), PARAMETER :: myName = "fe_GetLocalElemShapeData()"
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[NOT INITIATED] It seems AbstractFE_::obj is not initiated.')
  RETURN
END IF

SELECT TYPE (baseContinuity => obj%baseContinuity)
CLASS IS (H1_)
  CALL obj%GetLocalElemShapeData_H1(elemsd=elemsd, quad=quad)
CLASS is (HDIV_)
  CALL obj%GetLocalElemShapeData_HDiv(elemsd=elemsd, quad=quad)
CLASS is (HCURL_)
  CALL obj%GetLocalElemShapeData_HCurl(elemsd=elemsd, quad=quad)
CLASS IS (DG_)
  CALL obj%GetLocalElemShapeData_DG(elemsd=elemsd, quad=quad)
CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[NO CASE FOUND] No case found for type of '//  &
    & '  AbstractFE_::obj%baseContinuity')
  RETURN
END SELECT
END PROCEDURE fe_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                                    GetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_GetGlobalElemShapeData
CHARACTER(*), PARAMETER :: myName = "fe_GetGlobalElemShapeData()"
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[NOT INITIATED] It seems AbstractFE_::obj is not initiated.')
  RETURN
END IF

SELECT TYPE (baseContinuity => obj%baseContinuity)
CLASS IS (H1_)
  CALL obj%GetGlobalElemShapeData_H1(elemsd=elemsd, xij=xij,  &
    & geoElemsd=geoElemsd)
CLASS is (HDIV_)
  CALL obj%GetGlobalElemShapeData_HDiv(elemsd=elemsd, xij=xij,  &
    & geoElemsd=geoElemsd)
CLASS is (HCURL_)
  CALL obj%GetGlobalElemShapeData_HCurl(elemsd=elemsd, xij=xij,  &
    & geoElemsd=geoElemsd)
CLASS IS (DG_)
  CALL obj%GetGlobalElemShapeData_DG(elemsd=elemsd, xij=xij,  &
    & geoElemsd=geoElemsd)
CLASS DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[NO CASE FOUND] No case found for type of  &
    & AbstractFE_::obj%baseContinuity')
END SELECT
END PROCEDURE fe_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!                                                                GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_GetParam
CHARACTER(*), PARAMETER :: myName = "fe_GetParam()"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & '[WORK IN PROGRESS]')
END PROCEDURE fe_GetParam

!----------------------------------------------------------------------------
!                                                             GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_GetPrefix
CHARACTER(*), PARAMETER :: myName = "fe_GetPrefix()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE fe_GetPrefix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
