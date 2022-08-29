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

SUBMODULE(AbstractFE_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Deallocate
IF (ASSOCIATED(obj%refelem)) THEN
  CALL obj%refelem%Deallocate()
  DEALLOCATE (obj%refelem)
  obj%refelem => NULL()
END IF
obj%nsd = 0
obj%order = 0
obj%feType = 0
obj%ipType = 0
obj%dofType = 0
obj%transformType = 0
END PROCEDURE fe_Deallocate

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Display
IF (ASSOCIATED(obj%refelem)) THEN
  CALL obj%refelem%Display(msg="ReferenceElement=", unitno=unitno, &
    & notFull=.TRUE.)
END IF
CALL Display(obj%nsd, msg="nsd=", unitno=unitno)
CALL Display(obj%order, msg="order=", unitno=unitno)
CALL Display(obj%feType, msg="feType=", unitno=unitno)
CALL Display(obj%ipType, msg="ipType=", unitno=unitno)
CALL Display(obj%dofType, msg="dofType=", unitno=unitno)
CALL Display(obj%transformType, msg="transformType=", unitno=unitno)
END PROCEDURE fe_Display

!----------------------------------------------------------------------------
!                                                                  SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_SetParam
IF (PRESENT(order)) obj%order = order
IF (PRESENT(order)) obj%nsd = nsd
IF (PRESENT(feType)) obj%feType = feType
IF (PRESENT(ipType)) obj%ipType = ipType
IF (PRESENT(dofType)) obj%dofType = dofType
IF (PRESENT(transformType)) obj%transformType = transformType
END PROCEDURE fe_SetParam

END SUBMODULE Methods
