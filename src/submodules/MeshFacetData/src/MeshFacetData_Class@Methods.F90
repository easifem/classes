! This program is a part of EASIFEM library
! Copyright (C) (Since 2000)  Vikas Sharma, Ph.D
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
SUBMODULE(MeshFacetData_Class) Methods
USE ReallocateUtility
USE Display_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate1
IF (ALLOCATED(obj%masterCellNumber)) DEALLOCATE (obj%masterCellNumber)
IF (ALLOCATED(obj%slaveCellNumber)) DEALLOCATE (obj%slaveCellNumber)
IF (ALLOCATED(obj%masterLocalFacetID)) DEALLOCATE (obj%masterLocalFacetID)
IF (ALLOCATED(obj%slaveLocalFacetID)) DEALLOCATE (obj%slaveLocalFacetID)
END PROCEDURE obj_Deallocate1

!----------------------------------------------------------------------------
!                                                   MeshFacetDataDeallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate2
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

isok = .NOT. ALLOCATED(obj)
IF (isok) RETURN

tsize = SIZE(obj)

DO ii = 1, tsize
  CALL obj(ii)%DEALLOCATE()
END DO

DEALLOCATE (obj)

END PROCEDURE obj_Deallocate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
CALL Reallocate(obj%masterCellNumber, n)
CALL Reallocate(obj%slaveCellNumber, n)
CALL Reallocate(obj%masterLocalFacetID, n)
CALL Reallocate(obj%slaveLocalFacetID, n)
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                isInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isInitiated
IF (ALLOCATED(obj%masterCellNumber)) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE obj_isInitiated

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
IF (ALLOCATED(obj%masterCellNumber)) THEN
  ans = SIZE(obj%masterCellNumber)
ELSE
  ans = 0
END IF
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: abool

CALL Display(msg, unitno=unitno)

CALL Display("elementType: BOUNDARY_ELEMENT", unitno=unitno)

CALL Display(obj%masterMesh, "masterMesh: ", unitno=unitno)

CALL Display(obj%slaveMesh, "slaveMesh: ", unitno=unitno)

abool = ALLOCATED(obj%masterCellNumber)
CALL Display(abool, "masterCellNumber Allocated: ", unitNo=unitNo)

IF (abool) THEN
  CALL Display(obj%masterCellNumber, msg="masterCellNumber: ", &
    & unitno=unitno)
END IF

abool = ALLOCATED(obj%masterlocalFacetID)
CALL Display(abool, "masterlocalFacetID Allocated: ", unitNo=unitNo)

IF (abool) THEN
  CALL Display(obj%masterlocalFacetID, msg="masterlocalFacetID: ", &
    & unitno=unitno)
END IF

abool = ALLOCATED(obj%slaveCellNumber)
CALL Display(abool, "slaveCellNumber Allocated: ", unitNo=unitNo)

IF (abool) THEN
  CALL Display(obj%slaveCellNumber, msg="slaveCellNumber: ", &
    & unitno=unitno)
END IF

abool = ALLOCATED(obj%slavelocalFacetID)
IF (abool) THEN
  CALL Display(obj%slavelocalFacetID, msg="slavelocalFacetID: ", &
    & unitno=unitno)
END IF

END PROCEDURE obj_Display

END SUBMODULE Methods
