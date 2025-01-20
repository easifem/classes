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

SUBMODULE(BlockMatrixField_Class) SetColMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn1
CHARACTER(*), PARAMETER :: myName = "obj_SetColumn1"
CALL e%raiseError(modName//'::'//myName//" - "// &
& 'This routine is not callable for BlockMatrixField')
END PROCEDURE obj_SetColumn1

!----------------------------------------------------------------------------
!                                                                 SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn2
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(scalarVal)) &
  & CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & idof=idof, &
  & VALUE=scalarVal)
IF (PRESENT(vecVal)) &
  & CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & idof=idof, &
  & VALUE=vecVal)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & idof=idof, &
  & VALUE=realvec)
END IF
realvec => NULL()
END PROCEDURE obj_SetColumn2

!----------------------------------------------------------------------------
!                                                                 SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn3
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(scalarVal)) &
  & CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=scalarVal)
IF (PRESENT(vecVal)) &
  & CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=vecVal)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=realvec)
END IF
realvec => NULL()
END PROCEDURE obj_SetColumn3

!----------------------------------------------------------------------------
!                                                                    SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn4
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(scalarVal)) &
  & CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=scalarVal)
IF (PRESENT(vecVal)) &
  & CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=vecVal)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=realvec)
END IF
realvec => NULL()
END PROCEDURE obj_SetColumn4

!----------------------------------------------------------------------------
!                                                                    SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn5
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(scalarVal)) &
  & CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=scalarVal)
IF (PRESENT(vecVal)) &
  & CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=vecVal)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=realvec)
END IF
realvec => NULL()
END PROCEDURE obj_SetColumn5

!----------------------------------------------------------------------------
!                                                                    SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn6
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(scalarVal)) &
  & CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=scalarVal)
IF (PRESENT(vecVal)) &
  & CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=vecVal)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=realvec)
END IF
realvec => NULL()
END PROCEDURE obj_SetColumn6

!----------------------------------------------------------------------------
!                                                                    SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn7
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(scalarVal)) &
  & CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=scalarVal)
IF (PRESENT(vecVal)) &
  & CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=vecVal)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL SetColumn( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=realvec)
END IF
realvec => NULL()
END PROCEDURE obj_SetColumn7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetColMethods
