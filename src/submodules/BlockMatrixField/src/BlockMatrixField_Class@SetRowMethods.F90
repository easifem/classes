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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(BlockMatrixField_Class) SetRowMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow1
CHARACTER(*), PARAMETER :: myName = "obj_SetRow1"
CALL e%raiseError(modName//'::'//myName//" - "// &
& 'This routine is not callable for BlockMatrixField')
END PROCEDURE obj_SetRow1

!----------------------------------------------------------------------------
!                                                                 SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow2
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(scalarVal)) &
  & CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & idof=idof, &
  & VALUE=scalarVal)
IF (PRESENT(vecVal)) &
  & CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & idof=idof, &
  & VALUE=vecVal)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & idof=idof, &
  & VALUE=realvec)
END IF
realvec => NULL()
END PROCEDURE obj_SetRow2

!----------------------------------------------------------------------------
!                                                                 SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow3
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(scalarVal)) &
  & CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=scalarVal)
IF (PRESENT(vecVal)) &
  & CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=vecVal)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=realvec)
END IF
realvec => NULL()
END PROCEDURE obj_SetRow3

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow4
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(scalarVal)) &
  & CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=scalarVal)
IF (PRESENT(vecVal)) &
  & CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=vecVal)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=realvec)
END IF
realvec => NULL()
END PROCEDURE obj_SetRow4

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow5
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(scalarVal)) &
  & CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=scalarVal)
IF (PRESENT(vecVal)) &
  & CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=vecVal)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=realvec)
END IF
realvec => NULL()
END PROCEDURE obj_SetRow5

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow6
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(scalarVal)) &
  & CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=scalarVal)
IF (PRESENT(vecVal)) &
  & CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=vecVal)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=realvec)
END IF
realvec => NULL()
END PROCEDURE obj_SetRow6

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow7
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(scalarVal)) &
  & CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=scalarVal)
IF (PRESENT(vecVal)) &
  & CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=vecVal)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL SetRow( &
  & obj=obj%mat, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=realvec)
END IF
realvec => NULL()
END PROCEDURE obj_SetRow7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetRowMethods
