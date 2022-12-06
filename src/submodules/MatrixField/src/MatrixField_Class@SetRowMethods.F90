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

SUBMODULE(MatrixField_Class) SetRowMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setRow1
REAL(DFP), POINTER :: realvec(:)
!!
IF (PRESENT(scalarVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & idof=idof, &
  & value=scalarVal)
!!
IF (PRESENT(vecVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & idof=idof, &
  & value=vecVal)
!!
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & idof=idof, &
  & value=realvec)
END IF
!!
realvec => NULL()
END PROCEDURE mField_setRow1

!----------------------------------------------------------------------------
!                                                                 setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setRow2
REAL(DFP), POINTER :: realvec(:)
!!
IF (PRESENT(scalarVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & idof=idof, &
  & value=scalarVal)
!!
IF (PRESENT(vecVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & idof=idof, &
  & value=vecVal)
!!
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & idof=idof, &
  & value=realvec)
END IF
!!
realvec => NULL()
END PROCEDURE mField_setRow2

!----------------------------------------------------------------------------
!                                                                 setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setRow3
REAL(DFP), POINTER :: realvec(:)
!!
IF (PRESENT(scalarVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=scalarVal)
!!
IF (PRESENT(vecVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=vecVal)
!!
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=realvec)
END IF
!!
realvec => NULL()
END PROCEDURE mField_setRow3

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setRow4
REAL(DFP), POINTER :: realvec(:)
!!
IF (PRESENT(scalarVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=scalarVal)
!!
IF (PRESENT(vecVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=vecVal)
!!
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=realvec)
END IF
!!
realvec => NULL()
END PROCEDURE mField_setRow4

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setRow5
REAL(DFP), POINTER :: realvec(:)
!!
IF (PRESENT(scalarVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=scalarVal)
!!
IF (PRESENT(vecVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=vecVal)
!!
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=realvec)
END IF
!!
realvec => NULL()
END PROCEDURE mField_setRow5

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setRow6
REAL(DFP), POINTER :: realvec(:)
!!
IF (PRESENT(scalarVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=scalarVal)
!!
IF (PRESENT(vecVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=vecVal)
!!
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=realvec)
END IF
!!
realvec => NULL()
END PROCEDURE mField_setRow6

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setRow7
REAL(DFP), POINTER :: realvec(:)
!!
IF (PRESENT(scalarVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=scalarVal)
!!
IF (PRESENT(vecVal)) &
  & CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=vecVal)
!!
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%getPointer()
  CALL setRow( &
  & obj=obj%mat, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & value=realvec)
END IF
!!
realvec => NULL()
END PROCEDURE mField_setRow7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetRowMethods
