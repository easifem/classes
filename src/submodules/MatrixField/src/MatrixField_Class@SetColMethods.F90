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

SUBMODULE(MatrixField_Class) SetColMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn1
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "mField_setColumn1"
!
! main
!
IF (PRESENT(scalarVal)) THEN
  IF (obj%isRectangle) THEN
    CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domains(2)%ptr%getLocalNodeNumber(globalNode), &
    & idof=idof, &
    & VALUE=scalarVal)
  ELSE
    CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
    & idof=idof, &
    & VALUE=scalarVal)
  END IF
END IF
!
!
!
IF (PRESENT(vecVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    CALL setColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & idof=idof, &
      & VALUE=vecVal)
  END IF
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    realvec => nodeFieldVal%getPointer()
    CALL setColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & idof=idof, &
      & VALUE=realvec)
  END IF
END IF
!
realvec => NULL()
!
END PROCEDURE mField_setColumn1

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn2
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "mField_setColumn2"
!
! main
!
IF (PRESENT(scalarVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & idof=idof, &
    & VALUE=scalarVal)
  END IF
END IF
!
!
!
IF (PRESENT(vecVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & idof=idof, &
    & VALUE=vecVal)
  END IF
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    realvec => nodeFieldVal%getPointer()
    CALL setColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & idof=idof, &
      & VALUE=realvec)
  END IF
END IF
!
realvec => NULL()
!
END PROCEDURE mField_setColumn2

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn3
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "mField_setColumn3"
!
! main
!
IF (PRESENT(scalarVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    CALL setColumn( &
   & obj=obj%mat, &
   & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
   & ivar=ivar, &
   & spacecompo=spacecompo, &
   & timecompo=timecompo, &
   & VALUE=scalarVal)
  END IF
END IF
!
!
!
IF (PRESENT(vecVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    CALL setColumn( &
   & obj=obj%mat, &
   & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
   & ivar=ivar, &
   & spacecompo=spacecompo, &
   & timecompo=timecompo, &
   & VALUE=vecVal)
  END IF
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    realvec => nodeFieldVal%getPointer()
    CALL setColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec)
  END IF
END IF
!
realvec => NULL()
!
END PROCEDURE mField_setColumn3

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn4
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "mField_setColumn4"
!
! main
!
IF (PRESENT(scalarVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    CALL setColumn( &
   & obj=obj%mat, &
   & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
   & ivar=ivar, &
   & spacecompo=spacecompo, &
   & timecompo=timecompo, &
   & VALUE=scalarVal)
  END IF
END IF
!
!
!
IF (PRESENT(vecVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    CALL setColumn( &
   & obj=obj%mat, &
   & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
   & ivar=ivar, &
   & spacecompo=spacecompo, &
   & timecompo=timecompo, &
   & VALUE=vecVal)
  END IF
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    realvec => nodeFieldVal%getPointer()
    CALL setColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec)
  END IF
END IF
!
realvec => NULL()
!
END PROCEDURE mField_setColumn4

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn5
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "mField_setColumn5"
!
! main
!
IF (PRESENT(scalarVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    CALL setColumn( &
   & obj=obj%mat, &
   & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
   & ivar=ivar, &
   & spacecompo=spacecompo, &
   & timecompo=timecompo, &
   & VALUE=scalarVal)
  END IF
END IF
!
IF (PRESENT(vecVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    CALL setColumn( &
   & obj=obj%mat, &
   & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
   & ivar=ivar, &
   & spacecompo=spacecompo, &
   & timecompo=timecompo, &
   & VALUE=vecVal)
  END IF
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    realvec => nodeFieldVal%getPointer()
    CALL setColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec)
  END IF
END IF
!
realvec => NULL()
!
END PROCEDURE mField_setColumn5

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn6
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "mField_setColumn6"
!
! main
!
IF (PRESENT(scalarVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=scalarVal)
  END IF
END IF
!
IF (PRESENT(vecVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    CALL setColumn( &
   & obj=obj%mat, &
   & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
   & ivar=ivar, &
   & spacecompo=spacecompo, &
   & timecompo=timecompo, &
   & VALUE=vecVal)
  END IF
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    realvec => nodeFieldVal%getPointer()
    CALL setColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec)
  END IF
END IF
!
realvec => NULL()
!
END PROCEDURE mField_setColumn6

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn7
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "mField_setColumn7"
!
! main
!
IF (PRESENT(scalarVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    CALL setColumn( &
   & obj=obj%mat, &
   & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
   & ivar=ivar, &
   & spacecompo=spacecompo, &
   & timecompo=timecompo, &
   & VALUE=scalarVal)
  END IF
END IF
!
IF (PRESENT(vecVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    CALL setColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=vecVal)
  END IF
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'This routine not implemented for rectangle matrix')
  ELSE
    realvec => nodeFieldVal%getPointer()
    CALL setColumn( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec)
  END IF
END IF
!
realvec => NULL()
!
END PROCEDURE mField_setColumn7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetColMethods
