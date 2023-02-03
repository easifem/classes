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

SUBMODULE(TxtFile_Class) ConstructorMethods
USE ISO_FORTRAN_ENV, ONLY: IOSTAT_EOR, IOSTAT_END
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_initiate
CHARACTER(*), PARAMETER :: myName = 'txt_initiate'
IF (PRESENT(access)) CALL e % raiseDebug(modName//'::'//myName// &
  & ' - Optional input "ACCESS" is being ignored. Value is "SEQUENTIAL".')
IF (PRESENT(form)) CALL e % raiseDebug(modName//'::'//myName// &
  & ' - Optional input "FORM" is being ignored. Value is "FORMATTED".')
IF (PRESENT(pad)) CALL e % raiseDebug(modName//'::'//myName// &
  & ' - Optional input "PAD" is being ignored. Value is "YES".')
IF (PRESENT(position)) CALL e % raiseDebug(modName//'::'//myName// &
  & ' - Optional input "POSITION" is being ignored. Value is "REWIND".')
IF (PRESENT(recl)) CALL e % raiseDebug(modName//'::'//myName// &
  & ' - Optional input "RECL" is being ignored. File is "SEQUENTIAL".')
  !!
  !! Initialize the input file
  !!
CALL FortranFileInitiate( &
  & obj=obj, &
  & unit=unit, &
  & filename=filename, &
  & status=status, &
  & access='SEQUENTIAL', &
  & form='FORMATTED', &
  & position='ASIS', &
  & action=action, &
  & comment=comment, &
  & separator=separator, &
  & delimiter=delimiter)
!
END PROCEDURE txt_initiate

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_Deallocate
LOGICAL(LGT) :: bool
obj % echounit = -1
obj % echostat = .FALSE.
bool = .FALSE.
IF (PRESENT(Delete)) bool = Delete
CALL FortranFileDeallocate(obj, bool)
END PROCEDURE txt_Deallocate

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_Final
CALL obj % DEALLOCATE()
END PROCEDURE txt_Final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
