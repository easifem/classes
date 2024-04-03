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

SUBMODULE(PVDFile_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 PVDFile
!----------------------------------------------------------------------------

MODULE PROCEDURE InitiatePVDFile
CHARACTER(*), PARAMETER :: myName = "InitiatePVDFile()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

IF (obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: PVDFile is already initiated, '//  &
    & ' call Deallocate() first')
  RETURN
END IF

! obj%dataFormat = Input(option=dataFormat, default=PVD_BINARY_APPENDED)
! obj%spacing = 1
obj%attrNames = [String("part"), String("group"), String("timestep"), String("file")]

CALL obj%Initiate(filename=filename,  &
  & mode=Input(option=mode, default="NEW"))
CALL obj%OPEN()

CALL obj%WriteRootTag()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE InitiatePVDFile

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
CALL xmlFile_Deallocate(obj, delete)
obj%indent = 0
obj%attrNames = String("")
! obj%search = .FALSE.
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                                      Close
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Close
IF (obj%isOpen()) THEN
  obj%indent = 2
  CALL obj%WriteEndTag(name=String("Collection"))
  obj%indent = 0
  CALL obj%WriteEndTag(name=String('VTKFile'))
  CALL xmlFile_Close(obj)
END IF
END PROCEDURE obj_Close

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
