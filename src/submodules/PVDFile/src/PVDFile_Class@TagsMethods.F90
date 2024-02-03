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

SUBMODULE(PVDFile_Class) TagsMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteRootTag
CHARACTER(*), PARAMETER :: myName = "PVDFile_WriteRootTag()"
TYPE(String) :: buffer
CHARACTER(100) :: ioerrmsg
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & ' [INTERNAL ERROR] :: PVDFile is not initiated!')
END IF

IF (.NOT. obj%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: PVDFile is not open')
END IF

IF (.NOT. obj%isWrite()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: PVDFile does not have write permission')
END IF

buffer = '<?xml version="1.0" encoding="UTF-8"?>'//CHAR_LF

IF (endian .EQ. endianL) THEN
  buffer = buffer//'<VTKFile type="Collection"'// &
  & ' version="1.0" byte_order="LittleEndian">'
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP] not implemented yet')
END IF

WRITE (obj%unitNo, "(a)", iostat=ierr, iomsg=ioerrmsg) &
  & TRIM(buffer%chars())//CHAR_LF

obj%indent = 2

CALL obj%WriteStartTag(name=String("Collection"))

obj%indent = 4

IF (ierr .NE. 0) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Error has occured while'//  &
    & 'writing header info in PVDFile iostat = '// &
    & TRIM(str(ierr, .TRUE.))//' error msg :: '// &
    & TRIM(ioerrmsg))
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_WriteRootTag

!----------------------------------------------------------------------------
!                                                             WriteDataSetTag
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteDataSetTag
CHARACTER(*), PARAMETER :: myName = "WriteDataSetTag"
INTEGER(I4B) :: part0
TYPE(String) :: attrValues(4)
CHARACTER(:), ALLOCATABLE :: group0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & ' [INTERNAL ERROR] :: PVDFile is not initiated!')
END IF

IF (.NOT. obj%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: PVDFile is not open')
END IF

IF (.NOT. obj%isWrite()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: PVDFile does not have write permission')
END IF

part0 = input(option=part, default=0)
group0 = input(option=group, default="")

! IF (obj%search) THEN
!   CALL e%RaiseWarning(modName//'::'//myName//' - '// &
!     & '[TODO] searching is not implemented yet')
! END IF

attrValues(1) = '"'//tostring(part0)//'"'
attrValues(2) = '"'//String(group0)//'"'
attrValues(3) = '"'//tostring(currentTime)//'"'
attrValues(4) = '"'//String(filename)//'"'

CALL obj%WriteSelfClosingTag(name=String("DataSet"), attrNames=obj%attrNames,  &
& attrValues=attrValues)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_WriteDataSetTag

!----------------------------------------------------------------------------
!                                                             WriteStartTag
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteStartTag
TYPE(XMLTag_) :: tag
CALL tag%set(name=name, attrNames=attrNames, attrValues=attrValues, &
  & Indent=obj%Indent)
CALL tag%WRITE(unitNo=obj%unitNo, isIndented=.TRUE., endRecord=CHAR_LF, &
  & onlyStart=.TRUE.)
obj%Indent = obj%Indent + 2
CALL tag%DEALLOCATE()
END PROCEDURE obj_WriteStartTag

!----------------------------------------------------------------------------
!                                                              WriteEndTag
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteEndTag
TYPE(XMLTag_) :: tag
CALL tag%set(name=name, indent=obj%indent)
CALL tag%WRITE(unitNo=obj%unitNo, isIndented=.TRUE., endRecord=CHAR_LF, &
  & onlyEnd=.TRUE.)
CALL tag%DEALLOCATE()
END PROCEDURE obj_WriteEndTag

!----------------------------------------------------------------------------
!                                                       WriteSelfClosingTag
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteSelfClosingTag
TYPE(XMLTag_) :: tag
CALL tag%set(name=name, attrNames=attrNames, attrValues=attrValues, &
  & indent=obj%Indent, isSelfClosing=.TRUE.)
CALL tag%WRITE(unitNo=obj%unitNo, isIndented=.TRUE., endRecord=CHAR_LF)
CALL tag%DEALLOCATE()
END PROCEDURE obj_WriteSelfClosingTag

!----------------------------------------------------------------------------
!                                                                   WriteTag
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteTag
TYPE(XMLTag_) :: tag
CALL tag%set(name=name, attrNames=attrNames, attrValues=attrValues, &
  & content=content, Indent=obj%Indent)
CALL tag%WRITE(unitNo=obj%unitNo, isIndented=.TRUE., &
  & isContentIndented=.TRUE., endRecord=CHAR_LF)
CALL tag%DEALLOCATE()
END PROCEDURE obj_WriteTag

END SUBMODULE TagsMethods
