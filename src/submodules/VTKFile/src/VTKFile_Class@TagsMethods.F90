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

SUBMODULE(VTKFile_Class) TagsMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteRootTag
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteRootTag()"
TYPE(String) :: buffer
CHARACTER(100) :: ioerrmsg
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & ' [INTERNAL ERROR] :: VTKFile is not initiated!')
END IF

IF (.NOT. obj%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: VTKFile is not open')
END IF

IF (.NOT. obj%isWrite()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: VTKFile does not have write permission')
END IF

buffer = '<?xml version="1.0" encoding="UTF-8"?>'//CHAR_LF

IF (endian .EQ. endianL) THEN
  buffer = buffer//'<VTKFile type="'//TRIM(obj%DataStructureName)// &
  & '" version="1.0" byte_order="LittleEndian">'
ELSE
  buffer = buffer//'<VTKFile type="'//TRIM(obj%DataStructureName)// &
  & '" version="1.0" byte_order="BigEndian">'
END IF

WRITE (obj%unitNo, "(a)", iostat=ierr, iomsg=ioerrmsg) &
  & TRIM(buffer%chars())//CHAR_LF

obj%indent = 2

IF (ierr .NE. 0) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Error has occured while'//  &
    & 'writing header info in VTKFile iostat = '// &
    & TRIM(str(ierr, .TRUE.))//' error msg :: '// &
    & TRIM(ioerrmsg))
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE VTKFile_WriteRootTag

!----------------------------------------------------------------------------
!                                                      WriteDataStructureTag
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataStructureTag
CHARACTER(*), PARAMETER :: myName = "VTKFile_WriteDataStructureTag()"
INTEGER(I4B) :: tattr
TYPE(String) :: attrNames(4), attrValues(4)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

tattr = 0

SELECT CASE (obj%DataStructureType)

! Structured
CASE (VTK_RectilinearGrid, VTK_StructuredGrid, VTK_ImageData)
  attrNames(1) = "WholeExtent"
  attrValues(1) = &
    & '"'// &
    & str(obj%WholeExtent(1), .TRUE.)//CHAR_SPACE// &
    & str(obj%WholeExtent(2), .TRUE.)//CHAR_SPACE// &
    & str(obj%WholeExtent(3), .TRUE.)//CHAR_SPACE// &
    & str(obj%WholeExtent(4), .TRUE.)//CHAR_SPACE// &
    & str(obj%WholeExtent(5), .TRUE.)//CHAR_SPACE// &
    & str(obj%WholeExtent(6), .TRUE.)// &
    & '"'
  tattr = 1

  IF (obj%DataStructureType .EQ. VTK_ImageData) THEN
    attrNames(2) = "Origin"
    attrNames(3) = "Spacing"
    attrValues(2) = &
    & '"'// &
    & str(obj%Origin(1), .TRUE.)//CHAR_SPACE// &
    & str(obj%Origin(2), .TRUE.)//CHAR_SPACE// &
    & str(obj%Origin(3), .TRUE.)//CHAR_SPACE// &
    & '"'
    attrValues(3) = &
    & '"'// &
    & str(obj%SPACING(1), .TRUE.)//CHAR_SPACE// &
    & str(obj%SPACING(2), .TRUE.)//CHAR_SPACE// &
    & str(obj%SPACING(3), .TRUE.)//CHAR_SPACE// &
    & '"'
    tattr = 3
  END IF

! Parallel Structured
CASE (PARALLEL_VTK_RectilinearGrid, PARALLEL_VTK_StructuredGrid)
  attrNames(1) = "WholeExtent"
  attrValues(1) = &
    & '"'// &
    & str(obj%WholeExtent(1), .TRUE.)//CHAR_SPACE// &
    & str(obj%WholeExtent(2), .TRUE.)//CHAR_SPACE// &
    & str(obj%WholeExtent(3), .TRUE.)//CHAR_SPACE// &
    & str(obj%WholeExtent(4), .TRUE.)//CHAR_SPACE// &
    & str(obj%WholeExtent(5), .TRUE.)//CHAR_SPACE// &
    & str(obj%WholeExtent(6), .TRUE.)// &
    & '"'
  tattr = 1

  attrNames(2) = "GhostLevel"
  attrValues(2) = '"#"'
  tattr = 2

! Parallel unstructured
CASE (PARALLEL_VTK_UnstructuredGrid)
  attrNames(1) = "GhostLevel"
  attrValues(1) = '"#"'
  tattr = 1
END SELECT

! Write the StartTag
IF (tattr .GT. 0) THEN
  CALL obj%writeStartTag( &
    & name=String(obj%DataStructureName), &
    & attrNames=attrNames(1:tattr), &
    & attrValues=attrValues(1:tattr))
ELSE
  CALL obj%writeStartTag(name=String(obj%DataStructureName))
END IF

! parallel data structure types
SELECT CASE (obj%DataStructureType)

! Parallel RectilinearGrid
CASE (PARALLEL_VTK_RectilinearGrid)

  IF (.NOT. PRESENT(meshDataFormat)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR] :: meshDataFormat should be present '//  &
      & 'for PARALLEL_VTK_RECTILINEAR')
    RETURN
  END IF

  ! Write <PCoordinates>
  CALL obj%WriteStartTag(name=String('PCoordinates'))

  CALL obj%WriteSelfClosingTag( &
    & name=String('PDataArray'), &
    & attrNames=[String('type')], &
    & attrValues=[String('"'//TRIM(meshDataFormat)//'"')])

  CALL obj%WriteSelfClosingTag( &
    & name=String('PDataArray'), &
    & attrNames=[String('type')], &
    & attrValues=[String('"'//TRIM(meshDataFormat)//'"')])

  CALL obj%WriteSelfClosingTag( &
    & name=String('PDataArray'), &
    & attrNames=[String('type')], &
    & attrValues=[String('"'//TRIM(meshDataFormat)//'"')])

  CALL obj%WriteEndTag(name=String('PCoordinates'))

! PARALLEL StructuredGrid, PARALLEL UnstructuredGrid
CASE (PARALLEL_VTK_StructuredGrid, PARALLEL_VTK_UnstructuredGrid)

  IF (.NOT. PRESENT(meshDataFormat)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & '[INTERNAL ERROR] :: meshDataFormat should be present '//  &
      & 'for PARALLEL_VTK_RECTILINEAR')
    RETURN
  END IF

  CALL obj%WriteStartTag(name=String('PPoints'))
  CALL obj%WriteSelfClosingTag(  &
    & name=String('PDataArray'), &
    & attrNames=[String('type'), String('NumberOfComponents'), &
    & String('Name')], &
    & attrValues=[String('"'//TRIM(meshDataFormat)//'"'), &
    & String('"3"'), String('"Points"')])
  CALL obj%WriteEndTag(name=String('PPoints'))
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE VTKFile_WriteDataStructureTag

!----------------------------------------------------------------------------
!                                                             WriteStartTag
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteStartTag
TYPE(XMLTag_) :: tag
CALL tag%set(name=name, attrNames=attrNames, attrValues=attrValues, &
  & Indent=obj%Indent)
CALL tag%WRITE(unitNo=obj%unitNo, isIndented=.TRUE., endRecord=CHAR_LF, &
  & onlyStart=.TRUE.)
obj%Indent = obj%Indent + 2
CALL tag%DEALLOCATE()
END PROCEDURE VTKFile_WriteStartTag

!----------------------------------------------------------------------------
!                                                              WriteEndTag
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteEndTag
TYPE(XMLTag_) :: tag
CALL tag%set(name=name, indent=obj%indent)
CALL tag%WRITE(unitNo=obj%unitNo, isIndented=.TRUE., endRecord=CHAR_LF, &
  & onlyEnd=.TRUE.)
CALL tag%DEALLOCATE()
END PROCEDURE VTKFile_WriteEndTag

!----------------------------------------------------------------------------
!                                                       WriteSelfClosingTag
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteSelfClosingTag
TYPE(XMLTag_) :: tag
CALL tag%set(name=name, attrNames=attrNames, attrValues=attrValues, &
  & indent=obj%Indent, isSelfClosing=.TRUE.)
CALL tag%WRITE(unitNo=obj%unitNo, isIndented=.TRUE., endRecord=CHAR_LF)
CALL tag%DEALLOCATE()
END PROCEDURE VTKFile_WriteSelfClosingTag

!----------------------------------------------------------------------------
!                                                                   WriteTag
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteTag
TYPE(XMLTag_) :: tag
CALL tag%set(name=name, attrNames=attrNames, attrValues=attrValues, &
  & content=content, Indent=obj%Indent)
CALL tag%WRITE(unitNo=obj%unitNo, isIndented=.TRUE., &
  & isContentIndented=.TRUE., endRecord=CHAR_LF)
CALL tag%DEALLOCATE()
END PROCEDURE VTKFile_WriteTag

END SUBMODULE TagsMethods
