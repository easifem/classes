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

MODULE PVDFile_Class
USE Globaldata
USE BaseType
USE String_Class, ONLY: String
USE XMLFile_Class
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "PVDFile_Class"
INTEGER(I4B), PARAMETER, PUBLIC :: PVD_ASCII = 1
INTEGER(I4B), PARAMETER, PUBLIC :: PVD_BINARY = 2
INTEGER(I4B), PARAMETER, PUBLIC :: PVD_APPENDED = 3
INTEGER(I4B), PARAMETER, PUBLIC :: PVD_RAW_APPENDED = 3
INTEGER(I4B), PARAMETER, PUBLIC :: PVD_BINARY_APPENDED = 4
CHARACTER(*), PARAMETER, DIMENSION(3) :: dataFormatName = &
  & [ &
  & "ascii   ", &
  & "binary  ", &
  & "appended" &
  & ]
PUBLIC :: PVDFile_
PUBLIC :: PVDFilePointer_

!----------------------------------------------------------------------------
!                                                                   PVDFile_
!----------------------------------------------------------------------------

!> authors: Shion Shimizu
! date: 2024-02-03
! summary: PVDFile

TYPE, EXTENDS(XMLFile_) :: PVDFile_
  PRIVATE
  ! INTEGER(I4B) :: dataFormat = 0
    !! PVD_ASCII
    !! PVD_BINARY
    !! PVD_APPENDED
    !! PVD_RAW_APPENDED
    !! PVD_BINARY_APPENDED
  INTEGER(I4B) :: indent = 0
    !! Indent
  TYPE(String), DIMENSION(4) :: attrNames
  ! LOGICAL(LGT) :: search = .FALSE.
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiatePVDFile
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_Final
  PROCEDURE, PUBLIC, PASS(obj) :: CLOSE => obj_Close

  ! IO:
  ! @TagsMethods
  PROCEDURE, PUBLIC, PASS(obj) :: WriteRootTag => obj_WriteRootTag
  PROCEDURE, PUBLIC, PASS(obj) :: WriteDataSetTag => obj_WriteDataSetTag
  PROCEDURE, PUBLIC, PASS(obj) :: WriteStartTag => obj_WriteStartTag
  PROCEDURE, PUBLIC, PASS(obj) :: WriteEndTag => obj_WriteEndTag
  PROCEDURE, PUBLIC, PASS(obj) :: WriteSelfClosingTag => &
    & obj_WriteSelfClosingTag
  PROCEDURE, PUBLIC, PASS(obj) :: WriteTag => obj_WriteTag

END TYPE PVDFile_

!----------------------------------------------------------------------------
!                                                           PVDFilePointer_
!----------------------------------------------------------------------------

TYPE :: PVDFilePointer_
  CLASS(PVDFile_), POINTER :: ptr => NULL()
END TYPE PVDFilePointer_

!----------------------------------------------------------------------------
!                                                PVDFile@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Shion Shimizu
! date: 2024-02-03
! summary: This function returns an instance of PVDFile
!
!# Introduction
! This function returns an instance if PVDFile. After calling this function
! This routine initiates the XMLFile, and opens it.
! It also write header file and dataStructure tag to the file.
!

INTERFACE
  MODULE SUBROUTINE InitiatePVDFile(obj, filename, mode, dataFormat)
    CLASS(PVDFile_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: filename
    !! Name of the file, where xml data will be printed
    CHARACTER(*), OPTIONAL, INTENT(IN) :: mode
    !! READ, WRITE, NEW, REPLACE
    !! DEFAULT IS NEW
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dataFormat
    !! PVD_ASCII, PVD_APPENEDED, PVD_BINARY
    !! DEFAULT is PVD_BINARY_APPENDED
  END SUBROUTINE InitiatePVDFile
END INTERFACE

!----------------------------------------------------------------------------
!                                         Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Shion Shimizu
! date: 2024-02-03
! summary: Deallocates the content of PVDFile and close or delete it.

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj, delete)
    CLASS(PVDFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: delete
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Shion Shimizu
! date: 2024-02-03
! summary: Finalizer for PVDFile

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(PVDFile_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Close@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Shion Shimizu
! date: 2024-02-03
! summary: Close the file

INTERFACE
  MODULE SUBROUTINE obj_Close(obj)
    CLASS(PVDFile_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Close
END INTERFACE

!----------------------------------------------------------------------------
!                                                   WriteRootTag@TagsMethods
!----------------------------------------------------------------------------

!> authors: Shion Shimizu
! date: 2024-02-03
! summary: Write the root tag
!

INTERFACE
  MODULE SUBROUTINE obj_WriteRootTag(obj)
    CLASS(PVDFile_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_WriteRootTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                WriteDataSetTag@TagsMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_WriteDataSetTag(obj, currentTime, filename, part,  &
                                      & group)
    CLASS(PVDFile_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: currentTime
    CHARACTER(*), INTENT(IN) :: filename
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: part
    CHARACTER(*), OPTIONAL, INTENT(IN) :: group
  END SUBROUTINE obj_WriteDataSetTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                  WriteStartTag@TagsMethods
!----------------------------------------------------------------------------

!> authors: Shion Shimizu
! date: 2024-02-03
! summary: Write the start tag

INTERFACE
  MODULE SUBROUTINE obj_WriteStartTag(obj, name, attrNames, attrValues)
    CLASS(PVDFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    TYPE(String), OPTIONAL, INTENT(IN) :: attrNames(:)
    TYPE(String), OPTIONAL, INTENT(IN) :: attrValues(:)
  END SUBROUTINE obj_WriteStartTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                    WriteEndTag@TagsMethods
!----------------------------------------------------------------------------

!> authors: Shion Shimizu
! date: 2024-02-03
! summary: Write the End tag

INTERFACE
  MODULE SUBROUTINE obj_WriteEndTag(obj, name)
    CLASS(PVDFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
  END SUBROUTINE obj_WriteEndTag
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteSelfClosingTag@TagsMethods
!----------------------------------------------------------------------------

!> authors: Shion Shimizu
! date: 2024-02-03
! summary: Write the self closing tag

INTERFACE
  MODULE SUBROUTINE obj_WriteSelfClosingTag(obj, name, attrNames, &
    & attrValues)
    CLASS(PVDFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    TYPE(String), OPTIONAL, INTENT(IN) :: attrNames(:)
    TYPE(String), OPTIONAL, INTENT(IN) :: attrValues(:)
  END SUBROUTINE obj_WriteSelfClosingTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                       WriteTag@TagsMethods
!----------------------------------------------------------------------------

!> authors: Shion Shimizu
! date:  2024-02-03
! summary: Write Tags and contents

INTERFACE
  MODULE SUBROUTINE obj_WriteTag(obj, name, attrNames, &
    & attrValues, content)
    CLASS(PVDFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name
    TYPE(String), OPTIONAL, INTENT(IN) :: attrNames(:)
    TYPE(String), OPTIONAL, INTENT(IN) :: attrValues(:)
    TYPE(String), OPTIONAL, INTENT(IN) :: content
  END SUBROUTINE obj_WriteTag
END INTERFACE

END MODULE PVDFile_Class
