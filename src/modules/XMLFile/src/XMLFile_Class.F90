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

MODULE XMLFile_Class
USE GlobalData
USE String_Class, ONLY: String
USE AbstractFile_Class
USE XMLTag_Class
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
!!
!!
!!
CHARACTER(*), PARAMETER :: modName = "XMLFile_Class"
PUBLIC :: xmlTag_

!----------------------------------------------------------------------------
!                                                              XMLFile_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: File to handle XML format.

TYPE, EXTENDS(AbstractFile_) :: XMLFile_
  PRIVATE
  LOGICAL(LGT), PUBLIC :: isInitiated = .FALSE.
    !! Logical indicating if file was initialized
  INTEGER(I4B), PUBLIC :: unitNo = -1
    !! The unit number assigned to the file
  REAL(DFP) :: version = 1.0_DFP
    !! The XML version
  CHARACTER(32) :: encoding = 'UTF-8'
    !! The XML file encoding
  TYPE(String) :: style_sheet
  LOGICAL(LGT) :: standalone = .FALSE.
    !! The root XML element of the file
  LOGICAL(LGT) :: newstat = .FALSE.
    !! The 'new' status of a file
  LOGICAL(LGT) :: overwriteStat = .FALSE.
    !! When .TRUE., file data can be overwritten
  TYPE(String) :: fullname
    !! full name of the file
  TYPE(XMLTag_), PUBLIC, POINTER :: root => NULL()
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => xmlFile_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => xmlFile_Deallocate
  FINAL :: xmlFile_Final
  PROCEDURE, PUBLIC, PASS(obj) :: OPEN => xmlFile_Open
  PROCEDURE, PUBLIC, PASS(obj) :: CLOSE => xmlFile_Close
  PROCEDURE, PUBLIC, PASS(obj) :: Delete => xmlFile_Delete
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => xmlFile_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => xmlFile_Export
  PROCEDURE, PUBLIC, PASS(Obj) :: setNewStat => xmlFile_setNewStat
  PROCEDURE, PUBLIC, PASS(Obj) :: setOverwriteStat => &
    & xmlFile_setOverwriteStat
  PROCEDURE, PUBLIC, PASS(Obj) :: isNew => xmlFile_isNew
  PROCEDURE, PUBLIC, PASS(obj) :: isOverwrite => xmlFile_isOverwrite
  PROCEDURE, PUBLIC, PASS(Obj) :: isFormatted => xmlFile_isFormatted
  PROCEDURE, PUBLIC, PASS(obj) :: parseXMLDeclaration => &
    & xmlFile_parseXMLDeclaration
  PROCEDURE, PUBLIC, PASS(obj) :: BuildCache => xmlFile_BuildCache
  PROCEDURE, PUBLIC, PASS(obj) :: Display => xmlFile_Display
END TYPE XMLFile_

PUBLIC :: XMLFile_

!----------------------------------------------------------------------------
!                                                        XMLFilePointer_
!----------------------------------------------------------------------------

TYPE :: XMLFilePointer_
  CLASS(XMLFile_), POINTER :: ptr => NULL()
END TYPE XMLFilePointer_

PUBLIC :: XMLFilePointer_

!----------------------------------------------------------------------------
!                                              Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 9 Sept 2021
! summary: This routine initiates xmlfile
!
!# Introduction
!
! This routine initiates the xmlFile_.
!
! - filename is full name of the file
! - mode can be READ, WRITE, NEW, OVERWRITE
! - READ: The file should exists
! - WRITE: The file should exists
! - OVERWRITE: The file is opened with read and write access. If the file
! does not exists, then a new file is created.
! - NEW: The file is opened with read and write access. If the file
! does not exists, then a new file is created.

INTERFACE
  MODULE SUBROUTINE xmlFile_Initiate(obj, filename, mode)
    CLASS(xmlFile_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: filename
    CHARACTER(*), INTENT(IN) :: mode
    !! mode can be READ, WRITE, NEW, OVERWRITE
  END SUBROUTINE xmlFile_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                        Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Deallocate the data stored in the file, close the file.
!
!# Introduction
!
! This routine deallocates the data stored in the file. This routine also
! close the file. If `delete` is present then delete the file.

INTERFACE
  MODULE SUBROUTINE xmlFile_Deallocate(obj, delete)
    CLASS(xmlFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: delete
  END SUBROUTINE xmlFile_Deallocate
END INTERFACE

PUBLIC :: xmlFile_Deallocate

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Finalizer

INTERFACE
  MODULE SUBROUTINE xmlFile_Final(obj)
    TYPE(xmlFile_), INTENT(INOUT) :: obj
  END SUBROUTINE xmlFile_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Open@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Open the xml file

INTERFACE
  MODULE SUBROUTINE xmlFile_Open(obj)
    CLASS(xmlFile_), INTENT(INOUT) :: obj
  END SUBROUTINE xmlFile_Open
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Close@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Close the xml file

INTERFACE
  MODULE SUBROUTINE xmlFile_Close(obj)
    CLASS(xmlFile_), INTENT(INOUT) :: obj
  END SUBROUTINE xmlFile_Close
END INTERFACE

PUBLIC :: XMLFile_Close

!----------------------------------------------------------------------------
!                                                   Delete@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Delete the xmlfile

INTERFACE
  MODULE SUBROUTINE xmlFile_Delete(obj)
    CLASS(xmlFile_), INTENT(INOUT) :: obj
  END SUBROUTINE xmlFile_Delete
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Export the xml file

INTERFACE
  MODULE SUBROUTINE xmlFile_Export(obj, filename)
    CLASS(xmlFile_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE xmlFile_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Display the contents of xmlfile

INTERFACE
  MODULE SUBROUTINE xmlFile_Display(obj, msg, unitNo)
    CLASS(XMLFile_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE xmlFile_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Import the xml file

INTERFACE
  MODULE SUBROUTINE xmlFile_Import(obj, filename)
    CLASS(xmlFile_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE xmlFile_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                              ParseXMLDeclaration@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Parse XML declaration

INTERFACE
  MODULE SUBROUTINE xmlFile_parseXMLDeclaration(obj)
    CLASS(XMLFile_), INTENT(INOUT) :: obj
  END SUBROUTINE xmlFile_parseXMLDeclaration
END INTERFACE

!----------------------------------------------------------------------------
!                                                      buildCache@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Build Cache

INTERFACE
  MODULE SUBROUTINE xmlFile_BuildCache(obj, nchars, fileCache)
    CLASS(XMLFile_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(OUT) :: nchars
    CHARACTER(1), ALLOCATABLE, INTENT(INOUT) :: fileCache(:)
  END SUBROUTINE xmlFile_BuildCache
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setNewStat@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 June 2021
! summary: Sets whether or not the xml file is new.

INTERFACE
  MODULE SUBROUTINE xmlFile_setNewStat(obj, bool)
    CLASS(XMLFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: bool
  END SUBROUTINE xmlFile_setNewStat
END INTERFACE

!----------------------------------------------------------------------------
!                                              setOverwriteStat@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         6 June 2021
! summary:         Sets the value for the status of whether or not the file will be overwritable.

INTERFACE
  MODULE SUBROUTINE xmlFile_setOverwriteStat(obj, bool)
    CLASS(XMLFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: bool
  END SUBROUTINE xmlFile_setOverwriteStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                          isNew@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         6 June 2021
! summary: Returns whether or not the HDF5 file is new.

INTERFACE
  MODULE PURE FUNCTION xmlFile_isNew(obj) RESULT(Ans)
    CLASS(XMLFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION xmlFile_isNew
END INTERFACE

!----------------------------------------------------------------------------
!                                                    isOverwrite@getMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION xmlFile_isOverwrite(obj) RESULT(Ans)
    CLASS(XMLFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION xmlFile_isOverwrite
END INTERFACE

!----------------------------------------------------------------------------
!                                                    isFormatted@getMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION xmlFile_isFormatted(obj) RESULT(ans)
    CLASS(XMLFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION xmlFile_isFormatted
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE XMLFile_Class
