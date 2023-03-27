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
! date:         11 June 2021
! summary:         This module defines a class to handle elements in mesh file

MODULE mshElements_Class
USE BaseType
USE GlobalData
USE mshFormat_Class
USE TxtFile_Class
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "MSHELEMENT_CLASS"

!----------------------------------------------------------------------------
!                                                              mshElements_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 11 June 2021
! summary: This class handles the elements present in the mesh file

TYPE :: mshElements_
  PRIVATE
  INTEGER(I4B) :: numElements = 0
  INTEGER(I4B) :: numEntityBlocks = 0
  INTEGER(I4B) :: minElementTag = 0
  INTEGER(I4B) :: maxElementTag = 0
  LOGICAL(LGT) :: isSparse = .FALSE.
CONTAINS
  PRIVATE
  FINAL :: el_Final
      !! Finalizer
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => el_Deallocate
      !! deallocate data
  PROCEDURE, PUBLIC, PASS(obj) :: GotoTag => el_GotoTag
      !! go to the tag
  PROCEDURE, PUBLIC, PASS(obj) :: READ => el_Read
      !! Read data form file
  PROCEDURE, PUBLIC, PASS(obj) :: WRITE => el_Write
      !! Write data to file
  PROCEDURE, PUBLIC, PASS(obj) :: getNumElements => el_getNumElements
      !! total elements
  PROCEDURE, PUBLIC, PASS(obj) :: getNumEntityBlocks => el_getNumEntityBlocks
      !! Returns the number of entity blocks
  PROCEDURE, PUBLIC, PASS(Obj) :: getMinElementTag => el_getMinElementTag
  PROCEDURE, PUBLIC, PASS(Obj) :: getMaxElementTag => el_getMaxElementTag
END TYPE mshElements_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: mshElements_
TYPE(mshElements_), PUBLIC, PARAMETER :: TypemshElements = mshElements_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: mshElementsPointer_
  CLASS(mshElements_), POINTER :: Ptr => NULL()
END TYPE mshElementsPointer_
PUBLIC :: mshElementsPointer_

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE el_Final(obj)
    TYPE(mshElements_), INTENT(INOUT) :: obj
  END SUBROUTINE el_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 June 2021
! summary: This subroutine deallocates the data from obj

INTERFACE
  MODULE SUBROUTINE el_Deallocate(obj)
    CLASS(mshElements_), INTENT(INOUT) :: obj
  END SUBROUTINE el_Deallocate
END INTERFACE

INTERFACE DEALLOCATE
  MODULE PROCEDURE el_Deallocate
END INTERFACE DEALLOCATE

PUBLIC :: DEALLOCATE

!----------------------------------------------------------------------------
!                                                                   GotoTag
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 June 2021
! summary: This subroutine go the location of element in mesh file

INTERFACE
  MODULE SUBROUTINE el_GotoTag(obj, mshFile, error)
    CLASS(mshElements_), INTENT(IN) :: obj
    CLASS(TxtFile_), INTENT(INOUT) :: mshFile
    INTEGER(I4B), INTENT(INOUT) :: error
  END SUBROUTINE el_GotoTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 June 2021
! summary:         This subroutine reads data from a file

INTERFACE
  MODULE SUBROUTINE el_Read(obj, mshFile, mshFormat, error)
    CLASS(mshElements_), INTENT(INOUT) :: obj
    CLASS(TxtFile_), INTENT(INOUT) :: mshFile
    TYPE(mshFormat_), INTENT(INOUT) :: mshFormat
    INTEGER(I4B), INTENT(INOUT) :: error
  END SUBROUTINE el_Read
END INTERFACE

!----------------------------------------------------------------------------
!                                                      WriteToFile@mshElement
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 11 June 2021
! summary: This subroutine writes the data to a file

INTERFACE
  MODULE SUBROUTINE el_Write(obj, afile)
    CLASS(mshElements_), INTENT(INOUT) :: obj
    CLASS(TxtFile_), INTENT(INOUT) :: afile
  END SUBROUTINE el_Write
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Display@mshElements
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 June 2021
! summary:         This data displays the content of [[mshElements_]]

INTERFACE
  MODULE SUBROUTINE el_display(obj, Msg, UnitNo)
    CLASS(mshElements_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: Msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
  END SUBROUTINE el_display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE el_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                  TotalElements@mshElements
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 June 2021
! summary:          This function returns total number of elements

INTERFACE
  MODULE PURE FUNCTION el_getNumElements(obj) RESULT(ans)
    CLASS(mshElements_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION el_getNumElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getNumEntityBlocks
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 June 2021
! summary:         This function returns the number of entities blocks

INTERFACE
  MODULE PURE FUNCTION el_getNumEntityBlocks(obj) RESULT(Ans)
    CLASS(mshElements_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION el_getNumEntityBlocks
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getMinElementTag
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         12 June 2021
! summary: This routine returns the minimum element tag

INTERFACE
  MODULE PURE FUNCTION el_getMinElementTag(obj) RESULT(Ans)
    CLASS(mshElements_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION el_getMinElementTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getMaxElementTag
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         12 June 2021
! summary: This routine returns the Maximum element tag

INTERFACE
  MODULE PURE FUNCTION el_getMaxElementTag(obj) RESULT(Ans)
    CLASS(mshElements_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION el_getMaxElementTag
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE mshElements_Class
