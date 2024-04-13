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
! summary:         This module implements a class for handling nodes in mesh file

MODULE mshNodes_Class
USE BaseType
USE GlobalData
USE mshFormat_Class
USE TxtFile_Class
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "mshNodes_Class"

!----------------------------------------------------------------------------
!                                                                 mshNodes_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 JUNE 2021
! summary:         This class is defined to handle the nodes in mesh file

TYPE :: mshNodes_
  PRIVATE
  INTEGER(I4B) :: numNodes = 0
    !! number of nodes
  INTEGER(I4B) :: numEntityBlocks = 0
    !! number of entity blocks which contains the nodes
  INTEGER(I4B) :: minNodeTag = 0
    !! minimum node number
  INTEGER(I4B) :: maxNodeTag = 0
    !! maximum node number
  LOGICAL(LGT) :: isSparse = .FALSE.
    !! isSparse

CONTAINS
  FINAL :: n_Final
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => n_Deallocate
      !! Deallocate From the object
  PROCEDURE, PUBLIC, PASS(obj) :: GotoTag => n_GotoTag
      !! Go to the node tag in mesh file
  PROCEDURE, PUBLIC, PASS(obj) :: READ => n_Read
      !! read content from file
  PROCEDURE, PUBLIC, PASS(obj) :: WRITE => n_Write
      !! write data to file
  PROCEDURE, PUBLIC, PASS(obj) :: Display => n_Display
      !! Display the content of msh Nodes
  PROCEDURE, PUBLIC, PASS(obj) :: getNumEntityBlocks => n_getNumEntityBlocks
  PROCEDURE, PUBLIC, PASS(Obj) :: getNumNodes => n_getNumNodes
  PROCEDURE, PUBLIC, PASS(Obj) :: getMaxNodeTag => n_getMaxNodeTag
  PROCEDURE, PUBLIC, PASS(Obj) :: getMinNodeTag => n_getMinNodeTag
END TYPE mshNodes_

PUBLIC :: mshNodes_
TYPE(mshNodes_), PUBLIC, PARAMETER :: TypeMshNodes = mshNodes_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: mshNodesPointer_
  CLASS(mshNodes_), POINTER :: Ptr => NULL()
END TYPE mshNodesPointer_
PUBLIC :: mshNodesPointer_

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE n_Final(obj)
    TYPE(mshNodes_), INTENT(INOUT) :: obj
  END SUBROUTINE n_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 June 2021
! summary: This subroutine deallocate the data form the instance

INTERFACE
  MODULE SUBROUTINE n_Deallocate(obj)
    CLASS(mshNodes_), INTENT(INOUT) :: obj
  END SUBROUTINE n_Deallocate
END INTERFACE

INTERFACE DEALLOCATE
  MODULE PROCEDURE n_Deallocate
END INTERFACE DEALLOCATE

PUBLIC :: DEALLOCATE

!----------------------------------------------------------------------------
!                                                                   GotoTag
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 June 2021
! summary: This subroutine go to the position where nodes are defined

INTERFACE
  MODULE SUBROUTINE n_GotoTag(obj, mshFile, error)
    CLASS(mshNodes_), INTENT(IN) :: obj
    CLASS(TxtFile_), INTENT(INOUT) :: mshFile
    INTEGER(I4B), INTENT(INOUT) :: error
  END SUBROUTINE n_GotoTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 June 2021
! summary:         This subroutine read data from mesh file

INTERFACE
  MODULE SUBROUTINE n_Read(obj, mshFile, mshFormat, error)
    CLASS(mshNodes_), INTENT(INOUT) :: obj
    CLASS(TxtFile_), INTENT(INOUT) :: mshFile
    TYPE(mshFormat_), INTENT(INOUT) :: mshFormat
    INTEGER(I4B), INTENT(INOUT) :: error
  END SUBROUTINE n_Read
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 11 June 2021
! summary: This subroutine write data to a file

INTERFACE
  MODULE SUBROUTINE n_Write(obj, afile)
    CLASS(mshNodes_), INTENT(INOUT) :: obj
    CLASS(TxtFile_), INTENT(INOUT) :: afile
  END SUBROUTINE n_Write
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 June 2021
! summary: This subroutine display contents of [[mshNodes_]]

INTERFACE
  MODULE SUBROUTINE n_Display(obj, Msg, UnitNo)
    CLASS(mshNodes_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: Msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
  END SUBROUTINE n_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE n_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                         getNumEntityBlocks
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 June 2021
! summary:         This function returns the number of entities blocks

INTERFACE
  MODULE PURE FUNCTION n_getNumEntityBlocks(obj) RESULT(Ans)
    CLASS(mshNodes_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION n_getNumEntityBlocks
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getNumNodes
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         11 June 2021
! summary:         This function returns the number of entities blocks

INTERFACE
  MODULE PURE FUNCTION n_getNumNodes(obj) RESULT(Ans)
    CLASS(mshNodes_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION n_getNumNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                            getMaxNodeTag
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION n_getMaxNodeTag(obj) RESULT(Ans)
    CLASS(mshNodes_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION n_getMaxNodeTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                            getMinNodeTag
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION n_getMinNodeTag(obj) RESULT(Ans)
    CLASS(mshNodes_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION n_getMinNodeTag
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE mshNodes_Class
