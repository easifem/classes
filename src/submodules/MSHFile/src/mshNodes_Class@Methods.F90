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

SUBMODULE(mshNodes_Class) Methods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE n_Final
CALL obj%DEALLOCATE()
END PROCEDURE n_Final

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE n_Deallocate
obj%numNodes = 0
obj%numEntityBlocks = 0
obj%minNodeTag = 0
obj%maxNodeTag = 0
obj%isSparse = .FALSE.
END PROCEDURE n_Deallocate

!----------------------------------------------------------------------------
!                                                                    GotoTag
!----------------------------------------------------------------------------

MODULE PROCEDURE n_GotoTag
! Define internal variables
INTEGER(I4B) :: IOSTAT, Reopen, unitNo
CHARACTER(100) :: Dummy
CHARACTER(*), PARAMETER :: myName = "n_GotoTag"
LOGICAL(LGT) :: isNotOpen, isNotRead

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] GotoTag()')
#endif

isNotOpen = .NOT. mshFile%isOpen()
isNotRead = .NOT. mshFile%isRead()

! Find $meshFormat
IF (isNotOpen .OR. isNotRead) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: mshFile is either not opened '// &
    & 'or does not have read access!')
  error = -1
  RETURN
END IF

Reopen = 0; error = 0; 
#ifndef Darwin_SYSTEM
CALL mshFile%REWIND()
#endif

DO
  unitNo = mshFile%getUnitNo()
  READ (unitNo, "(A)", IOSTAT=IOSTAT) Dummy

  IF (IS_IOSTAT_END(IOSTAT)) THEN
    CALL mshFile%setEOFStat(.TRUE.)

#ifdef Darwin_SYSTEM
    CALL mshFile%CLOSE()
    CALL mshFile%OPEN()
#endif

    Reopen = Reopen + 1
  END IF

  IF (IOSTAT .GT. 0 .OR. Reopen .GT. 1) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Could not find $Nodes !')
    error = -2
    EXIT
  ELSE IF (TRIM(Dummy) .EQ. '$Nodes') THEN
    EXIT
  END IF
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] GotoTag()')
#endif
END PROCEDURE n_GotoTag

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE n_Read
CHARACTER(*), PARAMETER :: myName = "n_Read()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Read()')
#endif

CALL obj%GotoTag(mshFile, error)

IF (error .NE. 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Error occured while finding the tag.')
END IF

IF (mshFormat%getMajorVersion() .GT. 2) THEN
  READ (mshFile%getUnitNo(), *) obj%numEntityBlocks, obj%numNodes, &
    & obj%minNodeTag, obj%maxNodeTag
  IF ((obj%maxNodeTag - obj%minNodeTag) &
    & .EQ. (obj%numNodes - 1)) THEN
    obj%isSparse = .FALSE.
  ELSE
    obj%isSparse = .TRUE.
  END IF
ELSE
  READ (mshFile%getUnitNo(), *) obj%numNodes
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Read()')
#endif
END PROCEDURE n_Read

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

MODULE PROCEDURE n_write
TYPE(String) :: astr
INTEGER(I4B) :: unitNo

unitNo = afile%getUnitNo()

astr = tostring(obj%numEntityBlocks)//" "// &
  & tostring(obj%numNodes)//" "// &
  & tostring(obj%minNodeTag)//" "// &
  & tostring(obj%maxNodeTag)

WRITE (unitNo, "(A)") astr%chars()

END PROCEDURE n_write

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE n_Display
! Define internal variables
INTEGER(I4B) :: I
!
IF (PRESENT(UnitNo)) THEN
  I = UnitNo
ELSE
  I = stdout
END IF
!
IF (LEN_TRIM(Msg) .NE. 0) THEN
  WRITE (I, "(A)") TRIM(Msg)
END IF
!
CALL BlankLines(UnitNo=I, NOL=1)
WRITE (I, "(A)") "| Property | Value |"
WRITE (I, "(A)") "| :----    | ---:  |"
WRITE (I, "(A, I4, A)") "| Total Nodes    | ", obj%NumNodes, " | "
WRITE (I, "(A, I4, A)") "| Total Entities | ", obj%NumEntityBlocks, &
  & " | "
WRITE (I, "(A, I4, A)") "| Min Node Tag   | ", obj%minNodeTag, &
  & " | "
WRITE (I, "(A, I4, A)") "| Max Node Tag   | ", obj%maxNodeTag, &
  & " | "
WRITE (I, "(A, G5.2, A)") "| isSparse       | ", obj%isSparse, &
  & " | "
END PROCEDURE n_Display

!----------------------------------------------------------------------------
!                                                        getNumEntityBlocks
!----------------------------------------------------------------------------

MODULE PROCEDURE n_getNumEntityBlocks
ans = obj%NumEntityBlocks
END PROCEDURE n_getNumEntityBlocks

!----------------------------------------------------------------------------
!                                                                getNumNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE n_getNumNodes
ans = obj%NumNodes
END PROCEDURE n_getNumNodes

!----------------------------------------------------------------------------
!                                                            getMaxNodeTag
!----------------------------------------------------------------------------

MODULE PROCEDURE n_getMaxNodeTag
ans = obj%maxNodeTag
END PROCEDURE n_getMaxNodeTag

!----------------------------------------------------------------------------
!                                                            getMinNodeTag
!----------------------------------------------------------------------------

MODULE PROCEDURE n_getMinNodeTag
ans = obj%MinNodeTag
END PROCEDURE n_getMinNodeTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
