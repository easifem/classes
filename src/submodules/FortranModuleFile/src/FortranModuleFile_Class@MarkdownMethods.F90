! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(FortranModuleFile_Class) MarkdownMethods
USE ExceptionHandler_Class, ONLY: e
USE MarkdownFile_Class, ONLY: MarkdownFile_
USE BaseType, ONLY: fileopt => TypeFileOpt

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                       GenerateMarkdownDocs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GenerateMarkdownDocs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GenerateMarkdownDocs()"
#endif

TYPE(MarkdownFile_) :: md
TYPE(UserTypeData_), POINTER :: usertype
TYPE(ProcedureData_), POINTER :: proc
TYPE(String) :: filename, aline
INTEGER(I4B) :: iostat, linelen, ii, tsize
CHARACTER(fileopt%maxStrLen) :: iomsg
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Create a directory named "moduleDir" to keep the markdown files

! Create an index_.md using md, check md%content, senetize it
filename = "_index.md"
CALL md%Initiate( &
  filename=filename%Chars(), status=fileopt%replace, action=fileopt%WRITE)

CALL md%OPEN()

aline = obj%md%GetFrontmatter()
CALL md%WriteFrontmatter(val=aline)

aline = obj%md%GetContent()
linelen = aline%LEN_TRIM()
isok = linelen .NE. 0

IF (isok) THEN
  CALL md%WRITE(val=aline, iostat=iostat, iomsg=iomsg)

ELSE
  linelen = obj%moduleName%LEN_TRIM()
  isok = linelen .NE. 0

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    "moduleName is empty")
#endif

  CALL md%WriteH1(val=obj%moduleDir)
END IF

! ## Subheading for module used
linelen = obj%moduleUsed%LEN_TRIM()
isok = linelen .NE. 0
IF (isok) THEN
  aline = "Used modules"
  CALL md%WriteH2(val=aline)
  CALL md%StartCodeFence(lang="fortran")
  CALL md%WRITE(val=obj%moduleUsed, iostat=iostat, iomsg=iomsg)
  CALL md%EndCodeFence()
END IF

! ## Subheading for userTypes
isok = ALLOCATED(obj%userTypes)
IF (isok) THEN
  tsize = SIZE(obj%userTypes)
  aline = "User types"
  CALL md%WriteH2(val=aline)
  DO ii = 1, tsize
    isok = ASSOCIATED(obj%userTypes(ii)%ptr)
    IF (.NOT. isok) CYCLE
    usertype => obj%userTypes(ii)%ptr
    aline = "["//usertype%GetName()//"]("//"./"//usertype%GetName()//")"
    CALL md%WriteList(val=aline)

    CALL usertype%GenerateMarkdownDocs()
  END DO
END IF

! ## Subheading for procs
isok = ALLOCATED(obj%procs)
IF (isok) THEN
  tsize = SIZE(obj%procs)
  aline = "Procedures"
  CALL md%WriteH2(val=aline)
  DO ii = 1, tsize
    isok = ASSOCIATED(obj%procs(ii)%ptr)
    IF (.NOT. isok) CYCLE
    proc => obj%procs(ii)%ptr
    aline = "["//proc%GetName()//"]("//"./"//proc%GetName()//")"
    CALL md%WriteList(val=aline)

    CALL proc%GenerateMarkdownDocs()
  END DO
END IF

userType => NULL()
proc => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GenerateMarkdownDocs

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE MarkdownMethods
