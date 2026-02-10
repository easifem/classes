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
!
SUBMODULE(MarkdownFile_Class) WriteMethods
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "MarkdownFile_Class@WriteMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                           WriteFrontmatter
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteFrontmatter
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteFrontmatter()"
#endif

INTEGER(I4B) :: iostat, linelen
CHARACTER(fileopt%maxStrLen) :: iomsg
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

linelen = val%LEN_TRIM()
isok = linelen .EQ. 0

IF (isok) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = val%start_with(obj%frontmatterSep)

IF (isok) THEN
  CALL obj%WRITE(val=val, iostat=iostat, iomsg=iomsg)
ELSE
  CALL obj%WRITE(val=obj%frontmatterSep, iostat=iostat, iomsg=iomsg)
  CALL obj%WRITE(val=val, iostat=iostat, iomsg=iomsg)
  CALL obj%WRITE(val=obj%frontmatterSep, iostat=iostat, iomsg=iomsg)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_WriteFrontmatter

!----------------------------------------------------------------------------
!                                                                     WriteH1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteH1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteH1()"
#endif

TYPE(String) :: aline
CHARACTER(fileopt%maxStrLen) :: iomsg
INTEGER(I4B) :: iostat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

aline = fileopt%hash//fileopt%space//val

CALL obj%WRITE(val=fileopt%space, iostat=iostat, iomsg=iomsg)
CALL obj%WRITE(val=aline, iostat=iostat, iomsg=iomsg)
CALL obj%WRITE(val=fileopt%space, iostat=iostat, iomsg=iomsg)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_WriteH1

!----------------------------------------------------------------------------
!                                                                     WriteH2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteH2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteH2()"
#endif

TYPE(String) :: aline
CHARACTER(fileopt%maxStrLen) :: iomsg
INTEGER(I4B) :: iostat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

aline = fileopt%hash//fileopt%hash//fileopt%space//val

CALL obj%WRITE(val=fileopt%space, iostat=iostat, iomsg=iomsg)
CALL obj%WRITE(val=aline, iostat=iostat, iomsg=iomsg)
CALL obj%WRITE(val=fileopt%space, iostat=iostat, iomsg=iomsg)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_WriteH2

!----------------------------------------------------------------------------
!                                                             StartCodeFence
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_StartCodeFence
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_StartCodeFence()"
#endif

CHARACTER(fileopt%maxStrLen) :: iomsg
INTEGER(I4B) :: iostat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%WRITE(val=fileopt%space, iostat=iostat, iomsg=iomsg)
CALL obj%WRITE(val="```"//lang)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_StartCodeFence

!----------------------------------------------------------------------------
!                                                             EndCodeFence
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_EndCodeFence
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_EndCodeFence()"
#endif

CHARACTER(fileopt%maxStrLen) :: iomsg
INTEGER(I4B) :: iostat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[End] ')
#endif

CALL obj%WRITE(val="```")
CALL obj%WRITE(val=fileopt%space, iostat=iostat, iomsg=iomsg)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_EndCodeFence

!----------------------------------------------------------------------------
!                                                                  WriteList
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteList
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteList()"
#endif

TYPE(String) :: aline
INTEGER(I4B) :: linelen, iostat
LOGICAL(LGT) :: isok
CHARACTER(fileopt%maxStrLen) :: iomsg

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

linelen = val%LEN_TRIM()
isok = linelen .NE. 0

IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

aline = "-"//fileopt%space//val

CALL obj%WRITE(val=aline, iostat=iostat, iomsg=iomsg)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_WriteList

!----------------------------------------------------------------------------
!                                                               WriteSublist
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteSublist
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteSublist()"
#endif

TYPE(String) :: aline
INTEGER(I4B) :: linelen, iostat
LOGICAL(LGT) :: isok
CHARACTER(fileopt%maxStrLen) :: iomsg

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

linelen = val%LEN_TRIM()
isok = linelen .NE. 0

IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

aline = "  -"//fileopt%space//val

CALL obj%WRITE(val=aline, iostat=iostat, iomsg=iomsg)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_WriteSublist

!----------------------------------------------------------------------------
!                                                            Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteMethods
