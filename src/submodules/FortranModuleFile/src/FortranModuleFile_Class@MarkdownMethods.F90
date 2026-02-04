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

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                       GenerateMarkdownDocs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GenerateMarkdownDocs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GenerateMarkdownDocs()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Create a directory named "moduleDir" to keep the markdown files
! Create an index_.md using md, check md%content, senetize it
! Append the following data in the md%content
! # Title
!   brief summary
! ## Subheading for module used
! ## Subheading for userTypes
! ## Subheading for procedureLists

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
