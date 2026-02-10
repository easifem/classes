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

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-31
! summary: Extended TxtFile to handle MarkdownFiles

MODULE MarkdownFile_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE TxtFile_Class, ONLY: TxtFile_
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: fileopt => TypeFileOpt

IMPLICIT NONE

PRIVATE
PUBLIC :: MarkdownFile_

!----------------------------------------------------------------------------
!                                                              MarkdownFile_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-31
! summary: Data type for MarkdownFile_
!
!# MarkdownFile_
!
! MarkdownFile_ is a child of [TxtFile](../TxtFile). It handles
! reading and writing markdown files.

TYPE, EXTENDS(TxtFile_) :: MarkdownFile_
  PRIVATE
  CHARACTER(3) :: frontmatterSep = "---"

CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: WriteFrontmatter => obj_WriteFrontmatter
  !! Write frontmatter to markdown file
  PROCEDURE, PUBLIC, PASS(obj) :: WriteH1 => obj_WriteH1
  !! Write h1 heading
  PROCEDURE, PUBLIC, PASS(obj) :: WriteH2 => obj_WriteH2
  !! Write h2 heading
  PROCEDURE, PUBLIC, PASS(obj) :: StartCodeFence => obj_StartCodeFence
  !! Write the start of code fence
  PROCEDURE, PUBLIC, PASS(obj) :: EndCodeFence => obj_EndCodeFence
  !! Write the end of code fence
  PROCEDURE, PUBLIC, PASS(obj) :: WriteList => obj_WriteList
  !! write a list entry
  PROCEDURE, PUBLIC, PASS(obj) :: WriteSublist => obj_WriteSublist
  !! write a sublist entry
END TYPE MarkdownFile_

!----------------------------------------------------------------------------
!                                              WriteFrontmatter@WriteMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-09
! summary: Write frontmatter to markdown file.
!
!# WriteFrontMatter
!
! This method write frontmatter (given by val) to markdown file.
! If frontmatter is empty then it will not write anything.
! It will check if frontmatter starts with '---'. If there is not
! frontmatter separator then will enclose the frontmatter between
! frontmatter separator.

INTERFACE
  MODULE SUBROUTINE obj_WriteFrontmatter(obj, val)
    CLASS(MarkdownFile_), INTENT(INOUT) :: obj
    !! markdown file
    TYPE(String), INTENT(IN) :: val
    !! frontmatter
  END SUBROUTINE obj_WriteFrontmatter
END INTERFACE

!----------------------------------------------------------------------------
!                                                       WriteH1@WriteMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-09
! summary: Write h1 heading
!
!# WriteH1
!
! This method append val with `#` and write the line.

INTERFACE
  MODULE SUBROUTINE obj_WriteH1(obj, val)
    CLASS(MarkdownFile_), INTENT(INOUT) :: obj
    !! markdown file
    TYPE(String), INTENT(IN) :: val
    !! h1 heading
  END SUBROUTINE obj_WriteH1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       WriteH2@WriteMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-09
! summary: Write H2 heading
!
!# WriteH2
!
! This method append val with `##` and write the line.

INTERFACE
  MODULE SUBROUTINE obj_WriteH2(obj, val)
    CLASS(MarkdownFile_), INTENT(INOUT) :: obj
    !! markdown file
    TYPE(String), INTENT(IN) :: val
    !! H2 heading
  END SUBROUTINE obj_WriteH2
END INTERFACE

!----------------------------------------------------------------------------
!                                                StartCodeFence@WriteMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-09
! summary: Write the start of code fence
!
!# StartCodeFence
!
! This method write the start of code fence.

INTERFACE
  MODULE SUBROUTINE obj_StartCodeFence(obj, lang)
    CLASS(MarkdownFile_), INTENT(INOUT) :: obj
    !! markdown file
    CHARACTER(*), INTENT(IN) :: lang
    !! code fence language
  END SUBROUTINE obj_StartCodeFence
END INTERFACE

!----------------------------------------------------------------------------
!                                                  EndCodeFence@WriteMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-09
! summary: Write the end of code fence.
!
!# EndCodeFence
!
! This method write the end of code fence.

INTERFACE
  MODULE SUBROUTINE obj_EndCodeFence(obj)
    CLASS(MarkdownFile_), INTENT(INOUT) :: obj
    !! markdown file
  END SUBROUTINE obj_EndCodeFence
END INTERFACE

!----------------------------------------------------------------------------
!                                                     WriteList@WriteMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-09
! summary: Write a list entry
!
!# WriteList
!
! This method write a list entry, which starts with `-`.

INTERFACE
  MODULE SUBROUTINE obj_WriteList(obj, val)
    CLASS(MarkdownFile_), INTENT(INOUT) :: obj
    !! markdown file
    TYPE(String), INTENT(IN) :: val
    !! list entry
  END SUBROUTINE obj_WriteList
END INTERFACE

!----------------------------------------------------------------------------
!                                                  WriteSublist@WriteMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-09
! summary: Write a sublist entry
!
!# WriteList
!
! This method write a sublist entry, which starts with `-`.

INTERFACE
  MODULE SUBROUTINE obj_WriteSublist(obj, val)
    CLASS(MarkdownFile_), INTENT(INOUT) :: obj
    !! markdown file
    TYPE(String), INTENT(IN) :: val
    !! list entry
  END SUBROUTINE obj_WriteSublist
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MarkdownFile_Class
