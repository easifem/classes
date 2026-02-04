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
USE TxtFile_Class, ONLY: TxtFile_
USE String_Class, ONLY: String
IMPLICIT NONE

PRIVATE
PUBLIC :: MarkdownFile_

!----------------------------------------------------------------------------
!                                                              MarkdownFile_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-31
! summary: Data type for MarkdownFile_

TYPE, EXTENDS(TxtFile_) :: MarkdownFile_
  PRIVATE
  TYPE(String) :: frontmatter
  TYPE(String) :: content
END TYPE MarkdownFile_

END MODULE MarkdownFile_Class
