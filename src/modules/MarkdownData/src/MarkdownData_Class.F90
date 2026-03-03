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
! date: 2026-02-02
! summary: This module contains data structure and methods for dealing
!          with markdown data

MODULE MarkdownData_Class
USE String_Class, ONLY: String
USE GlobalData, ONLY: I4B, DFP, LGT
IMPLICIT NONE

PRIVATE
PUBLIC :: MarkdownData_

!----------------------------------------------------------------------------
!                                                             MarkdownData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-02
! summary: This user defined data type contains the markdown data
!
!# MarkdownData_
!
! This data types contains the data for markdown file. It is used for
! auto documentation of easifem library.

TYPE :: MarkdownData_
  PRIVATE
  TYPE(String) :: frontmatter
  !! frontmatter of markdown file
  TYPE(String) :: content
  !! content of markdown file
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of MarkdownData_
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate an instance of markdown data from agruments
  PROCEDURE, PUBLIC, PASS(obj) :: GetFrontmatter => obj_GetFrontmatter
  !! Get frontmatter
  PROCEDURE, PUBLIC, PASS(obj) :: GetContent => obj_GetContent
  !! Get content
  PROCEDURE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! copy the content into markdown data
END TYPE MarkdownData_

!----------------------------------------------------------------------------
!                                                       MarkdownDataPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-03
! summary: Data type for containing the pointer to MarkdownData_

TYPE :: MarkdownDataPointer_
  TYPE(MarkdownData_), POINTER :: ptr => NULL()
  !! pointer to MarkdownData_
END TYPE MarkdownDataPointer_

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-03
! summary: Display the content of MarkdownData

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(MarkdownData_), INTENT(INOUT) :: obj
    !! MarkdownData
    CHARACTER(*), INTENT(IN) :: msg
    !! message
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! unitno, default is standard output
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-04
! summary: Initiate an instance of MarkdownData from arguments
!
!# Initiate
!
! Initiate an instance of MarkdownData from agruments: frontmatter, and
! content.

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, frontmatter, content)
    CLASS(MarkdownData_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: frontmatter
    !! frontmatter of markdown file in yaml format
    TYPE(String), INTENT(IN) :: content
    !! content of markdown data
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetFrontMatter@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-09
! summary: Get the value of frontmatter

INTERFACE
  MODULE FUNCTION obj_GetFrontmatter(obj) RESULT(ans)
    CLASS(MarkdownData_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION obj_GetFrontmatter
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetContent@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-09
! summary: Get the value of content

INTERFACE
  MODULE FUNCTION obj_GetContent(obj) RESULT(ans)
    CLASS(MarkdownData_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION obj_GetContent
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Copy@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-10
! summary: Copy contents into MarkdownData

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(MarkdownData_), INTENT(INOUT) :: obj
    CLASS(MarkdownData_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MarkdownData_Class
