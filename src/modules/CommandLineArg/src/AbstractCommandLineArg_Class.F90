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
! date: 2021-11-08
! update: 2021-11-08
! summary: Abstract command line argument class

MODULE AbstractCommandLine_Class
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : stdout=>output_unit, stderr=>error_unit
USE PENF
IMPLICIT NONE
PRIVATE
SAVE

!----------------------------------------------------------------------------
!                                                     AbstractCommandLineArg_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-08
! update: 2021-11-08
! summary: AbstractCommandLine class
!
!{!pages/AbstractCommandLineArg_.md!}

TYPE, ABSTRACT :: AbstractCommandLineArg_
  CHARACTER(len=:), ALLOCATABLE :: progname
  !! Program name.
  CHARACTER(len=:), ALLOCATABLE :: version
  !! Program version.
  CHARACTER(len=:), ALLOCATABLE :: help
  !! Help message.
  CHARACTER(len=:), ALLOCATABLE :: help_color
  !! ANSI color of help messages.
  CHARACTER(len=:), ALLOCATABLE :: help_style
  !! ANSI style of help messages.
  CHARACTER(len=:), ALLOCATABLE :: help_markdown
  !! Longer help message, markdown formatted.
  CHARACTER(len=:), ALLOCATABLE :: description
  !! Detailed description.
  CHARACTER(len=:), ALLOCATABLE :: license
  !! License description.
  CHARACTER(len=:), ALLOCATABLE :: authors
  !! Authors list.
  CHARACTER(len=:), ALLOCATABLE :: epilog
  !! Epilogue message.
  CHARACTER(len=:), ALLOCATABLE :: m_exclude
  !! Mutually exclude other CLA(s group).
  CHARACTER(len=:), ALLOCATABLE :: error_message
  !! Meaningful error message to standard-error.
  CHARACTER(len=:), ALLOCATABLE :: error_color
  !! ANSI color of error messages.
  CHARACTER(len=:), ALLOCATABLE :: error_style
  !! ANSI style of error messages.
#ifdef __GFORTRAN__
  CHARACTER(512), ALLOCATABLE :: examples(:)
  !! Examples of correct usage.
#else
  CHARACTER(len=:), ALLOCATABLE :: examples(:)
  !! Examples of correct usage.
#endif
  INTEGER(I4P) :: error = 0_I4P
  !! trapping flag.
  INTEGER(I4P) :: usage_lun = stderr
  !! Output unit to print help/usage messages
  INTEGER(I4P) :: version_lun = stdout
  !! Output unit to print version message
  INTEGER(I4P) :: error_lun = stderr
  !! Error unit to print error messages
CONTAINS
  ! public methods
  PROCEDURE, PASS(self) :: free_object
  !! Free dynamic memory.
  PROCEDURE, PASS(self) :: print_version
  !! Print version.
  PROCEDURE, PASS(self) :: print_error_message
  !! Print meaningful error message.
  PROCEDURE, PASS(self) :: set_examples
  !! Set examples of correct usage.
  PROCEDURE, PASS(lhs) :: assign_object
  !! Assignment overloading.
END TYPE AbstractCommandLineArg_

PUBLIC :: AbstractCommandLineArg_

!----------------------------------------------------------------------------
!                                                                  Contains
!----------------------------------------------------------------------------

CONTAINS

ELEMENTAL SUBROUTINE free_object(self)
  !! Free dynamic memory.
  CLASS(AbstractCommandLineArg_), INTENT(inout) :: self !! Object data.

  IF (ALLOCATED(self%progname)) DEALLOCATE (self%progname)
  IF (ALLOCATED(self%version)) DEALLOCATE (self%version)
  IF (ALLOCATED(self%help)) DEALLOCATE (self%help)
  IF (ALLOCATED(self%help_color)) DEALLOCATE (self%help_color)
  IF (ALLOCATED(self%help_style)) DEALLOCATE (self%help_style)
  IF (ALLOCATED(self%help_markdown)) DEALLOCATE (self%help_markdown)
  IF (ALLOCATED(self%description)) DEALLOCATE (self%description)
  IF (ALLOCATED(self%license)) DEALLOCATE (self%license)
  IF (ALLOCATED(self%authors)) DEALLOCATE (self%authors)
  IF (ALLOCATED(self%epilog)) DEALLOCATE (self%epilog)
  IF (ALLOCATED(self%m_exclude)) DEALLOCATE (self%m_exclude)
  IF (ALLOCATED(self%error_message)) DEALLOCATE (self%error_message)
  IF (ALLOCATED(self%error_color)) DEALLOCATE (self%error_color)
  IF (ALLOCATED(self%error_style)) DEALLOCATE (self%error_style)
  self%error = 0_I4P
  self%usage_lun = stderr
  self%version_lun = stdout
  self%error_lun = stderr
END SUBROUTINE free_object

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE print_version(self, pref)
  !! Print version.
  CLASS(AbstractCommandLineArg_), INTENT(in) :: self
  !! Object data.
  CHARACTER(*), INTENT(in), OPTIONAL :: pref
  !! Prefixing string.
  CHARACTER(len=:), ALLOCATABLE :: prefd
  !! Prefixing string.

  prefd = ''; IF (PRESENT(pref)) prefd = pref
  WRITE(self%version_lun,'(A)')prefd//self%progname//' version '//self%version
  IF (self%license /= '') THEN
    WRITE (self%version_lun, '(A)') prefd//self%license
  END IF
  IF (self%authors /= '') THEN
    WRITE (self%version_lun, '(A)') prefd//self%authors
  END IF
END SUBROUTINE print_version

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE print_error_message(self)
  !! Print meaningful error message to standard-error.
  CLASS(AbstractCommandLineArg_), INTENT(in) :: self !! Object data.

  WRITE (self%error_lun, '(A)') self%error_message
  WRITE (self%error_lun, '(A)')
END SUBROUTINE print_error_message

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_examples(self, examples)
  !! Set examples of correct usage.
  CLASS(AbstractCommandLineArg_), INTENT(inout) :: self
  !! Object data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: examples(1:)
  !! Examples of correct usage.

  IF (PRESENT(examples)) THEN
#ifdef __GFORTRAN__
    ALLOCATE (self%examples(1:SIZE(examples)))
#else
 ALLOCATE (CHARACTER(len=LEN(examples(1))) :: self%examples(1:SIZE(examples)))
! does not work with gfortran 4.9.2
#endif
    self%examples = examples
  END IF
END SUBROUTINE set_examples

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ELEMENTAL SUBROUTINE assign_object(lhs, rhs)
  !! Assign two abstract objects.
  CLASS(AbstractCommandLineArg_), INTENT(inout) :: lhs
  !! Left hand side.
  CLASS(AbstractCommandLineArg_), INTENT(in) :: rhs
  !! Rigth hand side.

  IF (ALLOCATED(rhs%progname)) lhs%progname = rhs%progname
  IF (ALLOCATED(rhs%version)) lhs%version = rhs%version
  IF (ALLOCATED(rhs%help)) lhs%help = rhs%help
  IF (ALLOCATED(rhs%help_color)) lhs%help_color = rhs%help_color
  IF (ALLOCATED(rhs%help_style)) lhs%help_style = rhs%help_style
  IF (ALLOCATED(rhs%help_markdown)) lhs%help_markdown = rhs%help_markdown
  IF (ALLOCATED(rhs%description)) lhs%description = rhs%description
  IF (ALLOCATED(rhs%license)) lhs%license = rhs%license
  IF (ALLOCATED(rhs%authors)) lhs%authors = rhs%authors
  IF (ALLOCATED(rhs%epilog)) lhs%epilog = rhs%epilog
  IF (ALLOCATED(rhs%m_exclude)) lhs%m_exclude = rhs%m_exclude
  IF (ALLOCATED(rhs%error_message)) lhs%error_message = rhs%error_message
  IF (ALLOCATED(rhs%error_color)) lhs%error_color = rhs%error_color
  IF (ALLOCATED(rhs%error_style)) lhs%error_style = rhs%error_style
  lhs%error = rhs%error
  lhs%usage_lun = rhs%usage_lun
  lhs%version_lun = rhs%version_lun
  lhs%error_lun = rhs%error_lun
END SUBROUTINE assign_object
ENDMODULE AbstractCommandLine_Class
