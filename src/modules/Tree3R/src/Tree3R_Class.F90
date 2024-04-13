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

MODULE Tree3R_Class
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                   Tree3R_
!----------------------------------------------------------------------------

TYPE :: Tree3R_
  PRIVATE
  INTEGER(I4B) :: n = 0
  REAL(DFP) :: leftCoeff = 0.0_DFP
  REAL(DFP) :: rightCoeff = 0.0_DFP
  CLASS(Tree3R_), POINTER :: left => NULL()
  CLASS(Tree3R_), POINTER :: right => NULL()
CONTAINS
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => TR_Deallocate
  FINAL :: TR_Final
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => TR_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: isInitiated => TR_isInitiated
  !!
  !! @IOMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Display => TR_Display
  !!
END TYPE Tree3R_

PUBLIC :: Tree3R_

!----------------------------------------------------------------------------
!                                                            Tree3RPointer_
!----------------------------------------------------------------------------

TYPE :: Tree3RPointer_
  CLASS(Tree3R_), POINTER :: ptr => NULL()
END TYPE Tree3RPointer_

PUBLIC :: Tree3RPointer_

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Deallocate the tree

INTERFACE
  MODULE RECURSIVE SUBROUTINE TR_Deallocate(obj)
    CLASS(Tree3R_), INTENT(INOUT) :: obj
  END SUBROUTINE TR_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE TR_Final(obj)
    TYPE(Tree3R_), INTENT(INOUT) :: obj
  END SUBROUTINE TR_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Build the 3 term recurrence tree

INTERFACE
  MODULE RECURSIVE SUBROUTINE TR_Initiate(obj, n, leftCoeff, rightCoeff, &
    & lastLeft, lastRight)
    CLASS(Tree3R_), INTENT(INOUT) :: obj
    !! tree to be built
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: leftCoeff(0:n - 1)
    !! recurrence coefficient for the n-1 term
    REAL(DFP), INTENT(IN) :: rightCoeff(0:n - 1)
    !! recurrence coefficient for the nth term
    CLASS(Tree3R_), TARGET, INTENT(IN) :: lastLeft
    !! left element at the bottom of the tree
    CLASS(Tree3R_), TARGET, INTENT(IN) :: lastRight
    !! right element at the bottom of the tree
  END SUBROUTINE TR_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                             isInitiated@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: If left or right is associated then it returns true

INTERFACE
  MODULE ELEMENTAL FUNCTION TR_isInitiated(obj) RESULT(ans)
    CLASS(Tree3R_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION TR_isInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Display the recurrence tree

INTERFACE
  MODULE RECURSIVE SUBROUTINE TR_Display(obj, msg, unitno)
    CLASS(Tree3R_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE TR_Display
END INTERFACE

END MODULE Tree3R_Class
