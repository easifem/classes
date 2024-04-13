
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

!> author: Vikas Sharma, Ph. D.
! date: 5 Aug 2022
! summary: Space of orthogonal polynomial is defined

MODULE AbstractOrthopolSpace1D_Class
USE String_Class, ONLY: String
USE GlobalData
USE AbstractBasis_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                   AbstractOrthopolSpace1D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Abstract orthogonal Basis are defined
!
!{!pages/AbstractOrthopolSpace1D_.md!}

TYPE, ABSTRACT :: AbstractOrthopolSpace1D_
  PRIVATE
  INTEGER(I4B) :: n = 0_I4B
    !! order of space
  LOGICAL(LGT) :: isMonic = .FALSE.
    !! are members monic
  LOGICAL(LGT) :: isOrthonormal = .FALSE.
    !! are members orthonormal
  REAL(DFP), ALLOCATABLE :: coeff(:, :)
    !! Recurrence coefficients for chebyshev polynomials
  REAL(DFP), ALLOCATABLE :: scale(:, :)
    !! Scaling for chebyshev polynomials
CONTAINS
  !!
  PRIVATE
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => Orthopol_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: setParam => Orthopol_setParam
END TYPE AbstractOrthopolSpace1D_

PUBLIC :: AbstractOrthopolSpace1D_

!----------------------------------------------------------------------------
!                                           AbstractOrthopolSpace1DPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractOrthopolSpace1DPointer_
  CLASS(AbstractOrthopolSpace1D_), POINTER :: ptr => NULL()
END TYPE AbstractOrthopolSpace1DPointer_

PUBLIC :: AbstractOrthopolSpace1DPointer_

!----------------------------------------------------------------------------
!                                            Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary:         Deallocate the object

INTERFACE
  MODULE SUBROUTINE Orthopol_Deallocate(obj)
    CLASS(AbstractOrthopolSpace1D_), INTENT(INOUT) :: obj
  END SUBROUTINE Orthopol_Deallocate
END INTERFACE

INTERFACE AbstractOrthopolSpace1DDeallocate
  MODULE PROCEDURE Orthopol_Deallocate
END INTERFACE AbstractOrthopolSpace1DDeallocate

PUBLIC :: AbstractOrthopolSpace1DDeallocate

!----------------------------------------------------------------------------
!                                                       setParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Aug 2022
! summary: Set the parameters

INTERFACE
  MODULE PURE SUBROUTINE Orthopol_SetParam(obj, n, isMonic, isOrthonormal, &
    & coeff, scale)
    CLASS(AbstractOrthopolSpace1D_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isMonic
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrthonormal
    REAL(DFP), OPTIONAL, INTENT(IN) :: coeff(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale(:, :)
  END SUBROUTINE Orthopol_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractOrthopolSpace1D_Class
