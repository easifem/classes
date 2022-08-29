
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

!> author: Vikas Sharma, Ph. D.
! date: 5 Aug 2022
! summary: Space of Jacobi polynomial is defined

MODULE JacobiSpace1D_Class
USE String_Class, ONLY: String
USE GlobalData
USE AbstractOrthopolSpace1D_Class
USE Jacobi1D_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                            JacobiSpace1D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Chebyshev1 orthogonal Basis are defined
!
!{!pages/JacobiSpace1D_.md!}

TYPE, EXTENDS(AbstractOrthopolSpace1D_) :: JacobiSpace1D_
  PRIVATE
  TYPE(Jacobi1D_) :: x
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => Orthopol_Deallocate
  FINAL :: Orthopol_Final
END TYPE JacobiSpace1D_

PUBLIC :: JacobiSpace1D_

!----------------------------------------------------------------------------
!                                                      JacobiSpace1DPointer_
!----------------------------------------------------------------------------

TYPE :: JacobiSpace1DPointer_
  CLASS(JacobiSpace1D_), POINTER :: ptr => NULL()
END TYPE JacobiSpace1DPointer_

PUBLIC :: JacobiSpace1DPointer_

!----------------------------------------------------------------------------
!                                           JacobiSpace1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary: Constructor for Chebyshev Basis of first kind

INTERFACE
  MODULE FUNCTION JacobiSpace1D1(varname, n, alpha, beta, isMonic, &
      & isOrthonormal) RESULT(ans)
    CHARACTER(LEN=*), INTENT(IN) :: varname
    !! variable name
    INTEGER(I4B), INTENT(IN) :: n
    !! order of chebyshev Basis
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isMonic
    !! Default is .FALSE., if true then leading coeff of poly is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrthonormal
    !! Default is .FALSE., if true then the Basiss are orthonormal
    TYPE(JacobiSpace1D_) :: ans
    !! Chebyshev Basis of first kind
  END FUNCTION JacobiSpace1D1
END INTERFACE

INTERFACE JacobiSpace1D
  MODULE PROCEDURE JacobiSpace1D1
END INTERFACE JacobiSpace1D

PUBLIC :: JacobiSpace1D

!----------------------------------------------------------------------------
!                                   JacobiSpace1D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary: Constructor for Chebyshev1 Basis of first kind

INTERFACE
  MODULE FUNCTION JacobiSpace1D_Pointer1(varname, n, alpha, beta, &
    & isMonic, isOrthonormal) RESULT(ans)
    CHARACTER(LEN=*), INTENT(IN) :: varname
    !! variable name
    INTEGER(I4B), INTENT(IN) :: n
    !! order of chebyshev Basis
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isMonic
    !! Default is .FALSE., if true then leading coeff of poly is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isOrthonormal
    !! Default is .FALSE., if true then the Basiss are orthonormal
    CLASS(JacobiSpace1D_), POINTER :: ans
    !! Chebyshev Basis of first kind
  END FUNCTION JacobiSpace1D_Pointer1
END INTERFACE

INTERFACE JacobiSpace1D_Pointer
  MODULE PROCEDURE JacobiSpace1D_Pointer1
END INTERFACE JacobiSpace1D_Pointer

PUBLIC :: JacobiSpace1D_Pointer

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary:         Deallocate the object

INTERFACE
  MODULE SUBROUTINE Orthopol_Deallocate(obj)
    CLASS(JacobiSpace1D_), INTENT(INOUT) :: obj
  END SUBROUTINE Orthopol_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary:         Finalizer for chebyshev space

INTERFACE
  MODULE SUBROUTINE Orthopol_Final(obj)
    TYPE(JacobiSpace1D_), INTENT(INOUT) :: obj
  END SUBROUTINE Orthopol_Final
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE JacobiSpace1D_Class
