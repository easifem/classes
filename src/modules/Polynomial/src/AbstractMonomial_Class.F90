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

MODULE AbstractMonomial_Class
USE GlobalData
USE AbstractFunction_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "AbstractMonomial_Class"

!----------------------------------------------------------------------------
!                                                          AbstractMonomial_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: AbstractMonomial class is defined

TYPE, ABSTRACT, EXTENDS( AbstractFunction_ ) :: AbstractMonomial_
  REAL( DFP ) :: coeff = 0.0_DFP
  INTEGER( I4B ) :: uid = 0
  CONTAINS
    PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => func_Deallocate
END TYPE AbstractMonomial_

PUBLIC :: AbstractMonomial_

!----------------------------------------------------------------------------
!                                                  AbstractMonomialPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractMonomialPointer_
  CLASS( AbstractMonomial_ ), POINTER :: ptr => NULL()
END TYPE AbstractMonomialPointer_

PUBLIC :: AbstractMonomialPointer_

!----------------------------------------------------------------------------
!                                                      Deallocate@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate the function

INTERFACE
  MODULE SUBROUTINE func_Deallocate( obj )
    CLASS( AbstractMonomial_ ), INTENT( INOUT ) :: obj
  END SUBROUTINE func_Deallocate
END INTERFACE

INTERFACE AbstractMonomialDeallocate
  MODULE PROCEDURE func_Deallocate
END INTERFACE AbstractMonomialDeallocate

PUBLIC :: AbstractMonomialDeallocate

END MODULE AbstractMonomial_Class