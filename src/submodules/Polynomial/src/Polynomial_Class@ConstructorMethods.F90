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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBMODULE(Polynomial_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE func_CheckEssentialParam
  CHARACTER(LEN=*), PARAMETER :: myName = "func_CheckEssentialParam"
  INTEGER(I4B) :: ii
  INTEGER(I4B), PARAMETER :: maxEssentialParam = 4
  TYPE(String) :: essentialParam(maxEssentialParam)
  !!
  !! main
  !!
  essentialParam(1) = "Polynomial/coeff"
  essentialParam(2) = "Polynomial/power"
  essentialParam(3) = "Polynomial/tVariables"
  essentialParam(4) = "Polynomial/tCoeff"
  !!
  DO ii = 1, maxEssentialParam
    IF (.NOT. param%isPresent(key=TRIM(essentialParam(ii)%chars()))) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & TRIM(essentialParam(ii)%chars())//' should be present in param')
    END IF
  END DO
  !!
END PROCEDURE func_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                         setPlynomialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setPolynomialParam
  INTEGER( I4B ) :: ierr
  ierr = param%set( key="Polynomial/coeff", value=coeff)
  ierr = param%set( key="Polynomial/power", value=power)
  ierr = param%set( key="Polynomial/tVariables", &
    & value=SIZE(power, dim=2, KIND=I4B) )
  ierr = param%set( key="Polynomial/tCoeff", &
    & value=SIZE(power, dim=1, KIND=I4B) )
END PROCEDURE setPolynomialParam

!----------------------------------------------------------------------------
!                                                                 Polynomial
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Polynomial1
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: power( :, : )
  INTEGER( I4B ) :: N, ierr, tVariables, tCoeff
  !!
  !! main
  !!
  CALL ans%CheckEssentialParam( param )
  ierr = param%get(key="Polynomial/tVariables", VALUE=tVariables)
  ierr = param%get(key="Polynomial/tCoeff", VALUE=tCoeff)
  ALLOCATE( coeff( tCoeff ), power( tCoeff, tVariables ) )
  ierr = param%get(key="Polynomial/coeff", VALUE=coeff)
  ierr = param%get(key="Polynomial/power", VALUE=power)
  !!
  ans%coeff = coeff
  ans%power = power
  !!
  DEALLOCATE( coeff, power )
  !!
END PROCEDURE func_Polynomial1

!----------------------------------------------------------------------------
!                                                         Polynomial_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Polynomial_Pointer1
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: power( :, : )
  INTEGER( I4B ) :: N, ierr
  !!
  !! main
  !!
  CALL ans%CheckEssentialParam( param )
  ierr = param%get(key="Polynomial/coeff", VALUE=coeff)
  ierr = param%get(key="Polynomial/power", VALUE=power)
  !!
  ALLOCATE( ans )
  ans%coeff = coeff
  ans%power = power
  !!
  DEALLOCATE( coeff, power )
  !!
END PROCEDURE func_Polynomial_Pointer1

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate
  IF( ALLOCATED( obj%coeff ) ) DEALLOCATE( obj%coeff )
  IF( ALLOCATED( obj%power ) ) DEALLOCATE( obj%power )
END PROCEDURE func_Deallocate

MODULE PROCEDURE func_Final
  CALL obj%Deallocate()
END PROCEDURE func_Final

END SUBMODULE ConstructorMethods