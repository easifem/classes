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

MODULE AbstractNodeField_Class
USE GlobalData
USE BaseType
USE RealVector_Method, ONLY : getPointer
USE AbstractField_Class
USE FPL, ONLY: ParameterList_
USE Domain_Class, ONLY: DomainPointer_
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                         AbstractNodeField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 Sept 2021
! summary: Abstract node field

TYPE, ABSTRACT, EXTENDS( AbstractField_ ) :: AbstractNodeField_
  INTEGER( I4B ) :: tSize = 0
    !! Total length of the nodal field = tdof * tNodes
  TYPE( RealVector_ ) :: realVec
    !! Vector of reals to contains the nodes
  TYPE( DOF_ ) :: dof
    !! Degree of freedom object, which contains the information about
    !! how the different components are stored inside the realVec
  CONTAINS
    PROCEDURE, PUBLIC, PASS( obj ) :: getPointer => anf_getPointer
    PROCEDURE, PUBLIC, PASS( obj ) :: Size => anf_Size
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate3 => anf_Initiate3
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => anf_DeallocateData
END TYPE AbstractNodeField_

PUBLIC :: AbstractNodeField_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractNodeFieldPointer_
  CLASS( AbstractNodeField_ ), POINTER :: ptr => NULL()
END TYPE AbstractNodeFieldPointer_

PUBLIC :: AbstractNodeFieldPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE AbstractNodeFieldDeallocateData
  MODULE PROCEDURE anf_DeallocateData
END INTERFACE AbstractNodeFieldDeallocateData

PUBLIC :: AbstractNodeFieldDeallocateData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                getPointer
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jul 2021
! summary: Returns the pointer to a fortran real vector stored inside realVec

FUNCTION anf_getPointer( obj ) RESULT( ans )
  CLASS( AbstractNodeField_ ), TARGET, INTENT( IN ) :: obj
  REAL( DFP ), POINTER :: ans( : )
  ans => getPointer( obj%realVec )
END FUNCTION anf_getPointer

!----------------------------------------------------------------------------
!                                                                    Size
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Sept 2021
! summary: This function returns the size of the field

FUNCTION anf_Size( obj, dims ) RESULT( ans )
  CLASS( AbstractNodeField_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL :: dims
  INTEGER( I4B ) :: ans
  ans = obj%tSize
END FUNCTION anf_Size

!----------------------------------------------------------------------------
!                                                            anf_Initiate3
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Sept 2021
! summary: This function initiates the block field, here it does nothing in
! particular

SUBROUTINE anf_Initiate3( obj, param, dom )
  CLASS( AbstractNodeField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( DomainPointer_ ), TARGET, INTENT( IN ) :: dom( : )
END SUBROUTINE anf_Initiate3

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

SUBROUTINE anf_DeallocateData( obj )
  CLASS( AbstractNodeField_ ), INTENT( INOUT ) :: obj
  !> main
  obj%tSize = 0
  CALL AbstractFieldDeallocateData(obj)
  CALL DeallocateData(obj%realVec)
  CALL DeallocateData(obj%dof)
END SUBROUTINE anf_DeallocateData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractNodeField_Class