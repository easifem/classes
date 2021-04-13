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

MODULE FE
USE BaseType
USE GlobalData
USE Element_Class
USE FacetElement_Class
IMPLICIT NONE
PUBLIC

PRIVATE :: Element_Factory
INTERFACE Factory
  MODULE PROCEDURE Element_Factory
END INTERFACE Factory
PUBLIC :: Factory

CONTAINS

!----------------------------------------------------------------------------
!                                                              getFEPointer
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Dynamically returns the finite element

FUNCTION getFEPointer( Obj, Nptrs, Mat_Type, RefElem ) RESULT( Ans )
  ! Define internal variable
  CLASS( Element_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN) :: Nptrs( : ), Mat_Type
  CLASS( ReferenceElement_ ), TARGET, INTENT( IN ) :: RefElem
  CLASS( Element_ ), POINTER :: Ans
  !
  SELECT TYPE( Obj )
  TYPE IS ( Element_ )
    Ans => Element_Pointer( Nptrs, Mat_Type, RefElem )
  TYPE IS ( FacetElement_ )
    Ans => FacetElement_Pointer( Nptrs, Mat_Type, RefElem )
  END SELECT
END FUNCTION getFEPointer

!----------------------------------------------------------------------------
!                                                                 Factory
!----------------------------------------------------------------------------

FUNCTION Element_Factory( Obj ) RESULT( Ans )
  CLASS( Element_ ), INTENT( IN ) :: Obj
  CLASS( Element_ ), POINTER :: Ans
  SELECT TYPE( Obj )
  TYPE IS( Element_ )
    ALLOCATE( Element_ :: Ans )
    CALL Ans%Initiate( Obj )
  TYPE IS( FacetElement_ )
    ALLOCATE( FacetElement_ :: Ans )
    CALL Ans%Initiate( Obj )
  END SELECT
END FUNCTION Element_Factory

END MODULE FE
