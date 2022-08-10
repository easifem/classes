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
! date: 9 Aug 2022
! summary: 	Reference element for triangle is implemented

MODULE Test_ReferenceHexahedron_Class
USE GlobalData
USE Test_ReferenceElement_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="Test_ReferenceHexahedron_Class"

!----------------------------------------------------------------------------
!                                                   Test_ReferenceHexahedron_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: 	ReferenceHexahedron class is defined
!
!{!pages/ReferenceHexahedron_.md!}

TYPE, EXTENDS( Test_ReferenceElement_ ) :: Test_ReferenceHexahedron_
END TYPE Test_ReferenceHexahedron_

PUBLIC :: Test_ReferenceHexahedron_

!----------------------------------------------------------------------------
!                                            Test_ReferenceHexahedronPointer_
!----------------------------------------------------------------------------

TYPE :: Test_ReferenceHexahedronPointer_
  CLASS(Test_ReferenceHexahedron_), POINTER :: ptr => NULL()
END TYPE Test_ReferenceHexahedronPointer_

PUBLIC :: Test_ReferenceHexahedronPointer_

!----------------------------------------------------------------------------
!                               Test_ReferenceHexahedron@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 aug 2022
! summary: Constructor function for Reference point

INTERFACE
MODULE FUNCTION refelem_ReferenceHexahedron( nsd, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: nsd
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  TYPE( Test_ReferenceHexahedron_ ) :: ans
END FUNCTION refelem_ReferenceHexahedron
END INTERFACE

INTERFACE Test_ReferenceHexahedron
  MODULE PROCEDURE refelem_ReferenceHexahedron
END INTERFACE Test_ReferenceHexahedron

PUBLIC :: Test_ReferenceHexahedron

!----------------------------------------------------------------------------
!                        Test_ReferenceHexahedron_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Constructor function for ReferenceHexahedron

INTERFACE
MODULE FUNCTION refelem_ReferenceHexahedron_Pointer( nsd, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: nsd
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  CLASS( Test_ReferenceHexahedron_ ), POINTER :: ans
END FUNCTION refelem_ReferenceHexahedron_Pointer
END INTERFACE

INTERFACE Test_ReferenceHexahedron_Pointer
  MODULE PROCEDURE refelem_ReferenceHexahedron_Pointer
END INTERFACE Test_ReferenceHexahedron_Pointer

PUBLIC :: Test_ReferenceHexahedron_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Test_ReferenceHexahedron_Class