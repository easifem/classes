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

MODULE Test_ReferenceTetrahedron_Class
USE GlobalData
USE Test_ReferenceElement_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="Test_ReferenceTetrahedron_Class"

!----------------------------------------------------------------------------
!                                                   Test_ReferenceTetrahedron_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: 	ReferenceTetrahedron class is defined
!
!{!pages/ReferenceTetrahedron_.md!}

TYPE, EXTENDS( Test_ReferenceElement_ ) :: Test_ReferenceTetrahedron_
END TYPE Test_ReferenceTetrahedron_

PUBLIC :: Test_ReferenceTetrahedron_

!----------------------------------------------------------------------------
!                                            Test_ReferenceTetrahedronPointer_
!----------------------------------------------------------------------------

TYPE :: Test_ReferenceTetrahedronPointer_
  CLASS(Test_ReferenceTetrahedron_), POINTER :: ptr => NULL()
END TYPE Test_ReferenceTetrahedronPointer_

PUBLIC :: Test_ReferenceTetrahedronPointer_

!----------------------------------------------------------------------------
!                               Test_ReferenceTetrahedron@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 aug 2022
! summary: Constructor function for Reference point

INTERFACE
MODULE FUNCTION refelem_ReferenceTetrahedron( nsd, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: nsd
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  TYPE( Test_ReferenceTetrahedron_ ) :: ans
END FUNCTION refelem_ReferenceTetrahedron
END INTERFACE

INTERFACE Test_ReferenceTetrahedron
  MODULE PROCEDURE refelem_ReferenceTetrahedron
END INTERFACE Test_ReferenceTetrahedron

PUBLIC :: Test_ReferenceTetrahedron

!----------------------------------------------------------------------------
!                        Test_ReferenceTetrahedron_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Constructor function for ReferenceTetrahedron

INTERFACE
MODULE FUNCTION refelem_ReferenceTetrahedron_Pointer( nsd, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: nsd
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  CLASS( Test_ReferenceTetrahedron_ ), POINTER :: ans
END FUNCTION refelem_ReferenceTetrahedron_Pointer
END INTERFACE

INTERFACE Test_ReferenceTetrahedron_Pointer
  MODULE PROCEDURE refelem_ReferenceTetrahedron_Pointer
END INTERFACE Test_ReferenceTetrahedron_Pointer

PUBLIC :: Test_ReferenceTetrahedron_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Test_ReferenceTetrahedron_Class