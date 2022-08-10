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

MODULE Test_ReferenceTriangle_Class
USE GlobalData
USE Test_ReferenceElement_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="Test_ReferenceTriangle_Class"

!----------------------------------------------------------------------------
!                                                   Test_ReferenceTriangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: 	ReferenceTriangle class is defined
!
!{!pages/ReferenceTriangle_.md!}

TYPE, EXTENDS( Test_ReferenceElement_ ) :: Test_ReferenceTriangle_
END TYPE Test_ReferenceTriangle_

PUBLIC :: Test_ReferenceTriangle_

!----------------------------------------------------------------------------
!                                            Test_ReferenceTrianglePointer_
!----------------------------------------------------------------------------

TYPE :: Test_ReferenceTrianglePointer_
  CLASS(Test_ReferenceTriangle_), POINTER :: ptr => NULL()
END TYPE Test_ReferenceTrianglePointer_

PUBLIC :: Test_ReferenceTrianglePointer_

!----------------------------------------------------------------------------
!                                    Test_ReferenceTriangle@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 aug 2022
! summary: Constructor function for Reference point

INTERFACE
MODULE FUNCTION refelem_ReferenceTriangle( nsd, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: nsd
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  TYPE( Test_ReferenceTriangle_ ) :: ans
END FUNCTION refelem_ReferenceTriangle
END INTERFACE

INTERFACE Test_ReferenceTriangle
  MODULE PROCEDURE refelem_ReferenceTriangle
END INTERFACE Test_ReferenceTriangle

PUBLIC :: Test_ReferenceTriangle

!----------------------------------------------------------------------------
!                             Test_ReferenceTriangle_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Constructor function for ReferenceTriangle

INTERFACE
MODULE FUNCTION refelem_ReferenceTriangle_Pointer( nsd, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: nsd
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  CLASS( Test_ReferenceTriangle_ ), POINTER :: ans
END FUNCTION refelem_ReferenceTriangle_Pointer
END INTERFACE

INTERFACE Test_ReferenceTriangle_Pointer
  MODULE PROCEDURE refelem_ReferenceTriangle_Pointer
END INTERFACE Test_ReferenceTriangle_Pointer

PUBLIC :: Test_ReferenceTriangle_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Test_ReferenceTriangle_Class