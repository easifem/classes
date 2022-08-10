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

MODULE Test_ReferencePyramid_Class
USE GlobalData
USE Test_ReferenceElement_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="Test_ReferencePyramid_Class"

!----------------------------------------------------------------------------
!                                                   Test_ReferencePyramid_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: 	ReferencePyramid class is defined
!
!{!pages/ReferencePyramid_.md!}

TYPE, EXTENDS( Test_ReferenceElement_ ) :: Test_ReferencePyramid_
END TYPE Test_ReferencePyramid_

PUBLIC :: Test_ReferencePyramid_

!----------------------------------------------------------------------------
!                                            Test_ReferencePyramidPointer_
!----------------------------------------------------------------------------

TYPE :: Test_ReferencePyramidPointer_
  CLASS(Test_ReferencePyramid_), POINTER :: ptr => NULL()
END TYPE Test_ReferencePyramidPointer_

PUBLIC :: Test_ReferencePyramidPointer_

!----------------------------------------------------------------------------
!                               Test_ReferencePyramid@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 aug 2022
! summary: Constructor function for Reference point

INTERFACE
MODULE FUNCTION refelem_ReferencePyramid( nsd, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: nsd
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  TYPE( Test_ReferencePyramid_ ) :: ans
END FUNCTION refelem_ReferencePyramid
END INTERFACE

INTERFACE Test_ReferencePyramid
  MODULE PROCEDURE refelem_ReferencePyramid
END INTERFACE Test_ReferencePyramid

PUBLIC :: Test_ReferencePyramid

!----------------------------------------------------------------------------
!                        Test_ReferencePyramid_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Constructor function for ReferencePyramid

INTERFACE
MODULE FUNCTION refelem_ReferencePyramid_Pointer( nsd, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: nsd
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  CLASS( Test_ReferencePyramid_ ), POINTER :: ans
END FUNCTION refelem_ReferencePyramid_Pointer
END INTERFACE

INTERFACE Test_ReferencePyramid_Pointer
  MODULE PROCEDURE refelem_ReferencePyramid_Pointer
END INTERFACE Test_ReferencePyramid_Pointer

PUBLIC :: Test_ReferencePyramid_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Test_ReferencePyramid_Class