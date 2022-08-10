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

MODULE Test_ReferencePrism_Class
USE GlobalData
USE Test_ReferenceElement_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="Test_ReferencePrism_Class"

!----------------------------------------------------------------------------
!                                                   Test_ReferencePrism_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: 	ReferencePrism class is defined
!
!{!pages/ReferencePrism_.md!}

TYPE, EXTENDS( Test_ReferenceElement_ ) :: Test_ReferencePrism_
END TYPE Test_ReferencePrism_

PUBLIC :: Test_ReferencePrism_

!----------------------------------------------------------------------------
!                                            Test_ReferencePrismPointer_
!----------------------------------------------------------------------------

TYPE :: Test_ReferencePrismPointer_
  CLASS(Test_ReferencePrism_), POINTER :: ptr => NULL()
END TYPE Test_ReferencePrismPointer_

PUBLIC :: Test_ReferencePrismPointer_

!----------------------------------------------------------------------------
!                               Test_ReferencePrism@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 aug 2022
! summary: Constructor function for Reference point

INTERFACE
MODULE FUNCTION refelem_ReferencePrism( nsd, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: nsd
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  TYPE( Test_ReferencePrism_ ) :: ans
END FUNCTION refelem_ReferencePrism
END INTERFACE

INTERFACE Test_ReferencePrism
  MODULE PROCEDURE refelem_ReferencePrism
END INTERFACE Test_ReferencePrism

PUBLIC :: Test_ReferencePrism

!----------------------------------------------------------------------------
!                        Test_ReferencePrism_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Constructor function for ReferencePrism

INTERFACE
MODULE FUNCTION refelem_ReferencePrism_Pointer( nsd, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: nsd
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  CLASS( Test_ReferencePrism_ ), POINTER :: ans
END FUNCTION refelem_ReferencePrism_Pointer
END INTERFACE

INTERFACE Test_ReferencePrism_Pointer
  MODULE PROCEDURE refelem_ReferencePrism_Pointer
END INTERFACE Test_ReferencePrism_Pointer

PUBLIC :: Test_ReferencePrism_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Test_ReferencePrism_Class