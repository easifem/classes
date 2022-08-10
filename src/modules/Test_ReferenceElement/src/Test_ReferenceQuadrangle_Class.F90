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

MODULE Test_ReferenceQuadrangle_Class
USE GlobalData
USE Test_ReferenceElement_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="Test_ReferenceQuadrangle_Class"

!----------------------------------------------------------------------------
!                                                   Test_ReferenceQuadrangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: 	ReferenceQuadrangle class is defined
!
!{!pages/ReferenceQuadrangle_.md!}

TYPE, EXTENDS( Test_ReferenceElement_ ) :: Test_ReferenceQuadrangle_
END TYPE Test_ReferenceQuadrangle_

PUBLIC :: Test_ReferenceQuadrangle_

!----------------------------------------------------------------------------
!                                            Test_ReferenceQuadranglePointer_
!----------------------------------------------------------------------------

TYPE :: Test_ReferenceQuadranglePointer_
  CLASS(Test_ReferenceQuadrangle_), POINTER :: ptr => NULL()
END TYPE Test_ReferenceQuadranglePointer_

PUBLIC :: Test_ReferenceQuadranglePointer_

!----------------------------------------------------------------------------
!                               Test_ReferenceQuadrangle@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 aug 2022
! summary: Constructor function for Reference point

INTERFACE
MODULE FUNCTION refelem_ReferenceQuadrangle( nsd, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: nsd
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  TYPE( Test_ReferenceQuadrangle_ ) :: ans
END FUNCTION refelem_ReferenceQuadrangle
END INTERFACE

INTERFACE Test_ReferenceQuadrangle
  MODULE PROCEDURE refelem_ReferenceQuadrangle
END INTERFACE Test_ReferenceQuadrangle

PUBLIC :: Test_ReferenceQuadrangle

!----------------------------------------------------------------------------
!                        Test_ReferenceQuadrangle_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Constructor function for ReferenceQuadrangle

INTERFACE
MODULE FUNCTION refelem_ReferenceQuadrangle_Pointer( nsd, xij ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: nsd
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  CLASS( Test_ReferenceQuadrangle_ ), POINTER :: ans
END FUNCTION refelem_ReferenceQuadrangle_Pointer
END INTERFACE

INTERFACE Test_ReferenceQuadrangle_Pointer
  MODULE PROCEDURE refelem_ReferenceQuadrangle_Pointer
END INTERFACE Test_ReferenceQuadrangle_Pointer

PUBLIC :: Test_ReferenceQuadrangle_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Test_ReferenceQuadrangle_Class