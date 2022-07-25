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

MODULE Test_Topolgy_Class
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                            Test_Topology_
!----------------------------------------------------------------------------

TYPE :: Test_Topolgy_
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  INTEGER(I4B) :: name = 0
  INTEGER(I4B) :: xiDimension = 0
END TYPE Test_Topolgy_

PUBLIC :: Test_Topolgy_

!----------------------------------------------------------------------------
!                                                     Test_TopologyPointer_
!----------------------------------------------------------------------------

TYPE :: Test_TopologyPointer_
  CLASS(Test_Topology_), POINTER :: ptr => NULL()
END TYPE Test_TopologyPointer_

PUBLIC :: Test_TopologyPointer_

END MODULE Test_Topolgy_Class