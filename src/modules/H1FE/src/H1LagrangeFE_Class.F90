! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

MODULE H1LagrangeFE_Class
USE AbstractFE_Class, ONLY: AbstractFE_

IMPLICIT NONE

PRIVATE

PUBLIC :: H1LagrangeFE_

CHARACTER(*), PARAMETER :: modName = "H1LagrangeFE_Class"

!----------------------------------------------------------------------------
!                                                              H1LagrangeFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-09
! summary:  Scalar H1 Lagrange Finite Element

TYPE, ABSTRACT, EXTENDS(AbstractFE_) :: H1LagrangeFE_
END TYPE H1LagrangeFE_

!----------------------------------------------------------------------------
!                                                       H1LagrangeFEPointer_
!----------------------------------------------------------------------------

TYPE :: H1LagrangeFEPointer_
  CLASS(H1LagrangeFE_), POINTER :: ptr => NULL()
END TYPE H1LagrangeFEPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE H1LagrangeFE_Class
