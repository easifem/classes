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

MODULE AbstractNodeField_Class
USE GlobalData
USE BaseType
USE AbstractField_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                         AbstractNodeField_
!----------------------------------------------------------------------------

TYPE, ABSTRACT, EXTENDS( AbstractField_ ) :: AbstractNodeField_
  INTEGER( I4B ) :: tSize = 0
    !! Total length of the nodal field = tdof * tNodes
  TYPE( RealVector_ ) :: realVec
    !! Vector of reals to contains the nodes
  TYPE( DOF_ ) :: dof
    !! Degree of freedom object, which contains the information about how the different components are stored inside the realVec
END TYPE AbstractNodeField_

PUBLIC :: AbstractNodeField_

END MODULE AbstractNodeField_Class