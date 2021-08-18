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
USE RealVector_Method, ONLY : getPointer
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
  CONTAINS
    PROCEDURE, PUBLIC, PASS( obj ) :: getPointer => anf_getPointer
    PROCEDURE, PUBLIC, PASS( obj ) :: size => anf_Size
END TYPE AbstractNodeField_

PUBLIC :: AbstractNodeField_

CONTAINS

!----------------------------------------------------------------------------
!                                                                getPointer
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Jul 2021
! summary: This routine returns the pointer to a fortran real vector stored inside realVec

FUNCTION anf_getPointer( obj ) RESULT( ans )
  CLASS( AbstractNodeField_ ), TARGET, INTENT( IN ) :: obj
  REAL( DFP ), POINTER :: ans( : )
  ans => getPointer( obj%realVec )
END FUNCTION anf_getPointer


!----------------------------------------------------------------------------
!                                                                    Size
!----------------------------------------------------------------------------

FUNCTION anf_Size( obj, dims ) RESULT( ans )
  CLASS( AbstractNodeField_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL :: dims
  INTEGER( I4B ) :: ans
  ans = obj%tSize
END FUNCTION anf_Size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractNodeField_Class