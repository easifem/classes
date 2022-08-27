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

MODULE AbstractFiniteElement_Class
USE GlobalData
USE AbstractRefElement_Class
IMPLICIT NONE
PRIVATE
  !!
CHARACTER(LEN=*), PARAMETER :: modName = "AbstractFiniteElement_Class"

!----------------------------------------------------------------------------
!                                                        AbstractRefElement_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Abstract class for finite element is defined
!
!{!pages/AbstractFiniteElement_.md!}

TYPE, ABSTRACT :: AbstractFiniteElement_
  PRIVATE
  INTEGER(I4B) :: order = 0
  !! Isotropic order of polynomial space
  INTEGER(I4B) :: refelem = 0
  !! name of reference element
  INTEGER(I4B) :: polySpace = 0
  !! name of polynomial space
  INTEGER(I4B) :: pointDOFType = 0
  !! type of dof for shape function defined on vertex
  INTEGER(I4B) :: edgeDOFType = 0
  !! type of dof for shape functions on edge
  INTEGER(I4B) :: faceDOFType = 0
  !! type of dof for shape functions on face
  INTEGER(I4B) :: cellDOFType = 0
  !! type of dof for shape functions in cell
  INTEGER(I4B) :: transformType = 0
  !! type of Tranformation usef for polynomial space
  INTEGER(I4B) :: latticeType = 0
  !! type of lattice (interpolation point type) point
  ! CLASS(AbstractRefElement_), POINTER :: refelem => NULL()
CONTAINS
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE(fe_Initiate), DEFERRED, PUBLIC, PASS(obj) :: Initiate
END TYPE AbstractFiniteElement_
!!
PUBLIC :: AbstractFiniteElement_
!!
!----------------------------------------------------------------------------
!                                              AbstractFiniteElementPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractFiniteElementPointer_
  CLASS(AbstractFiniteElement_), POINTER :: ptr => NULL()
END TYPE AbstractFiniteElementPointer_

PUBLIC :: AbstractFiniteElementPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary:         Initiate the finite element

ABSTRACT INTERFACE
  SUBROUTINE fe_Initiate(obj, elemType, feType, order, latticeType)
    IMPORT :: AbstractFiniteElement_, I4B
    CLASS(AbstractFiniteElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: feType
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: latticeType
  END SUBROUTINE fe_Initiate
END INTERFACE

END MODULE AbstractFiniteElement_Class
