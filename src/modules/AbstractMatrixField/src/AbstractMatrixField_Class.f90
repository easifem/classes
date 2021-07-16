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

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: This module defines an abstract class for finite element matrix field.

MODULE AbstractMatrixField_Class
USE GlobalData
USE BaseType
USE AbstractField_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                     AbstractMatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This abstract class is defined to handle the finite element matrix
!
!### Introduction
! This abstract class is designed to handle the global tangent matrices
! defined for a computation domain. This abstract class will be extended to
!
! - [[MatrixField_]]
! - [[MPIMatrixField_]]
! - [[PetscMatrixField_]]
! - [[LISMatrixField_]]
! - [[OMPMatrixField_]]
! - [[ACCMatrixField_]]

TYPE, ABSTRACT, EXTENDS( AbstractField_ ) :: AbstractMatrixField_
  TYPE( CSRMatrix_ ) :: mat
END TYPE AbstractMatrixField_

PUBLIC :: AbstractMatrixField_

END MODULE AbstractMatrixField_Class