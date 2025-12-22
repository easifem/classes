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

MODULE FieldUtility
USE GlobalData, ONLY: DFP, LGT, I4B
USE VectorField_Class, ONLY: VectorField_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

CHARACTER(*), PARAMETER :: modName = "FieldUtility"

PRIVATE
PUBLIC :: VectorFieldSetNodeCoord

!----------------------------------------------------------------------------
!                                                               SetNodeCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-25
! summary: Set NodeCoord Field (VectorField) from AbstractMesh

INTERFACE
  MODULE SUBROUTINE VectorFieldSetNodeCoord(obj, mesh)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
  END SUBROUTINE VectorFieldSetNodeCoord
END INTERFACE

END MODULE FieldUtility
