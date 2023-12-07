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
! date: 26 Aug 2021
! summary: This modules is a factory for linear solvers

SUBMODULE(FieldFactory) MatrixFieldFactoryMethods
USE BaseMethod, ONLY: UpperCase
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         MatrixFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractMatrixFieldFactory
CHARACTER(*), PARAMETER :: myName = "AbstractMatrixFieldFactory()"
CHARACTER(:), ALLOCATABLE :: case0

case0 = UpperCase(engine)//":"//UpperCase(name)

SELECT CASE (case0)
CASE ("NATIVE_SERIAL:MATRIX")
  ALLOCATE (MatrixField_ :: ans)

CASE ("NATIVE_SERIAL:BLOCKMATRIX")
  ALLOCATE (BlockMatrixField_ :: ans)

CASE ("LIS_OMP:MATRIX")
  ALLOCATE (MatrixFieldLis_ :: ans)

CASE ("LIS_OMP:BLOCKMATRIX")
  ALLOCATE (BlockMatrixFieldLis_ :: ans)

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[NO CASE FOUND] :: No case found for given engine '//  &
    & "following values are acceptable = "//  &
    & "[NATIVE_SERIAL:MATRIX, NATIVE_SERIAL:BLOCKMATRIX, "//  &
    & " LIS_OMP:MATRIX, LIS_OMP:BLOCKMATRIX]"// &
    & " but found  = "//TRIM(case0))

  ALLOCATE (MatrixField_ :: ans)
  RETURN
END SELECT
END PROCEDURE AbstractMatrixFieldFactory

!----------------------------------------------------------------------------
!                                                         MatrixFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixFieldFactory
CHARACTER(*), PARAMETER :: myName = "MatrixFieldFactory()"
CHARACTER(:), ALLOCATABLE :: case0

case0 = UpperCase(engine)

SELECT CASE (case0)
CASE ("NATIVE_SERIAL")
  ALLOCATE (MatrixField_ :: ans)

CASE ("LIS_OMP")
  ALLOCATE (MatrixFieldLis_ :: ans)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[NO CASE FOUND] :: No case found for given engine '//  &
    & "following values are acceptable = "//  &
    & "[NATIVE_SERIAL, LIS_OMP]"// &
    & " but found  = "//TRIM(case0))
  ALLOCATE (MatrixField_ :: ans)
  RETURN
END SELECT
END PROCEDURE MatrixFieldFactory

!----------------------------------------------------------------------------
!                                                   BlockMatrixFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE BlockMatrixFieldFactory
CHARACTER(*), PARAMETER :: myName = "BlockMatrixFieldFactory()"
CHARACTER(:), ALLOCATABLE :: case0

case0 = UpperCase(engine)

SELECT CASE (case0)
CASE ("NATIVE_SERIAL")
  ALLOCATE (BlockMatrixField_ :: ans)

CASE ("LIS_OMP")
  ALLOCATE (BlockMatrixFieldLis_ :: ans)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[NO CASE FOUND] :: No case found for given engine '//  &
    & "following values are acceptable = "//  &
    & "[NATIVE_SERIAL, LIS_OMP]"// &
    & " but found  = "//TRIM(case0))
  ALLOCATE (BlockMatrixField_ :: ans)
  RETURN
END SELECT
END PROCEDURE BlockMatrixFieldFactory

END SUBMODULE MatrixFieldFactoryMethods
