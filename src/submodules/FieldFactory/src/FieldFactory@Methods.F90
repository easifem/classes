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

SUBMODULE(FieldFactory) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         MatrixFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE MatrixFieldFactory
CHARACTER(LEN=*), PARAMETER :: myName = "MatrixFieldFactory"
SELECT CASE (trim(engine))
CASE ("NATIVE_SERIAL")
  ALLOCATE (MatrixField_ :: ans)
CASE ("NATIVE_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_OMP engine is not available currently!! We are working on it.')
CASE ("NATIVE_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_MPI engine is not available currently!! We are working on it.')
CASE ("PETSC")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'PETSC engine is not available currently!! We are working on it.')
CASE ("LIS_SERIAL")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'LIS_SERIAL engine is not available currently!! We are working on it.')
CASE ("LIS_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!! We are working on it.')
CASE ("LIS_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!! We are working on it.')
END SELECT
END PROCEDURE MatrixFieldFactory

!----------------------------------------------------------------------------
!                                                   BlockMatrixFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE BlockMatrixFieldFactory
CHARACTER(LEN=*), PARAMETER :: myName = "BlockMatrixFieldFactory"
SELECT CASE (TRIM(engine))
CASE ("NATIVE_SERIAL")
  ALLOCATE (BlockMatrixField_ :: ans)
CASE ("NATIVE_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_OMP engine is not available currently!! We are working on it.')
CASE ("NATIVE_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_MPI engine is not available currently!! We are working on it.')
CASE ("PETSC")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'PETSC engine is not available currently!! We are working on it.')
CASE ("LIS_SERIAL")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'LIS_SERIAL engine is not available currently!! We are working on it.')
CASE ("LIS_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!! We are working on it.')
CASE ("LIS_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!! We are working on it.')
END SELECT
END PROCEDURE BlockMatrixFieldFactory

!----------------------------------------------------------------------------
!                                                         NodeFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE NodeFieldFactory
CHARACTER(LEN=*), PARAMETER :: myName = "NodeFieldFactory"
SELECT CASE (trim(engine))
CASE ("NATIVE_SERIAL")
  IF (TRIM(datatype) .EQ. "SCALAR") THEN
    ALLOCATE (ScalarField_ :: ans)
  ELSE IF (TRIM(datatype) .EQ. "ST_SCALAR") THEN
    ALLOCATE (STScalarField_ :: ans)
  ELSE IF (TRIM(datatype) .EQ. "VECTOR") THEN
    ALLOCATE (VectorField_ :: ans)
  ELSE IF (TRIM(datatype) .EQ. "ST_VECTOR") THEN
    ALLOCATE (STVectorField_ :: ans)
  ELSE IF (TRIM(datatype) .EQ. "BLOCK") THEN
    ALLOCATE (BlockNodeField_ :: ans)
  END IF
CASE ("NATIVE_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_OMP engine is not available currently!! We are working on it.')
CASE ("NATIVE_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_MPI engine is not available currently!! We are working on it.')
CASE ("PETSC")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'PETSC engine is not available currently!! We are working on it.')
CASE ("LIS_SERIAL")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'LIS_SERIAL engine is not available currently!! We are working on it.')
CASE ("LIS_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!! We are working on it.')
CASE ("LIS_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!! We are working on it.')
END SELECT
END PROCEDURE NodeFieldFactory

!----------------------------------------------------------------------------
!                                                     BlockNodeFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE BlockNodeFieldFactory
CHARACTER(LEN=*), PARAMETER :: myName = "BlockNodeFieldFactory"
SELECT CASE (trim(engine))
CASE ("NATIVE_SERIAL")
  ALLOCATE (BlockNodeField_ :: ans)
CASE ("NATIVE_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_OMP engine is not available currently!! We are working on it.')
CASE ("NATIVE_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_MPI engine is not available currently!! We are working on it.')
CASE ("PETSC")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'PETSC engine is not available currently!! We are working on it.')
CASE ("LIS_SERIAL")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'LIS_SERIAL engine is not available currently!! We are working on it.')
CASE ("LIS_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!! We are working on it.')
CASE ("LIS_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!! We are working on it.')
END SELECT
END PROCEDURE BlockNodeFieldFactory

!----------------------------------------------------------------------------
!                                                         ScalarFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldFactory
CHARACTER(LEN=*), PARAMETER :: myName = "ScalarFieldFactory"
SELECT CASE (trim(engine))
CASE ("NATIVE_SERIAL")
  ALLOCATE (ScalarField_ :: ans)
CASE ("NATIVE_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_OMP engine is not available currently!! We are working on it.')
CASE ("NATIVE_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_MPI engine is not available currently!! We are working on it.')
CASE ("PETSC")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'PETSC engine is not available currently!! We are working on it.')
CASE ("LIS_SERIAL")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'LIS_SERIAL engine is not available currently!! We are working on it.')
CASE ("LIS_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!! We are working on it.')
CASE ("LIS_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!! We are working on it.')
END SELECT
END PROCEDURE ScalarFieldFactory

!----------------------------------------------------------------------------
!                                                         VectorFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorFieldFactory
CHARACTER(LEN=*), PARAMETER :: myName = "VectorFieldFactory"
SELECT CASE (trim(engine))
CASE ("NATIVE_SERIAL")
  ALLOCATE (VectorField_ :: ans)
CASE ("NATIVE_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_OMP engine is not available currently!! We are working on it.')
CASE ("NATIVE_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_MPI engine is not available currently!! We are working on it.')
CASE ("PETSC")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'PETSC engine is not available currently!! We are working on it.')
CASE ("LIS_SERIAL")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'LIS_SERIAL engine is not available currently!! We are working on it.')
CASE ("LIS_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!! We are working on it.')
CASE ("LIS_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!! We are working on it.')
END SELECT
END PROCEDURE VectorFieldFactory

!----------------------------------------------------------------------------
!                                                      STVectorFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE STVectorFieldFactory
CHARACTER(LEN=*), PARAMETER :: myName = "STVectorFieldFactory"
SELECT CASE (trim(engine))
CASE ("NATIVE_SERIAL")
  ALLOCATE (STVectorField_ :: ans)
CASE ("NATIVE_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_OMP engine is not available currently!! We are working on it.')
CASE ("NATIVE_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_MPI engine is not available currently!! We are working on it.')
CASE ("PETSC")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'PETSC engine is not available currently!! We are working on it.')
CASE ("LIS_SERIAL")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'LIS_SERIAL engine is not available currently!! We are working on it.')
CASE ("LIS_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!! We are working on it.')
CASE ("LIS_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!! We are working on it.')
END SELECT
END PROCEDURE STVectorFieldFactory

!----------------------------------------------------------------------------
!                                                         STScalarFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE STScalarFieldFactory
CHARACTER(LEN=*), PARAMETER :: myName = "STScalarFieldFactory"
SELECT CASE (trim(engine))
CASE ("NATIVE_SERIAL")
  ALLOCATE (STScalarField_ :: ans)
CASE ("NATIVE_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_OMP engine is not available currently!! We are working on it.')
CASE ("NATIVE_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'NATIVE_MPI engine is not available currently!! We are working on it.')
CASE ("PETSC")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'PETSC engine is not available currently!! We are working on it.')
CASE ("LIS_SERIAL")
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'LIS_SERIAL engine is not available currently!! We are working on it.')
CASE ("LIS_OMP")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_OMP engine is not available currently!! We are working on it.')
CASE ("LIS_MPI")
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'LIS_MPI engine is not available currently!! We are working on it.')
END SELECT
END PROCEDURE STScalarFieldFactory

END SUBMODULE Methods
