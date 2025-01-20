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

SUBMODULE(AbstractLinSolver_Class) ExportHDFMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE als_Export
CHARACTER(*), PARAMETER :: myName = "als_Export"
TYPE(String) :: dsetname, strval

CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[START] Export()")

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The object is not initiated, initiate it first!')
END IF

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isWrite()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have write permission')
END IF

dsetname = TRIM(group)//"/engine"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%engine)

dsetname = TRIM(group)//"/solverName"
strval = obj%getLinSolverNameFromCode(obj%solverName)
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/preconditionOption"
SELECT CASE (obj%preconditionOption)
CASE (NO_PRECONDITION)
  strval = "NONE"
CASE (LEFT_PRECONDITION)
  strval = "LEFT"
CASE (RIGHT_PRECONDITION)
  strval = "RIGHT"
CASE (LEFT_RIGHT_PRECONDITION)
  strval = "LEFT_RIGHT"
END SELECT
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/convergenceIn"
SELECT CASE (obj%convergenceIn)
CASE (convergenceInRes)
  strval = "RESIDUAL"
CASE (convergenceInSol)
  strval = "SOLUTION"
END SELECT
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/convergenceType"
SELECT CASE (obj%convergenceType)
CASE (absoluteConvergence)
  strval = "ABSOLUTE"
CASE (relativeConvergence)
  strval = "RELATIVE"
END SELECT
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/relativeToRHS"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%relativeToRHS)

dsetname = TRIM(group)//"/maxIter"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%maxIter)

dsetname = TRIM(group)//"/KrylovSubspaceSize"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%KrylovSubspaceSize)

dsetname = TRIM(group)//"/relativeTolerance"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%rtol)

dsetname = TRIM(group)//"/absoluteTolerance"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%atol)

CALL e%raiseInformation(modName//"::"//myName//" - "// &
& "[END] Export()")
END PROCEDURE als_Export

END SUBMODULE ExportHDFMethods
