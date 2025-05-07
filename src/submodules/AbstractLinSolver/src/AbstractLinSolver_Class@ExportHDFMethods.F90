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
USE Display_Method, ONLY: ToString
USE BaseType, ONLY: TypePrecondOpt, TypeConvergenceOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myname = "obj_Export()"
TYPE(String) :: dsetname, strval
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AssertError1(obj%isInit, myname, &
                  'The object is not initiated, initiate it first!')

isok = hdf5%isOpen()
CALL AssertError1(isok, myname, &
                  'The object is not initiated, initiate it first!')

isok = hdf5%isWrite()
CALL AssertError1(isok, myname, &
                  'HDF5 file does not have write permission')

dsetname = TRIM(group)//"/engine"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%engine)

dsetname = TRIM(group)//"/solverName"
strval = obj%GetLinSolverNameFromCode(obj%solverName)
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/preconditionOption"
SELECT CASE (obj%preconditionOption)
CASE (TypePrecondOpt%NONE)
  strval = "NONE"
CASE (TypePrecondOpt%left)
  strval = "LEFT"
CASE (TypePrecondOpt%right)
  strval = "RIGHT"
CASE (TypePrecondOpt%both)
  strval = "LEFT_RIGHT"
CASE default
  CALL no_case_found("preconditionOption")
END SELECT
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/convergenceIn"
SELECT CASE (obj%convergenceIn)
CASE (TypeConvergenceOpt%res)
  strval = "RESIDUAL"
CASE (TypeConvergenceOpt%sol)
  strval = "SOLUTION"
CASE default
  CALL no_case_found("convergenceIn")
END SELECT
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

dsetname = TRIM(group)//"/convergenceType"
SELECT CASE (obj%convergenceType)
CASE (TypeConvergenceOpt%absolute)
  strval = "ABSOLUTE"
CASE (TypeConvergenceOpt%relative)
  strval = "RELATIVE"
CASE default
  CALL no_case_found("convergenceType")
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
                        "[END] Export()")

CONTAINS
SUBROUTINE no_case_found(name)
  CHARACTER(*), INTENT(in) :: name
  CALL AssertError1(.FALSE., myname, &
                    'No case found for '//name)
END SUBROUTINE no_case_found
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ExportHDFMethods
