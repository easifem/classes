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

SUBMODULE(AbstractLinSolver_Class) ImportHDFMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE als_Import
CHARACTER(*), PARAMETER :: myName = "als_Import"
TYPE(String) :: dsetname, strval

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The object is already initiated, deallocate first!')
END IF
obj%isInitiated = .TRUE.

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
END IF
IF (.NOT. hdf5%isRead()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have read permission')
END IF

dsetname = TRIM(group)//"/engine"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset engine should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%engine)

dsetname = TRIM(group)//"/solverName"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset solverName should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
obj%solverName = obj%getLinSolverCodeFromName(TRIM(strval%chars()))

dsetname = TRIM(group)//"/preconditionOption"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset preconditionOption should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
SELECT CASE (TRIM(strval%chars()))
CASE ("NONE")
  obj%preconditionOption = NO_PRECONDITION
CASE ("LEFT")
  obj%preconditionOption = LEFT_PRECONDITION
CASE ("RIGHT")
  obj%preconditionOption = RIGHT_PRECONDITION
CASE ("LEFT_RIGHT")
  obj%preconditionOption = LEFT_RIGHT_PRECONDITION
END SELECT

dsetname = TRIM(group)//"/convergenceIn"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset convergenceIn should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
SELECT CASE (TRIM(strval%chars()))
CASE ("RESIDUAL")
  obj%convergenceIn = convergenceInRes
CASE ("SOLUTION")
  obj%convergenceIn = convergenceInSol
END SELECT

dsetname = TRIM(group)//"/convergenceType"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset convergenceType should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
SELECT CASE (TRIM(strval%chars()))
CASE ("ABSOLUTE")
  obj%convergenceType = absoluteConvergence
CASE ("RELATIVE")
  obj%convergenceType = relativeConvergence
END SELECT

IF (obj%convergenceType .EQ. relativeConvergence) THEN
  dsetname = TRIM(group)//"/relativeToRHS"
  IF (hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%relativeToRHS)
  ELSE
    obj%relativeToRHS = .FALSE.
  END IF
ELSE
  obj%relativeToRHS = .FALSE.
END IF

dsetname = TRIM(group)//"/maxIter"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset maxIter should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%maxIter)

dsetname = TRIM(group)//"/KrylovSubspaceSize"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%KrylovSubspaceSize)
ELSE
  obj%KrylovSubspaceSize = 20
END IF

dsetname = TRIM(group)//"/relativeTolerance"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset relativeTolerance should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%rtol)

dsetname = TRIM(group)//"/absoluteTolerance"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset absoluteTolerance should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%atol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE als_Import

END SUBMODULE ImportHDFMethods
