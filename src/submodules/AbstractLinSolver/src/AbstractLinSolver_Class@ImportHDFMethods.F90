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
USE HDF5File_Method, ONLY: HDF5ReadScalar
USE Display_Method, ONLY: ToString
USE BaseType, ONLY: TypePrecondOpt, TypeConvergenceOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
TYPE(String) :: strval
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = .NOT. obj%isInit
CALL AssertError1(isok, myName, &
                  'The object is already initiated, deallocate first!')

obj%isInit = .TRUE.

isok = hdf5%isOpen()
CALL AssertError1(isok, myName, "HDF5 file is not opened")

isok = hdf5%isRead()
CALL AssertError1(isok, myName, "HDF5 file does not have read permission")

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%engine, group=group, &
             fieldname="engine", myname=myName, modName=modName, check=.TRUE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=strval, group=group, &
         fieldname="solverName", myname=myName, modName=modName, check=.TRUE.)
obj%solverName = obj%getLinSolverCodeFromName(strval%chars())

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=strval, group=group, &
 fieldname="preconditionOption", myname=myName, modName=modName, check=.TRUE.)
strval = strval%Upper()
SELECT CASE (strval%chars())
CASE ("NONE")
  obj%preconditionOption = TypePrecondOpt%NONE
CASE ("LEFT")
  obj%preconditionOption = TypePrecondOpt%left
CASE ("RIGHT")
  obj%preconditionOption = TypePrecondOpt%right
CASE ("LEFT_RIGHT", "BOTH")
  obj%preconditionOption = TypePrecondOpt%both
CASE DEFAULT
  CALL no_case_found("preconditionOption")
END SELECT

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=strval, group=group, &
      fieldname="convergenceIn", myname=myName, modName=modName, check=.TRUE.)
strval = strval%Upper()
SELECT CASE (strval%chars())
CASE ("RESIDUAL")
  obj%convergenceIn = TypeConvergenceOpt%res
CASE ("SOLUTION")
  obj%convergenceIn = TypeConvergenceOpt%sol
CASE DEFAULT
  CALL no_case_found("convergenceIn")
END SELECT

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=strval, group=group, &
    fieldname="convergenceType", myname=myName, modName=modName, check=.TRUE.)
strval = strval%Upper()
SELECT CASE (strval%chars())
CASE ("ABSOLUTE")
  obj%convergenceType = TypeConvergenceOpt%absolute
CASE ("RELATIVE")
  obj%convergenceType = TypeConvergenceOpt%relative
CASE default
  CALL no_case_found("convergenceType")
END SELECT

obj%relativeToRHS = .FALSE.
CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%relativeToRHS, group=group, &
     fieldname="relativeToRHS", myname=myName, modName=modName, check=.FALSE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%maxIter, group=group, &
           fieldname="maxIter", myname=myName, modName=modName, check=.FALSE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%KrylovSubspaceSize, group=group, &
fieldname="KrylovSubspaceSize", myname=myName, modName=modName, check=.FALSE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%rtol, group=group, &
 fieldname="relativeTolerance", myname=myName, modName=modName, check=.FALSE.)

CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%atol, group=group, &
 fieldname="absoluteTolerance", myname=myName, modName=modName, check=.FALSE.)

obj%isInit = .FALSE.

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

! CALL param%Initiate()
!
! CALL SetAbstractLinSolverParam( &
!   param=param, &
!   prefix=obj%GetPrefix(), &
!   engine=obj%engine%chars(), &
!   solverName=obj%solverName, &
!   preconditionOption=obj%preconditionOption, &
!   convergenceIn=obj%convergenceIn, &
!   convergenceType=obj%convergenceType, &
!   maxIter=obj%maxIter, &
!   relativeToRHS=obj%relativeToRHS, &
!   KrylovSubspaceSize=obj%KrylovSubspaceSize, &
!   rtol=obj%rtol, &
!   atol=obj%atol)
!
! CALL obj%Initiate(param)
! CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

CONTAINS
SUBROUTINE no_case_found(msg)
  CHARACTER(*), INTENT(IN) :: msg
  CALL AssertError1(.FALSE., myName, "No case found for "//msg)
END SUBROUTINE no_case_found

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ImportHDFMethods
