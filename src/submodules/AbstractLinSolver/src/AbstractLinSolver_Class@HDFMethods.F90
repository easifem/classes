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

SUBMODULE(AbstractLinSolver_Class) HDFMethods
USE HDF5File_Method, ONLY: HDF5ReadScalar
USE Display_Method, ONLY: ToString
USE BaseType, ONLY: TypePrecondOpt, TypeConvergenceOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

! CHARACTER(*), PARAMETER :: myName = "obj_Import()"
! TYPE(String) :: strval
! LOGICAL(LGT) :: isok
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! isok = .NOT. obj%isInit
! CALL AssertError1(isok, myName, &
!                   'The object is already initiated, deallocate first!')
!
! obj%isInit = .TRUE.
!
! isok = hdf5%isOpen()
! CALL AssertError1(isok, myName, "HDF5 file is not opened")
!
! isok = hdf5%isRead()
! CALL AssertError1(isok, myName, "HDF5 file does not have read permission")
!
! CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%engine, group=group, &
!              fieldname="engine", myname=myName, modName=modName, check=.TRUE.)
!
! CALL HDF5ReadScalar(hdf5=hdf5, VALUE=strval, group=group, &
!          fieldname="solverName", myname=myName, modName=modName, check=.TRUE.)
! obj%solverName = obj%getLinSolverCodeFromName(strval%chars())
!
! CALL HDF5ReadScalar(hdf5=hdf5, VALUE=strval, group=group, &
!  fieldname="preconditionOption", myname=myName, modName=modName, check=.TRUE.)
! strval = strval%Upper()
! SELECT CASE (strval%chars())
! CASE ("NONE")
!   obj%preconditionOption = TypePrecondOpt%NONE
! CASE ("LEFT")
!   obj%preconditionOption = TypePrecondOpt%left
! CASE ("RIGHT")
!   obj%preconditionOption = TypePrecondOpt%right
! CASE ("LEFT_RIGHT", "BOTH")
!   obj%preconditionOption = TypePrecondOpt%both
! CASE DEFAULT
!   CALL no_case_found("preconditionOption")
! END SELECT
!
! CALL HDF5ReadScalar(hdf5=hdf5, VALUE=strval, group=group, &
!       fieldname="convergenceIn", myname=myName, modName=modName, check=.TRUE.)
! strval = strval%Upper()
! SELECT CASE (strval%chars())
! CASE ("RESIDUAL")
!   obj%convergenceIn = TypeConvergenceOpt%res
! CASE ("SOLUTION")
!   obj%convergenceIn = TypeConvergenceOpt%sol
! CASE DEFAULT
!   CALL no_case_found("convergenceIn")
! END SELECT
!
! CALL HDF5ReadScalar(hdf5=hdf5, VALUE=strval, group=group, &
!     fieldname="convergenceType", myname=myName, modName=modName, check=.TRUE.)
! strval = strval%Upper()
! SELECT CASE (strval%chars())
! CASE ("ABSOLUTE")
!   obj%convergenceType = TypeConvergenceOpt%absolute
! CASE ("RELATIVE")
!   obj%convergenceType = TypeConvergenceOpt%relative
! CASE default
!   CALL no_case_found("convergenceType")
! END SELECT
!
! obj%relativeToRHS = .FALSE.
! CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%relativeToRHS, group=group, &
!      fieldname="relativeToRHS", myname=myName, modName=modName, check=.FALSE.)
!
! CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%maxIter, group=group, &
!            fieldname="maxIter", myname=myName, modName=modName, check=.FALSE.)
!
! CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%KrylovSubspaceSize, group=group, &
! fieldname="KrylovSubspaceSize", myname=myName, modName=modName, check=.FALSE.)
!
! CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%rtol, group=group, &
!  fieldname="relativeTolerance", myname=myName, modName=modName, check=.FALSE.)
!
! CALL HDF5ReadScalar(hdf5=hdf5, VALUE=obj%atol, group=group, &
!  fieldname="absoluteTolerance", myname=myName, modName=modName, check=.FALSE.)
!
! obj%isInit = .FALSE.
!
! #ifdef DEBUG_VER
! CALL e%RaiseError(modName//'::'//myName//' - '// &
!                   '[WIP ERROR] :: This routine is under development')
! #endif
!
! ! CALL param%Initiate()
! !
! ! CALL SetAbstractLinSolverParam( &
! !   param=param, &
! !   prefix=obj%GetPrefix(), &
! !   engine=obj%engine%chars(), &
! !   solverName=obj%solverName, &
! !   preconditionOption=obj%preconditionOption, &
! !   convergenceIn=obj%convergenceIn, &
! !   convergenceType=obj%convergenceType, &
! !   maxIter=obj%maxIter, &
! !   relativeToRHS=obj%relativeToRHS, &
! !   KrylovSubspaceSize=obj%KrylovSubspaceSize, &
! !   rtol=obj%rtol, &
! !   atol=obj%atol)
! !
! ! CALL obj%Initiate(param)
! ! CALL param%DEALLOCATE()
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[END] ')
! #endif
!
! CONTAINS
! SUBROUTINE no_case_found(msg)
!   CHARACTER(*), INTENT(IN) :: msg
!   CALL AssertError1(.FALSE., myName, "No case found for "//msg)
! END SUBROUTINE no_case_found

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

! CHARACTER(*), PARAMETER :: myname = "obj_Export()"
! TYPE(String) :: dsetname, strval
! LOGICAL(LGT) :: isok
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! CALL AssertError1(obj%isInit, myname, &
!                   'The object is not initiated, initiate it first!')
!
! isok = hdf5%isOpen()
! CALL AssertError1(isok, myname, &
!                   'The object is not initiated, initiate it first!')
!
! isok = hdf5%isWrite()
! CALL AssertError1(isok, myname, &
!                   'HDF5 file does not have write permission')
!
! dsetname = TRIM(group)//"/engine"
! CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%engine)
!
! dsetname = TRIM(group)//"/solverName"
! strval = obj%GetLinSolverNameFromCode(obj%solverName)
! CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)
!
! dsetname = TRIM(group)//"/preconditionOption"
! SELECT CASE (obj%preconditionOption)
! CASE (TypePrecondOpt%NONE)
!   strval = "NONE"
! CASE (TypePrecondOpt%left)
!   strval = "LEFT"
! CASE (TypePrecondOpt%right)
!   strval = "RIGHT"
! CASE (TypePrecondOpt%both)
!   strval = "LEFT_RIGHT"
! CASE default
!   CALL no_case_found("preconditionOption")
! END SELECT
! CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)
!
! dsetname = TRIM(group)//"/convergenceIn"
! SELECT CASE (obj%convergenceIn)
! CASE (TypeConvergenceOpt%res)
!   strval = "RESIDUAL"
! CASE (TypeConvergenceOpt%sol)
!   strval = "SOLUTION"
! CASE default
!   CALL no_case_found("convergenceIn")
! END SELECT
! CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)
!
! dsetname = TRIM(group)//"/convergenceType"
! SELECT CASE (obj%convergenceType)
! CASE (TypeConvergenceOpt%absolute)
!   strval = "ABSOLUTE"
! CASE (TypeConvergenceOpt%relative)
!   strval = "RELATIVE"
! CASE default
!   CALL no_case_found("convergenceType")
! END SELECT
! CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)
!
! dsetname = TRIM(group)//"/relativeToRHS"
! CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%relativeToRHS)
!
! dsetname = TRIM(group)//"/maxIter"
! CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%maxIter)
!
! dsetname = TRIM(group)//"/KrylovSubspaceSize"
! CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%KrylovSubspaceSize)
!
! dsetname = TRIM(group)//"/relativeTolerance"
! CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%rtol)
!
! dsetname = TRIM(group)//"/absoluteTolerance"
! CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%atol)
!
! CALL e%raiseInformation(modName//"::"//myName//" - "// &
!                         "[END] Export()")
!
! CONTAINS
! SUBROUTINE no_case_found(name)
!   CHARACTER(*), INTENT(in) :: name
!   CALL AssertError1(.FALSE., myname, &
!                     'No case found for '//name)
! END SUBROUTINE no_case_found
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
