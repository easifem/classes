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

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This submodule contains input-output methods

SUBMODULE(NewtonianFluidModel_Class) HDFMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Import
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

! TYPE(String) :: dsetname, strval
! REAL(DFP) :: DynamicViscosity
! TYPE(ParameterList_) :: param
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! IF (obj%isInitiated()) THEN
!   CALL e%raiseError(modName//'::'//myName//" - "// &
!   & '[CONFIG ERROR] :: The object is already initiated, deallocate first!')
! END IF
!
! IF (.NOT. hdf5%isOpen()) THEN
!   CALL e%raiseError(modName//'::'//myName//" - "// &
!   & '[CONFIG ERROR] :: HDF5 file is not opened')
! END IF
!
! IF (.NOT. hdf5%isRead()) THEN
!   CALL e%raiseError(modName//'::'//myName//" - "// &
!   & '[CONFIG ERROR] :: HDF5 file does not have read permission')
! END IF
!
! !> READ name
! dsetname = TRIM(group)//"/name"
! IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
!   CALL e%raiseError(modName//'::'//myName//" - "// &
!   & '[CONFIG] The dataset name should be present')
! END IF
! CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
! CALL obj%SetName(strval%chars())
! strval = ""
! !> READ DynamicViscosity
!
! dsetname = TRIM(group)//"/dynamicViscosity"
! IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
!   CALL e%raiseError(modName//'::'//myName//" - "// &
!     & 'The dataset dynamicViscosity should be present')
! ELSE
!   CALL hdf5%READ(dsetname=dsetname%chars(), vals=DynamicViscosity)
! END IF
!
! CALL param%initiate()
! CALL SetNewtonianFluidModelParam(param=param, &
!                                  dynamicViscosity=dynamicViscosity)
! CALL obj%initiate(param)
! CALL param%DEALLOCATE()
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[END] Import()')
! #endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export"
#endif

LOGICAL(LGT) :: isok
TYPE(String) :: dsetname, strval

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'The object is not initiated, initiate first!')
#endif

#ifdef DEBUG_VER
isok = hdf5%IsOpen()
CALL AssertError1(isok, myName, 'HDF5 file is not opened')

isok = hdf5%IsWrite()
CALL AssertError1(isok, myName, 'HDF5 file does not have write permission')
#endif

dsetname = group//"/name"
strval = obj%GetName()
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)
strval = ""

dsetname = group//"/dynamicViscosity"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%Mu)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                               Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
