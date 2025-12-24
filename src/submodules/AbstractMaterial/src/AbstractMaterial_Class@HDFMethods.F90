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

SUBMODULE(AbstractMaterial_Class) HDFMethods
USE Display_Method, ONLY: ToString
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
LOGICAL(LGT) :: isok
#endif

TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

! check
#ifdef DEBUG_VER
isok = obj%isInit
CALL AssertError1(isok, myName, &
     'Instance of AbstractMaterial_ or its child-class is not initiated, &
     &initiate it first.')
#endif

! check
#ifdef DEBUG_VER
isok = hdf5%IsOpen()
CALL AssertError1(isok, myName, 'HDF5 file is not opened')
#endif

#ifdef DEBUG_VER
isok = hdf5%IsWrite()
CALL AssertError1(isok, myName, 'HDF5 file is does not have write permission')
#endif

! name
dsetname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%name)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
LOGICAL(LGT) :: isok
#endif

TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
isok = .NOT. obj%isInit
CALL AssertError1(isok, myName, &
  'Instance of AbstractMaterial_ or its child is already initiated, &
  &Deallocate it first!')
#endif

#ifdef DEBUG_VER
isok = hdf5%IsOpen()
CALL AssertError1(isok, myName, 'HDF5 file is not opened')
#endif

#ifdef DEBUG_VER
isok = hdf5%IsRead()
CALL AssertError1(isok, myName, 'HDF5 file does not have read permission')
#endif

obj%isInit = .TRUE.

dsetname = TRIM(group)//"/name"
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)

dsetname = ''

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                           Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
