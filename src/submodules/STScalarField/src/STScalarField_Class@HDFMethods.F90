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

SUBMODULE(STScalarField_Class) HDFMethods
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldExport
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldImport
USE String_Class, ONLY: String
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "STScalarField_Class@HDFMethods.F90"
#endif

CONTAINS

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

CALL AbstractNodeFieldImport( &
  obj=obj, hdf5=hdf5, group=group, fedof=fedof, fedofs=fedofs, &
  timefedof=timefedof, timefedofs=timefedofs, geofedof=geofedof, &
  geofedofs=geofedofs)

! timeCompo
dsetname = TRIM(group)//"/IntR0/timeCompo"

#ifdef DEBUG_VER
isok = hdf5%pathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  dsetname//' should be present')
#endif

CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%timeCompo)

! idofs
dsetname = TRIM(group)//"/IntR1/idofs"

#ifdef DEBUG_VER
isok = hdf5%pathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  dsetname//' should be present')
#endif

CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%idofs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
#endif

TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractNodeFieldExport(obj=obj, hdf5=hdf5, group=group)

! timeCompo
dsetname = TRIM(group)//"/IntR0/timeCompo"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%timeCompo)

! idofs
dsetname = TRIM(group)//"/IntR1/idofs"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%idofs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
