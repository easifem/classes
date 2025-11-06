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

SUBMODULE(STVectorField_Class) IOMethods
USE String_Class, ONLY: String
USE Display_Method, ONLY: Display
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldDisplay, &
                                   AbstractNodeFieldImport, &
                                   AbstractNodeFieldExport

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CALL AbstractNodeFieldDisplay(obj=obj, msg=msg, unitno=unitno)
CALL Display(obj%spaceCompo, msg="spaceCompo = ", unitno=unitno)
CALL Display(obj%timeCompo, msg="timeCompo = ", unitno=unitno)
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
TYPE(String) :: dsetname
LOGICAL(LGT) :: bools(3), isok
TYPE(ParameterList_) :: param

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractNodeFieldImport(obj=obj, hdf5=hdf5, group=group, fedof=fedof, &
                             fedofs=fedofs)
! spaceCompo
dsetname = TRIM(group)//"/spaceCompo"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: The dataset spaceCompo should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%spaceCompo)

! timeCompo
dsetname = TRIM(group)//"/timeCompo"
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                '[INTERNAL ERROR] :: The dataset timeCompo should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%timeCompo)

dsetname = TRIM(group)//"/tSize"
bools(1) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/dof"
bools(2) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/realVec"
bools(3) = hdf5%pathExists(dsetname%chars())

isok = ALL(bools)
IF (isok) THEN
  CALL FinishMe
  RETURN
END IF

CALL param%initiate()

CALL SetSTVectorFieldParam( &
  param=param, name=obj%name%chars(), fieldType=obj%fieldType, &
  timeCompo=obj%timeCompo, spaceCompo=obj%spaceCompo, &
  engine=obj%engine%chars())

obj%isInit = .FALSE.

CALL obj%Initiate(param=param, fedof=fedof, geofedof=geofedof)

CALL param%DEALLOCATE()

CALL finishMe

CONTAINS
SUBROUTINE finishMe

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//"::"//myName//" - "// &
                          "[END]")
#endif

END SUBROUTINE finishMe

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractNodeFieldExport(obj=obj, hdf5=hdf5, group=group)

! timeCompo
dsetname = TRIM(group)//"/timeCompo"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%timeCompo)

! spaceCompo
dsetname = TRIM(group)//"/spaceCompo"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%spaceCompo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
