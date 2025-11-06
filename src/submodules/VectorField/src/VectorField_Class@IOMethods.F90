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

SUBMODULE(VectorField_Class) IOMethods
USE String_Class, ONLY: String
USE Display_Method, ONLY: Display, ToString
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldDisplay, &
                                   AbstractNodeFieldImport, &
                                   AbstractNodeFieldExport
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CALL AbstractNodeFieldDisplay(obj=obj, msg=msg, unitno=unitno)
CALL Display(obj%spaceCompo, msg="spaceCompo = ", unitno=unitno)
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
TYPE(String) :: dsetname
TYPE(ParameterList_) :: param
LOGICAL(LGT) :: bools(3), isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractNodeFieldImport( &
  obj=obj, hdf5=hdf5, group=group, fedof=fedof, fedofs=fedofs, &
  geofedof=geofedof, geofedofs=geofedofs)

! spaceCompo
dsetname = TRIM(group)//"/spaceCompo"
isok = hdf5%pathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'The dataset spaceCompo should be present')

CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%spaceCompo)

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

CALL SetVectorFieldParam(param=param, name=obj%name%chars(), &
                         fieldType=obj%fieldType, spaceCompo=obj%spaceCompo, &
                         engine=obj%engine%chars())

obj%isInit = .FALSE.

CALL obj%Initiate(param=param, fedof=fedof, geofedof=geofedof)

CALL param%DEALLOCATE()

CALL finishMe

CONTAINS
SUBROUTINE finishMe

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE finishMe

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                  Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractNodeFieldExport(obj=obj, hdf5=hdf5, group=group)

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

MODULE PROCEDURE obj_ExportToVTK
CHARACTER(*), PARAMETER :: myName = "obj_ExportToVTK()"

INTEGER(I4B) :: tsize, tnodes, nrow, ncol
REAL(DFP), ALLOCATABLE :: VALUE(:, :)
TYPE(String) :: name
CHARACTER(1), ALLOCATABLE :: dofnames(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalPhysicalVars()
ALLOCATE (dofnames(tsize))
CALL obj%GetPhysicalNames(dofnames)

ncol = obj%spaceCompo
tsize = obj%fedof%GetTotalDOF()
tnodes = obj%fedof%GetTotalVertexDOF()

ALLOCATE (VALUE(tsize, 3))
CALL obj%Get(VALUE=VALUE, nrow=nrow, ncol=ncol, storageFMT=MYSTORAGEFORMAT)

VALUE(:, 3) = 0.0_DFP
! name = obj%name%chars()//"_"//dofnames(1)
name = obj%name%Join(array=dofnames, sep="_")

! CALL vtk%WriteDataArray(name=name, x=VALUE(1:nrow, 1:ncol), &
!                         numberOfComponents=1)
CALL vtk%WriteDataArray(name=name, x=VALUE(1:nrow, 1), &
                        y=VALUE(1:nrow, 2), z=VALUE(1:nrow, 3))

name = ''
DEALLOCATE (dofnames)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ExportToVTK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
