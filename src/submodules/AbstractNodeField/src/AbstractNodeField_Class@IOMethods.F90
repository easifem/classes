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

SUBMODULE(AbstractNodeField_Class) IOMethods
USE BaseMethod
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Display
CALL AbstractFieldDisplay(obj=obj, msg=msg, unitNo=unitNo)
CALL Display(obj%tSize, "# tSize : ", unitNo=unitNo)
CALL Display(obj%realVec, obj%dof, "# realVec : ", unitNo=unitNo)
END PROCEDURE anf_Display

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Import
CHARACTER(*), PARAMETER :: myName = "anf_Import"
TYPE(String) :: dsetname
LOGICAL(LGT) :: abool
TYPE(ParameterList_) :: param

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START] Import()')

CALL AbstractFieldImport( &
  & obj=obj, &
  & hdf5=hdf5, &
  & group=group, &
  & dom=dom, &
  & domains=domains)

dsetname = TRIM(group)//"/tSize"
abool = hdf5%pathExists(dsetname%chars())
IF (abool) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%tSize)
END IF

dsetname = TRIM(group)//"/dof"
abool = hdf5%pathExists(dsetname%chars())
IF (abool) THEN
  CALL ImportDOF(obj=obj%dof, hdf5=hdf5, group=dsetname%chars())
END IF

dsetname = TRIM(group)//"/realVec"
abool = hdf5%pathExists(dsetname%chars())
IF (abool) THEN
  CALL ImportRealVector(obj=obj%realvec, hdf5=hdf5, &
  & group=dsetname%chars())
END IF

! info
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[END] Import()")

END PROCEDURE anf_Import

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Export
CHARACTER(*), PARAMETER :: myName = "anf_Export"
TYPE(String) :: strval, dsetname

CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[START] Export()")

CALL AbstractFieldExport(obj=obj, hdf5=hdf5, group=group)

! tSize
dsetname = TRIM(group)//"/tSize"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%tSize)

! dof
dsetname = TRIM(group)//"/dof"
CALL ExportDOF(obj=obj%dof, hdf5=hdf5, group=dsetname%chars())

! realVec
dsetname = TRIM(group)//"/realVec"
CALL ExportRealVector(obj=obj%realVec, hdf5=hdf5, &
  & group=dsetname%chars())

! info
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[END] Export()")

END PROCEDURE anf_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
