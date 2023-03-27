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

SUBMODULE(STScalarField_Class) IOMethods
USE BaseMethod
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Display
CALL AbstractNodeFieldDisplay(obj=obj, msg=msg, unitno=unitno)
CALL Display(obj%timeCompo, msg="# timeCompo = ", unitno=unitno)
END PROCEDURE stsField_Display

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Import
CHARACTER(*), PARAMETER :: myName = "stsField_Import"
TYPE(String) :: dsetname
LOGICAL(LGT) :: bools(3)
TYPE(ParameterList_) :: param

! info
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[START] Import()")

CALL AbstractNodeFieldImport( &
  & obj=obj, &
  & hdf5=hdf5, &
  & group=group, &
  & dom=dom, &
  & domains=domains)

! timeCompo
dsetname = TRIM(group)//"/timeCompo"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%timeCompo)
ELSE
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset timeCompo should be present')
END IF

dsetname = TRIM(group)//"/tSize"
bools(1) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/dof"
bools(2) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/realVec"
bools(3) = hdf5%pathExists(dsetname%chars())

IF (.NOT. ALL(bools)) THEN
  CALL param%initiate()

  CALL setSTScalarFieldParam( &
    & param=param, &
    & name=obj%name%chars(), &
    & fieldType=obj%fieldType, &
    & timeCompo=obj%timeCompo, &
    & engine=obj%engine%chars() &
    & )

  obj%isInitiated = .FALSE.

  CALL obj%initiate(param=param, dom=dom)

  CALL param%DEALLOCATE()
END IF

CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[END] Import()")

END PROCEDURE stsField_Import

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Export
CHARACTER(*), PARAMETER :: myName = "stsField_Export"
TYPE(String) :: dsetname

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START] Export()')

CALL AbstractNodeFieldExport(obj=obj, hdf5=hdf5, group=group)

! timeCompo
dsetname = TRIM(group)//"/timeCompo"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%timeCompo)

CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[END] Export()")
END PROCEDURE stsField_Export

!----------------------------------------------------------------------------
!                                                                 
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
