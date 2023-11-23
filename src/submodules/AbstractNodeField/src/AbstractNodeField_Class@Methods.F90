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

SUBMODULE(AbstractNodeField_Class) Methods
USE BaseMethod
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_SetParam
INTEGER(I4B) :: ii

IF (PRESENT(dof_tPhysicalVars)) THEN
  obj%dof_tPhysicalVars = dof_tPhysicalVars
END IF

IF (PRESENT(dof_storageFMT)) THEN
  obj%dof_storageFMT = dof_storageFMT
END IF

IF (PRESENT(dof_spaceCompo)) THEN
  obj%dof_spaceCompo = dof_spaceCompo
END IF

IF (PRESENT(dof_timeCompo)) THEN
  obj%dof_timeCompo = dof_timeCompo
END IF

IF (PRESENT(dof_tNodes)) THEN
  obj%dof_tNodes = dof_tNodes
END IF

IF (PRESENT(dof_names_char)) THEN
  IF (ALLOCATED(obj%dof_names_char)) DEALLOCATE (obj%dof_names_char)
  ALLOCATE (obj%dof_names_char(SIZE(dof_names_char)))
  DO ii = 1, SIZE(dof_names_char)
    obj%dof_names_char(ii) = obj%dof_names_char(ii)
  END DO
END IF

END PROCEDURE anf_SetParam

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Display
CALL AbstractFieldDisplay(obj=obj, msg=msg, unitNo=unitNo)
CALL Display(obj%tSize, "# tSize : ", unitNo=unitNo)
CALL Display(obj%realVec, obj%dof, "# realVec : ", unitNo=unitNo)
END PROCEDURE anf_Display

!----------------------------------------------------------------------------
!                                                                GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetPointer
ans => GetPointer(obj%realVec)
END PROCEDURE anf_GetPointer

!----------------------------------------------------------------------------
!                                                                     Size
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_size
CHARACTER(*), PARAMETER :: myName = "anf_size"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
  & 'child classes')
! ans = obj%tSize
END PROCEDURE anf_size

!----------------------------------------------------------------------------
!                                                                 Initiate2
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_initiate2
CHARACTER(*), PARAMETER :: myName = "anf_initiate2"
INTEGER(I4B) :: ii, tsize

CALL obj%DEALLOCATE()
CALL AbstractFieldInitiate2( &
  & obj=obj, &
  & obj2=obj2, &
  & copyFull=copyFull, &
  & copyStructure=copyStructure, &
  & usePointer=usePointer)

SELECT TYPE (obj2); CLASS IS (AbstractNodeField_)
  obj%tSize = obj2%tSize
  obj%realVec = obj2%realVec
  obj%dof = obj2%dof
END SELECT

END PROCEDURE anf_initiate2

!----------------------------------------------------------------------------
!                                                            anf_Initiate3
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_initiate3
CHARACTER(*), PARAMETER :: myName = "anf_Initiate3"
CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[IMPLEMENTATION ERROR] :: Initiate3 should be implemented by the'// &
  & ' child of AbstractNodeField_')
END PROCEDURE anf_initiate3

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Deallocate
CALL AbstractFieldDeallocate(obj)
obj%tSize = 0
CALL DEALLOCATE (obj%realVec)
CALL DEALLOCATE (obj%dof)
obj%dof_tPhysicalVars = 0
obj%dof_storageFMT = NODES_FMT
IF (ALLOCATED(obj%dof_spaceCompo)) DEALLOCATE (obj%dof_spaceCompo)
IF (ALLOCATED(obj%dof_timeCompo)) DEALLOCATE (obj%dof_timeCompo)
IF (ALLOCATED(obj%dof_tNodes)) DEALLOCATE (obj%dof_tNodes)
IF (ALLOCATED(obj%dof_names_char)) DEALLOCATE (obj%dof_names_char)
END PROCEDURE anf_Deallocate

!----------------------------------------------------------------------------
!                                                                     Norm2
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Norm2
CHARACTER(*), PARAMETER :: myName = "anf_Norm2"
IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  ans = NORM2(obj=obj%realvec)
ELSE
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This method has been implemented for NATIVE engines')
  ans = 0.0_DFP
END IF
END PROCEDURE anf_Norm2

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
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_SetSingle
IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  IF (PRESENT(addContribution)) THEN
    CALL add(obj%realVec, nodenum=1, VALUE=VALUE, scale=scale)
  ELSE
    CALL set(obj%realVec, nodenum=1, VALUE=VALUE)
  END IF
ELSE
  IF (PRESENT(addContribution)) THEN
    CALL add(obj%realVec, nodenum=indx, VALUE=VALUE, scale=scale)
  ELSE
    CALL set(obj%realVec, nodenum=indx, VALUE=VALUE)
  END IF
END IF
END PROCEDURE anf_SetSingle

!----------------------------------------------------------------------------
!                                                                 GetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetSingle
IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  VALUE = Get( &
    & obj=obj%realVec, &
    & nodenum=1, &
    & dataType=1.0_DFP)
ELSE
  VALUE = Get( &
    & obj=obj%realVec, &
    & nodenum=indx, &
    & dataType=1.0_DFP)
END IF
END PROCEDURE anf_GetSingle

!----------------------------------------------------------------------------
!                                                             Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractNodeFieldInitiate
CHARACTER(*), PARAMETER :: myName = "AbstractNodeFieldInitiate"

CALL AbstractFieldInitiate(obj=obj, param=param, prefix=prefix, dom=dom)

IF (obj%dof_tPhysicalVars .EQ. 0_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INITIATE ERROR] :: AbstractNodeField_::obj%dof_tPhysicalVars is 0')
END IF

IF (.NOT. ALLOCATED(obj%dof_spaceCompo)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INITIATE ERROR] :: AbstractNodeField_::obj%dof_spaceCompo '// &
    & ' is NOT ALLOCATED')
END IF

IF (.NOT. ALLOCATED(obj%dof_timeCompo)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INITIATE ERROR] :: AbstractNodeField_::obj%dof_timeCompo '// &
    & ' is NOT ALLOCATED')
END IF

IF (.NOT. ALLOCATED(obj%dof_tNodes)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INITIATE ERROR] :: AbstractNodeField_::obj%dof_tNodes '// &
    & ' is NOT ALLOCATED')
END IF

IF (.NOT. ALLOCATED(obj%dof_names_char)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INITIATE ERROR] :: AbstractNodeField_::obj%dof_names_char '// &
    & ' is NOT ALLOCATED')
END IF

CALL Initiate( &
  & obj=obj%dof, &
  & tNodes=obj%dof_tNodes, &
  & names=obj%dof_names_char, &
  & spaceCompo=obj%dof_spaceCompo, &
  & timeCompo=obj%dof_timeCompo, &
  & storageFMT=obj%dof_storageFMT)

CALL Initiate(obj=obj%realVec, dofobj=obj%dof)

obj%tSize = SIZE(obj%realVec)

IF (obj%local_n .EQ. 0) THEN
  obj%local_n = obj%tSize
END IF

IF (obj%global_n .EQ. 0) THEN
  obj%global_n = obj%tSize
END IF

END PROCEDURE AbstractNodeFieldInitiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
