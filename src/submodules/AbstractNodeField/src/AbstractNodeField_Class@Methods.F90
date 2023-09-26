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
USE FiniteElement_Class, ONLY: Deallocate_FE => DEALLOCATE,  &
& Initiate_FE => Initiate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
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
!                                                                    Size
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_size
CHARACTER(*), PARAMETER :: myName = "anf_size"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP] :: This method is under development')
! ans = obj%tSize
END PROCEDURE anf_size

!----------------------------------------------------------------------------
!                                                               GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetTotalDOF
CHARACTER(*), PARAMETER :: myName = "anf_GetTotalDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP] :: This method is under development')
! IF (ASSOCIATED(obj%domain)) THEN
!   ! ans = GetTotalDOF(obj=fem, dom=obj%domain)
! END IF
!
! IF (ALLOCATED(obj%domains)) THEN
! END IF
END PROCEDURE anf_GetTotalDOF

!----------------------------------------------------------------------------
!                                                          GetTotalVertexDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetTotalVertexDOF
CHARACTER(*), PARAMETER :: myName = "anf_GetTotalVertexDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP] :: This method is under development')
END PROCEDURE anf_GetTotalVertexDOF

!----------------------------------------------------------------------------
!                                                          GetTotalEdgeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetTotalEdgeDOF
CHARACTER(*), PARAMETER :: myName = "anf_GetTotalEdgeDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP] :: This method is under development')
END PROCEDURE anf_GetTotalEdgeDOF

!----------------------------------------------------------------------------
!                                                          GetTotalFaceDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetTotalFaceDOF
CHARACTER(*), PARAMETER :: myName = "anf_GetTotalFaceDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP] :: This method is under development')
END PROCEDURE anf_GetTotalFaceDOF

!----------------------------------------------------------------------------
!                                                          GetTotalCellDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_GetTotalCellDOF
CHARACTER(*), PARAMETER :: myName = "anf_GetTotalCellDOF()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP] :: This method is under development')
END PROCEDURE anf_GetTotalCellDOF

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
  ! CALL obj%fe%Copy(obj2%fe)
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
IF (ALLOCATED(obj%fe)) THEN
  CALL DEALLOCATE_FE(obj%fe)
  ! NOTE: This module is called from FiniteElement_Class
END IF
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
INTEGER(I4B), ALLOCATABLE :: spaceCompo(:)
INTEGER(I4B), ALLOCATABLE :: timeCompo(:)
INTEGER(I4B) :: storageFMT
CHARACTER(1), ALLOCATABLE :: names_char(:)

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP] :: This routine is under development')

! CALL AbstractFieldInitiate(obj=obj, param=param, prefix=prefix, dom=dom)
! CALL Initiate_FE(obj=obj%fe, param=param, dom=dom)
! !INFO: Initiate_FE is defined in FiniteElement_Class
! spaceCompo = obj%GetSpaceCompo()
! timeCompo = obj%GetTimeCompo()
! storageFMT = obj%GetStorageFMT()
! ! names_char = obj%GetNames()
! !FIXME: How to get the names in the vase of block matrix?
!
! IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
!   tNodes = 1
! ELSE
!   tNodes = obj%GetTotalDOF()
! END IF
!
! CALL Initiate( &
!   & obj=obj%dof, &
!   & tNodes=tNodes, &
!   & names=names_char, &
!   & spaceCompo=spaceCompo, &
!   & timeCompo=timeCompo, &
!   & storageFMT=storageFMT)
!
! CALL Initiate(obj%realVec, obj%dof)
!
! obj%tSize = SIZE(obj%realVec)
!
! IF (obj%local_n .EQ. 0) THEN
!   obj%local_n = obj%tSize
! END IF
! IF (obj%global_n .EQ. 0) THEN
!   obj%global_n = obj%tSize
! END IF

END PROCEDURE AbstractNodeFieldInitiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
