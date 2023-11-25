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
USE Mesh_Class
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
!                                                             WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_WriteData_vtk
CHARACTER(*), PARAMETER :: myName = "anf_WriteData_vtk()"
LOGICAL(LGT) :: isOK, isSingleDomain, isMultiDomain
TYPE(Domain_), POINTER :: dom
TYPE(Mesh_), POINTER :: meshPtr
INTEGER(I4B) :: imesh, tMesh, nsd, tPhysicalVars, tComponents, ivar, &
& tnodes, var_rank, var_vartype, itime
INTEGER(I4B), ALLOCATABLE :: nptrs(:), spaceCompo(:), timeCompo(:)
REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), r3(:, :, :), xij(:, :)
CHARACTER(1), ALLOCATABLE :: names(:)
TYPE(FEVariable_) :: fevar

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] WriteData()')
#endif

NULLIFY (dom, meshPtr)

isOK = obj%isInitiated
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: AbstractNodeField_::obj is not isInitiated.')
  RETURN
END IF

isOK = vtk%isOpen()
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: VTKFile_::vtk is not open.')
  RETURN
END IF

isSingleDomain = ASSOCIATED(obj%domain)
isMultiDomain = ALLOCATED(obj%domains)
isOK = isSingleDomain .OR. isMultiDomain
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Either AbstractNodeField_::obj%domain, '// &
    & ' ot AbstractNodeField_::obj%domains not allocated.')
  RETURN
END IF

tPhysicalVars = obj%GetTotalPhysicalVars()
ALLOCATE (names(tPhysicalVars), spaceCompo(tPhysicalVars),  &
  & timeCompo(tPhysicalVars))
CALL obj%GetPhysicalNames(names)
spaceCompo = obj%GetSpaceCompo(tPhysicalVars)
timeCompo = obj%GetTimeCompo(tPhysicalVars)

IF (isSingleDomain) THEN
  dom => obj%domain
  nsd = dom%GetNSD()
  tMesh = dom%GetTotalMesh(dim=nsd)

  DO imesh = 1, tMesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=imesh)

    CALL dom%GetNodeCoord(nodeCoord=xij, dim=nsd, entityNum=imesh)

    CALL meshPtr%ExportToVTK(vtkfile=vtk, nodeCoord=xij,  &
      & openTag=.TRUE., content=.TRUE., closeTag=.FALSE.)

    CALL vtk%WriteDataArray(location=String('node'), action=String('open'))

    nptrs = meshPtr%GetNptrs()
    tnodes = meshPtr%GetTotalNodes()

    DO ivar = 1, tPhysicalVars
      CALL obj%GetFEVariable(globalNode=nptrs, VALUE=fevar, ivar=ivar)

      var_rank = .RANK.fevar
      var_vartype = .vartype.fevar

      SELECT CASE (var_rank)
      CASE (Scalar)
        IF (var_vartype .EQ. Space) THEN
          r1 = Get(fevar, TypeFEVariableScalar, TypeFEVariableSpace)
          CALL vtk%WriteDataArray( &
            & name=String(names(ivar)),  &
            & x=r1,  &
            & numberOfComponents=spaceCompo(ivar))
        END IF

        IF (var_vartype .EQ. SpaceTime) THEN
          r2 = Get(fevar, TypeFEVariableScalar, TypeFEVariableSpaceTime)
          DO itime = 1, timeCompo(ivar)
            CALL vtk%WriteDataArray( &
              & name=String(names(ivar)//"_t"//tostring(itime)),  &
              & x=r2(itime, :),  &
              & numberOfComponents=spaceCompo(ivar))
          END DO
        END IF

      CASE (Vector)
        IF (var_vartype .EQ. Space) THEN
          r2 = Get(fevar, TypeFEVariableVector, TypeFEVariableSpace)
          CALL vtk%WriteDataArray( &
            & name=String(names(ivar)),  &
            & x=r2,  &
            & numberOfComponents=spaceCompo(ivar))
        END IF

        IF (var_vartype .EQ. SpaceTime) THEN
          r3 = Get(fevar, TypeFEVariableVector, TypeFEVariableSpaceTime)
          DO itime = 1, timeCompo(ivar)
            CALL vtk%WriteDataArray( &
              & name=String(names(ivar)//"_t"//tostring(itime)),  &
              & x=r3(:, :, itime),  &
              & numberOfComponents=spaceCompo(ivar))
          END DO
        END IF

      CASE DEFAULT
        CALL e%RaiseError(modName//'::'//myName//' - '// &
          & '[INTERNAL ERROR] :: No case found for fevar')
      END SELECT

    END DO

    CALL vtk%WriteDataArray(location=String('node'), action=String('close'))

    CALL vtk%WritePiece()
  END DO

  CALL DEALLOCATE (fevar)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(r1)) DEALLOCATE (r1)
  IF (ALLOCATED(r2)) DEALLOCATE (r2)
  IF (ALLOCATED(r3)) DEALLOCATE (r3)
  IF (ALLOCATED(names)) DEALLOCATE (names)
  IF (ALLOCATED(spaceCompo)) DEALLOCATE (spaceCompo)
  IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)
  NULLIFY (meshPtr, dom)

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] WriteData()')
#endif
END PROCEDURE anf_WriteData_vtk

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
