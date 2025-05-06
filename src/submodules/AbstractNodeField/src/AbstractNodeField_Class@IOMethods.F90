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
USE GlobalData, ONLY: Scalar, &
                      Vector, &
                      Space, &
                      SpaceTime
USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableVector, &
                    TypeFEVariableSpace, &
                    TypeFEVariableSpaceTime, &
                    IntVector_

USE Display_Method, ONLY: Display, ToString

USE DOF_Method, ONLY: DOF_Display => Display

USE HDF5File_Method, ONLY: ImportRealVector, &
                           ImportDOF, &
                           ExportRealVector, &
                           ExportDOF

USE AbstractMesh_Class, ONLY: AbstractMesh_

USE AbstractField_Class, ONLY: AbstractFieldImport, &
                               AbstractFieldExport, &
                               AbstractFieldDisplay

USE String_Class, ONLY: String

USE FEVariable_Method, ONLY: OPERATOR(.RANK.), &
                             OPERATOR(.vartype.), &
                             FEVariable_Get => Get, &
                             FEVariable_Deallocate => DEALLOCATE

USE IntVector_Method, ONLY: IntVector_Initiate => Initiate, &
                            ASSIGNMENT(=)

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CALL AbstractFieldDisplay(obj=obj, msg=msg, unitNo=unitNo)
CALL Display(obj%tSize, "tSize: ", unitNo=unitNo)
CALL DOF_Display(obj%realVec, obj%dof, "realVec: ", unitNo=unitNo)
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
TYPE(String) :: dsetname
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

CALL AbstractFieldImport(obj=obj, hdf5=hdf5, group=group, &
                         fedof=fedof, fedofs=fedofs)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

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
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                       ExportFieldToVTK
!----------------------------------------------------------------------------

SUBROUTINE ExportFieldToVTK(obj, vtk, nptrs, tPhysicalVars, dofNames, &
                            spaceCompo, timeCompo, islocal)
  CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
  TYPE(VTKFile_), INTENT(INOUT) :: vtk
  INTEGER(I4B), INTENT(IN) :: nptrs(:)
  !! local or global node numbers
  INTEGER(I4B), INTENT(IN) :: spaceCompo(:), timeCompo(:)
  !! space and time components in each physical variables
  !! the size of spaceCompo and timeCompo should be equal to tPhysicalVars
  INTEGER(I4B), INTENT(IN) :: tPhysicalVars
  !! total number of physical variables
  CHARACTER(1), INTENT(IN) :: dofNames(:)
  !! names of physical variables
  LOGICAL(LGT), INTENT(IN) :: islocal
  !! is nptrs local or global
  !! if true then nptrs are local node numbers

  ! internal variables
  INTEGER(I4B) :: ivar, var_rank, var_vartype, itime
  REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), r3(:, :, :)
  TYPE(FEVariable_) :: fevar
  CHARACTER(*), PARAMETER :: myName = "ExportToVTK()"
  CHARACTER(:), ALLOCATABLE :: name

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  DO ivar = 1, tPhysicalVars
    CALL obj%GetFEVariable(globalNode=nptrs, VALUE=fevar, ivar=ivar, &
                           islocal=islocal)

    name = obj%name%chars()//"_"//dofNames(ivar)
    var_rank = .RANK.fevar
    var_vartype = .vartype.fevar

    SELECT CASE (var_rank)
    CASE (Scalar)

      !! ScalarField
      IF (var_vartype .EQ. Space) THEN
        r1 = FEVariable_Get(fevar, TypeFEVariableScalar, TypeFEVariableSpace)
        CALL vtk%WriteDataArray(name=String(name), x=r1, &
                                numberOfComponents=spaceCompo(ivar))
      END IF

      !! STScalarField
      IF (var_vartype .EQ. SpaceTime) THEN
        r2 = FEVariable_Get(fevar, TypeFEVariableScalar, &
                            TypeFEVariableSpaceTime)
        DO itime = 1, timeCompo(ivar)
          CALL vtk%WriteDataArray(name=String(name//"_t"//ToString(itime)), &
                          x=r2(itime, :), numberOfComponents=spaceCompo(ivar))
        END DO
      END IF

    CASE (Vector)

      !! Vector Space
      IF (var_vartype .EQ. Space) THEN
        r2 = FEVariable_Get(fevar, TypeFEVariableVector, TypeFEVariableSpace)
        CALL vtk%WriteDataArray(name=String(name), x=r2, &
                                numberOfComponents=spaceCompo(ivar))
      END IF

      !! Vector Space-Time
      IF (var_vartype .EQ. SpaceTime) THEN

        r3 = FEVariable_Get(fevar, TypeFEVariableVector, &
                            TypeFEVariableSpaceTime)

        DO itime = 1, timeCompo(ivar)
          CALL vtk%WriteDataArray(name=String(name//"_t"//ToString(itime)), &
                       x=r3(:, :, itime), numberOfComponents=spaceCompo(ivar))
        END DO

      END IF

    CASE DEFAULT

      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: No case found for fevar')

    END SELECT

    DEALLOCATE (name)
  END DO

  CALL FEVariable_DEALLOCATE(fevar)
  IF (ALLOCATED(r1)) DEALLOCATE (r1)
  IF (ALLOCATED(r2)) DEALLOCATE (r2)
  IF (ALLOCATED(r3)) DEALLOCATE (r3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ExportFieldToVTK

!----------------------------------------------------------------------------
!                                                                ExportToVTK
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportToVTK
CHARACTER(*), PARAMETER :: myName = "obj_ExportToVTK()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_ExportToVTK

!----------------------------------------------------------------------------
!                                                WriteData_vtk1_checkerror
!----------------------------------------------------------------------------

SUBROUTINE Writedata_vtk1_checkerror(obj, vtk)
  CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
  TYPE(VTKFile_), INTENT(INOUT) :: vtk

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "Writedata_vtk1_checkerror()"
  LOGICAL(LGT) :: isok, isSingleFEDOF, isMultiFEDOF
  CLASS(AbstractMesh_), POINTER :: meshptr
  CLASS(FEDOF_), POINTER :: fedof
  INTEGER(I4B) :: tPhysicalVars
  INTEGER(I4B), ALLOCATABLE :: spaceCompo(:), timeCompo(:)
  CHARACTER(1), ALLOCATABLE :: dofnames(:)

  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')

  isok = obj%isInitiated
  CALL AssertError1(isok, myName, &
                    'AbstractNodeField_::obj is not isInitiated.')

  isok = vtk%isOpen()
  CALL AssertError1(isok, myName, 'VTKFile_::vtk is not open.')

  isSingleFEDOF = ASSOCIATED(obj%fedof)
  isMultiFEDOF = ALLOCATED(obj%fedofs)

  isok = isSingleFEDOF .OR. isMultiFEDOF
  CALL AssertError1(isok, myName, &
                    'Either AbstractNodeField_::obj%fedof, '// &
                    ' or AbstractNodeField_::obj%fedofs not allocated.')

  tPhysicalVars = obj%GetTotalPhysicalVars()

  isok = tPhysicalVars .GT. 0
  CALL AssertError1(isok, myName, &
                    'AbstractNodeField_::obj%GetTotalPhysicalVars() '// &
                    'returned zero.')

  ALLOCATE (dofnames(tPhysicalVars), spacecompo(tPhysicalVars), &
            timecompo(tPhysicalVars))

  CALL obj%GetPhysicalNames(dofnames)

  isok = ALLOCATED(dofnames)
  CALL AssertError1(isok, myname, &
                    'AbstractNodeField_::obj%GetPhysicalNames() '// &
                    'returned unallocated dofnames.')

  isok = SIZE(dofnames) .GT. 0
  CALL AssertError1(isok, myname, &
                    'AbstractNodeField_::obj%GetPhysicalNames() '// &
                    'returned zero size dofnames.')

  spaceCompo = obj%GetSpaceCompo(tPhysicalVars)
  timeCompo = obj%GetTimeCompo(tPhysicalVars)

  isok = ALL(spaceCompo .GT. 0)
  CALL AssertError1(isok, myname, &
                    'AbstractNodeField_::obj%GetSpaceCompo() '// &
                    'returned zero spaceCompo.')
  isok = ALL(timeCompo .GT. 0)
  CALL AssertError1(isok, myname, &
                    'AbstractNodeField_::obj%GetTimeCompo() '// &
                    'returned zero timeCompo.')

  CALL AssertError1(isSingleFEDOF, myname, &
                    'Multi-FEDOF is not implemented yet')

  fedof => obj%fedof
  isok = ASSOCIATED(fedof)
  CALL AssertError1(isok, myname, &
                    'AbstractNodeField_::obj%fedof is not associated.')

  meshptr => fedof%GetMeshPointer()
  isok = ASSOCIATED(meshptr)
  CALL AssertError1(isok, myname, &
                    'AbstractNodeField_::fedof%GetMeshPointer() '// &
                    'returned unassociated meshptr.')

  fedof => NULL()
  meshptr => NULL()

  IF (ALLOCATED(dofnames)) DEALLOCATE (dofnames)
  IF (ALLOCATED(spaceCompo)) DEALLOCATE (spaceCompo)
  IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)

  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')

END SUBROUTINE Writedata_vtk1_checkerror

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_vtk1
CHARACTER(*), PARAMETER :: myname = "obj_WriteData_vtk1()"
LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.

CLASS(AbstractMesh_), POINTER :: meshptr
TYPE(String) :: location, action

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL Writedata_vtk1_checkerror(obj=obj, vtk=vtk)
#endif

meshptr => obj%fedof%GetMeshPointer()
CALL meshptr%ExportToVTK(vtk=vtk, openTag=yes, content=yes, closeTag=no)
location = String('node')
action = String('open')
CALL vtk%WriteDataArray(location=location, action=action)

! Call ExporToVTK
! CALL ExportFieldToVTK(obj, vtk, nptrs, tPhysicalVars, dofNames, &
!                       spaceCompo, timeCompo, .FALSE.)
CALL obj%ExportToVTK(vtk=vtk)

action = String('close')
CALL vtk%WriteDataArray(location=location, action=action)

CALL vtk%WritePiece()
CALL vtk%CLOSE()

NULLIFY (meshptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_WriteData_vtk1

!----------------------------------------------------------------------------
!                                                             WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_vtk2
CHARACTER(*), PARAMETER :: myName = "obj_WriteData_vtk2()"
LOGICAL(LGT) :: isOK
INTEGER(I4B) :: tnodes
INTEGER(I4B), ALLOCATABLE :: nptrs(:), tPhysicalVars(:)
REAL(DFP), ALLOCATABLE :: xij(:, :)
CHARACTER(1), ALLOCATABLE :: dofNames(:), dofNames_sub(:)
INTEGER(I4B) :: tfield, iobj, tsize, aint, nrow, ncol
TYPE(IntVector_), ALLOCATABLE :: spaceCompo(:), timeCompo(:)
CLASS(AbstractNodeField_), POINTER :: obj0
CLASS(FEDOF_), POINTER :: fedof
CLASS(AbstractMesh_), POINTER :: meshptr, meshptr2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

tfield = SIZE(obj)
ALLOCATE (tPhysicalVars(tfield), spaceCompo(tfield), &
          timeCompo(tfield))

NULLIFY (fedof, meshptr, obj0, meshptr2)

isOK = vtk%isOpen()
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: VTKFile_::vtk is not open.')
  RETURN
END IF

DO iobj = 1, tfield
  obj0 => obj(iobj)%ptr
  IF (.NOT. ASSOCIATED(obj0)) CYCLE

  isOK = obj0%isInitiated
  IF (.NOT. isOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: AbstractNodeField_:: '// &
                      'obj('//ToString(iobj)//') is not Initiated.')
    RETURN
  END IF

  isOK = ASSOCIATED(obj0%fedof)
  IF (.NOT. isOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: AbstractNodeField_::'// &
                      'obj('//ToString(iobj)//')%fedof is not associated')
    RETURN
  END IF

  ! FIXME: I think this should be meshptr instead of fedof
  ! We want mesh to be same for all fields
  IF (.NOT. ASSOCIATED(fedof)) THEN
    fedof => obj0%fedof
    meshptr => fedof%GetMeshPointer()
  ELSE
    meshptr2 => obj0%fedof%GetMeshPointer()
    isOK = ASSOCIATED(meshptr, meshptr2)
    IF (.NOT. isOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: AbstractNodeField_ :: '// &
                        'associated mesh should  be the same ')
    END IF
  END IF
END DO

tsize = 0
DO iobj = 1, tfield

  obj0 => obj(iobj)%ptr
  IF (.NOT. ASSOCIATED(obj0)) THEN
    tPhysicalVars(iobj) = 0
    CALL IntVector_Initiate(spaceCompo(iobj), 0)
    CALL IntVector_Initiate(timeCompo(iobj), 0)

  ELSE
    tPhysicalVars(iobj) = obj0%GetTotalPhysicalVars()
    spaceCompo(iobj) = obj0%GetSpaceCompo(tPhysicalVars(iobj))
    timeCompo(iobj) = obj0%GetTimeCompo(tPhysicalVars(iobj))
  END IF

  tsize = tsize + tPhysicalVars(iobj)

END DO

ALLOCATE (dofNames(tsize))

tsize = 0
DO iobj = 1, tfield
  obj0 => obj(iobj)%ptr
  IF (.NOT. ASSOCIATED(obj0)) CYCLE

  ALLOCATE (dofNames_sub(tPhysicalVars(iobj)))
  CALL obj0%GetPhysicalNames(dofNames_sub)
  dofNames(tsize + 1:tsize + tPhysicalVars(iobj)) = dofNames_sub
  tsize = tsize + tPhysicalVars(iobj)
  DEALLOCATE (dofNames_sub)
END DO

tnodes = meshptr%GetTotalNodes()
ALLOCATE (xij(3, tnodes))
CALL meshptr%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol)

CALL meshptr%ExportToVTK(vtk=vtk, nodeCoord=xij, &
                         openTag=.TRUE., content=.TRUE., closeTag=.FALSE.)

CALL vtk%WriteDataArray(location=String('node'), action=String('open'))

nptrs = meshptr%GetNptrs()

tsize = 0
DO iobj = 1, tfield
  obj0 => obj(iobj)%ptr
  IF (.NOT. ASSOCIATED(obj0)) CYCLE

  aint = tsize + tPhysicalVars(iobj)
  CALL ExportFieldToVTK(obj0, vtk, nptrs, tPhysicalVars(iobj), &
        dofNames(tsize + 1:aint), spaceCompo(iobj)%val, timeCompo(iobj)%val, &
                        .FALSE.)
  tsize = aint

END DO

CALL vtk%WriteDataArray(location=String('node'), action=String('close'))
CALL vtk%WritePiece()

IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
DEALLOCATE (tPhysicalVars)
DEALLOCATE (dofNames)
DEALLOCATE (spaceCompo)
DEALLOCATE (timeCompo)
NULLIFY (meshptr, fedof, obj0, meshptr2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_WriteData_vtk2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
