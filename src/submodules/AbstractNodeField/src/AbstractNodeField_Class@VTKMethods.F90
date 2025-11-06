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

SUBMODULE(AbstractNodeField_Class) VTKMethods
USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableVector, &
                    TypeFEVariableSpace, &
                    TypeFEVariableSpaceTime, &
                    IntVector_, &
                    TypeFEVariableOpt

USE Display_Method, ONLY: Display, ToString

USE AbstractMesh_Class, ONLY: AbstractMesh_

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
!                                                                ExportToVTK
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportToVTK
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ExportToVTK()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ExportToVTK

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_vtk1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myname = "obj_WriteData_vtk1()"
#endif

LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.
CLASS(AbstractMesh_), POINTER :: meshptr
TYPE(String) :: location, action

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL Writedata_vtk1_CheckError(obj=obj, vtk=vtk)
#endif

meshptr => obj%fedof%GetMeshPointer()
CALL meshptr%ExportToVTK(vtk=vtk, openTag=yes, content=yes, closeTag=no)
location = String('node')
action = String('open')
CALL vtk%WriteDataArray(location=location, action=action)

CALL obj%ExportToVTK(vtk=vtk)

action = String('close')
CALL vtk%WriteDataArray(location=location, action=action)

CALL vtk%WritePiece()
CALL vtk%CLOSE()

NULLIFY (meshptr)
location = ''
action = ''

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_WriteData_vtk1

!----------------------------------------------------------------------------
!                                                             WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_vtk2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteData_vtk2()"
#endif

INTEGER(I4B), PARAMETER :: maxObjSize = 24 ! maximum size of obj
LOGICAL(LGT), PARAMETER :: no = .FALSE., yes = .TRUE.

LOGICAL(LGT) :: isOK
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
REAL(DFP), ALLOCATABLE :: xij(:, :)
CHARACTER(1), ALLOCATABLE :: dofNames(:), dofNames_sub(:)
INTEGER(I4B) :: tfield, iobj, tsize, aint, nrow, ncol, &
                tPhysicalVars(maxObjSize), tnodes

TYPE(IntVector_) :: spaceCompo(maxObjSize), timeCompo(maxObjSize)
CLASS(AbstractNodeField_), POINTER :: obj0
CLASS(FEDOF_), POINTER :: fedof
CLASS(AbstractMesh_), POINTER :: meshptr, meshptr2
TYPE(String) :: location, action

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

NULLIFY (fedof, meshptr, obj0, meshptr2)

tfield = SIZE(obj)

#ifdef DEBUG_VER
isok = tfield .LE. maxObjSize
CALL AssertError1(isok, myName, &
                  'The size of obj ('//ToString(tfield)// &
                  'exceeds the maximum limit of '//ToString(maxObjSize))
#endif

#ifdef DEBUG_VER
isok = vtk%isOpen()
CALL AssertError1(isok, myName, 'VTKFile_::vtk is not open.')
#endif

#ifdef DEBUG_VER
DO iobj = 1, tfield
  obj0 => obj(iobj)%ptr
  isok = ASSOCIATED(obj0)
  IF (.NOT. isok) CYCLE

  isok = obj0%IsInitiated()
  CALL AssertError1(isok, myName, &
            'AbstractNodeField_::obj('//ToString(iobj)//') is not Initiated.')

  isok = ASSOCIATED(obj0%fedof)
  CALL AssertError1(isok, myName, &
      'AbstractNodeField_::obj('//ToString(iobj)//')%fedof is not associated')
END DO
#endif

tsize = 0
DO iobj = 1, tfield

  obj0 => obj(iobj)%ptr
  isok = ASSOCIATED(obj0)

  IF (.NOT. isok) THEN
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
  isok = ASSOCIATED(obj0)
  IF (.NOT. isok) CYCLE

  ALLOCATE (dofNames_sub(tPhysicalVars(iobj)))
  CALL obj0%GetPhysicalNames(dofNames_sub)
  dofNames(tsize + 1:tsize + tPhysicalVars(iobj)) = dofNames_sub
  tsize = tsize + tPhysicalVars(iobj)
  DEALLOCATE (dofNames_sub)

  fedof => obj0%fedof
END DO

meshptr => fedof%GetMeshPointer()
tnodes = meshptr%GetTotalNodes()
ALLOCATE (xij(3, tnodes))
CALL meshptr%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol)

CALL meshptr%ExportToVTK(vtk=vtk, nodeCoord=xij, &
                         openTag=yes, content=yes, closeTag=no)

location = String('node')
action = String('open')
CALL vtk%WriteDataArray(location=location, action=action)

nptrs = meshptr%GetNptrs()

tsize = 0
DO iobj = 1, tfield
  obj0 => obj(iobj)%ptr
  IF (.NOT. ASSOCIATED(obj0)) CYCLE

  aint = tsize + tPhysicalVars(iobj)
  CALL ExportFieldToVTK( &
    obj=obj0, vtk=vtk, nptrs=nptrs, tPhysicalVars=tPhysicalVars(iobj), &
    dofNames=dofNames(tsize + 1:aint), spaceCompo=spaceCompo(iobj)%val, &
    timeCompo=timeCompo(iobj)%val, islocal=no)

  tsize = aint
END DO

action = String('close')
CALL vtk%WriteDataArray(location=location, action=action)
CALL vtk%WritePiece()

IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(xij)) DEALLOCATE (xij)

DEALLOCATE (dofNames)
NULLIFY (meshptr, fedof, obj0, meshptr2)
location = ''
action = ''

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_WriteData_vtk2

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
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ExportToVTK()"
#endif

  INTEGER(I4B) :: ivar, var_rank, var_vartype, itime
  REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), r3(:, :, :)
  TYPE(FEVariable_) :: fevar
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
    CASE (TypeFEVariableOpt%scalar)

      !! ScalarField
      IF (var_vartype .EQ. TypeFEVariableOpt%space) THEN
        r1 = FEVariable_Get(fevar, TypeFEVariableScalar, TypeFEVariableSpace)
        CALL vtk%WriteDataArray(name=String(name), x=r1, &
                                numberOfComponents=spaceCompo(ivar))
      END IF

      !! STScalarField
      IF (var_vartype .EQ. TypeFEVariableOpt%spaceTime) THEN
        r2 = FEVariable_Get(fevar, TypeFEVariableScalar, &
                            TypeFEVariableSpaceTime)
        DO itime = 1, timeCompo(ivar)
          CALL vtk%WriteDataArray(name=String(name//"_t"//ToString(itime)), &
                          x=r2(itime, :), numberOfComponents=spaceCompo(ivar))
        END DO
      END IF

    CASE (TypeFEVariableOpt%vector)

      !! Vector Space
      IF (var_vartype .EQ. TypeFEVariableOpt%space) THEN
        r2 = FEVariable_Get(fevar, TypeFEVariableVector, TypeFEVariableSpace)
        CALL vtk%WriteDataArray(name=String(name), x=r2, &
                                numberOfComponents=spaceCompo(ivar))
      END IF

      !! Vector Space-Time
      IF (var_vartype .EQ. TypeFEVariableOpt%spaceTime) THEN

        r3 = FEVariable_Get(fevar, TypeFEVariableVector, &
                            TypeFEVariableSpaceTime)

        DO itime = 1, timeCompo(ivar)
          CALL vtk%WriteDataArray(name=String(name//"_t"//ToString(itime)), &
                       x=r3(:, :, itime), numberOfComponents=spaceCompo(ivar))
        END DO

      END IF

#ifdef DEBUG_VER
    CASE DEFAULT
      CALL AssertError1(.FALSE., myName, 'No case found for fevar')
#endif

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
!                                                WriteData_vtk1_checkerror
!----------------------------------------------------------------------------

SUBROUTINE Writedata_vtk1_checkerror(obj, vtk)
  CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
  TYPE(VTKFile_), INTENT(INOUT) :: vtk

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Writedata_vtk1_checkerror()"
  LOGICAL(LGT) :: isok, isSingleFEDOF, isMultiFEDOF
  CLASS(AbstractMesh_), POINTER :: meshptr
  CLASS(FEDOF_), POINTER :: fedof
  INTEGER(I4B) :: tPhysicalVars
  INTEGER(I4B), ALLOCATABLE :: spaceCompo(:), timeCompo(:)
  CHARACTER(1), ALLOCATABLE :: dofnames(:)
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = obj%IsInitiated()
  CALL AssertError1(isok, myName, &
                    'AbstractNodeField_::obj is not isInitiated.')
#endif

#ifdef DEBUG_VER
  isok = vtk%isOpen()
  CALL AssertError1(isok, myName, 'VTKFile_::vtk is not open.')
#endif

#ifdef DEBUG_VER
  isSingleFEDOF = ASSOCIATED(obj%fedof)
  isMultiFEDOF = ALLOCATED(obj%fedofs)

  isok = isSingleFEDOF .OR. isMultiFEDOF
  CALL AssertError1(isok, myName, &
                    'Either AbstractNodeField_::obj%fedof, '// &
                    ' or AbstractNodeField_::obj%fedofs not allocated.')
#endif

#ifdef DEBUG_VER
  tPhysicalVars = obj%GetTotalPhysicalVars()
  isok = tPhysicalVars .GT. 0
  CALL AssertError1(isok, myName, &
                    'AbstractNodeField_::obj%GetTotalPhysicalVars() '// &
                    'returned zero.')
#endif

#ifdef DEBUG_VER
  ALLOCATE (dofnames(tPhysicalVars), spacecompo(tPhysicalVars), &
            timecompo(tPhysicalVars))
  CALL obj%GetPhysicalNames(dofnames)
#endif

#ifdef DEBUG_VER
  isok = ALLOCATED(dofnames)
  CALL AssertError1(isok, myname, &
                    'AbstractNodeField_::obj%GetPhysicalNames() '// &
                    'returned unallocated dofnames.')
#endif

#ifdef DEBUG_VER
  isok = SIZE(dofnames) .GT. 0
  CALL AssertError1(isok, myname, &
                    'AbstractNodeField_::obj%GetPhysicalNames() '// &
                    'returned zero size dofnames.')
#endif

#ifdef DEBUG_VER
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
#endif

#ifdef DEBUG_VER
  CALL AssertError1(isSingleFEDOF, myname, &
                    'Multi-FEDOF is not implemented yet')
#endif

#ifdef DEBUG_VER
  fedof => obj%fedof
  isok = ASSOCIATED(fedof)
  CALL AssertError1(isok, myname, &
                    'AbstractNodeField_::obj%fedof is not associated.')
#endif

#ifdef DEBUG_VER
  meshptr => fedof%GetMeshPointer()
  isok = ASSOCIATED(meshptr)
  CALL AssertError1(isok, myname, &
                    'AbstractNodeField_::fedof%GetMeshPointer() '// &
                    'returned unassociated meshptr.')
#endif

#ifdef DEBUG_VER
  fedof => NULL()
  meshptr => NULL()
  IF (ALLOCATED(dofnames)) DEALLOCATE (dofnames)
  IF (ALLOCATED(spaceCompo)) DEALLOCATE (spaceCompo)
  IF (ALLOCATED(timeCompo)) DEALLOCATE (timeCompo)
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Writedata_vtk1_CheckError

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE VTKMethods
