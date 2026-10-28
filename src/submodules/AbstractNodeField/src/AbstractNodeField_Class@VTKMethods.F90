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
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableVector
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE BaseType, ONLY: IntVector_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE ElemshapeData_Method, ONLY: ElemshapeDataDeallocate => DEALLOCATE
USE FEVariable_Method, ONLY: OPERATOR(.RANK.)
USE FEVariable_Method, ONLY: OPERATOR(.vartype.)
USE FEVariable_Method, ONLY: FEVariableGet => Get
USE FEVariable_Method, ONLY: FEVariableDeallocate => DEALLOCATE
USE IntVector_Method, ONLY: IntVectorInitiate => Initiate
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE QuadraturePoint_Method, ONLY: QuadraturePointInitiate => Initiate
USE QuadraturePoint_Method, ONLY: QuadraturePointDeallocate => DEALLOCATE
USE ReallocateUtility, ONLY: Reallocate
USE ReferenceElement_Method, ONLY: GetVTKElementTypeFromTopology_
USE String_Class, ONLY: String
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: &
  modName = "AbstractNodeField_Class@VTKMethods.F90"
#endif

INTEGER(I4B), PARAMETER :: MAX_VTK_NODES = 1331

CONTAINS

!----------------------------------------------------------------------------
!                                                                ExportToVTK
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportToVTK
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_ExportToVTK()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                    '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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

  CLASS(AbstractMesh_), POINTER :: meshptr
  TYPE(String) :: location, action

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myname // ' - ' // &
                          '[START]')
#endif

! Check error
#ifdef DEBUG_VER
  CALL Writedata_vtk1_checkerror(obj=obj, vtk=vtk)
#endif

! Export mesh to VTKFile_
  meshptr => obj%fedof%GetMeshPointer()
  CALL meshptr%ExportToVTK(vtk=vtk, openTag=math%yes, content=math%yes, &
                           closeTag=math%no)
  location = String('node')
  action = String('open')
  CALL vtk%WriteDataArray(location=location, action=action)

! Export data to VTKFile_
  CALL obj%ExportToVTK(vtk=vtk)

  action = String('close')
  CALL vtk%WriteDataArray(location=location, action=action)

  CALL vtk%WritePiece()
  CALL vtk%CLOSE()

  NULLIFY(meshptr)
  location = ''
  action = ''

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myname // ' - ' // &
                          '[END]')
#endif
END PROCEDURE obj_WriteData_vtk1

!----------------------------------------------------------------------------
!                                                                  WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_vtk2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_WriteData_vtk2()"
#endif

  INTEGER(I4B), PARAMETER :: maxObjSize = 24
! maximum size of obj

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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START]')
#endif

  NULLIFY(fedof, meshptr, obj0, meshptr2)

  tfield = SIZE(obj)

#ifdef DEBUG_VER
  isOK = tfield .LE. maxObjSize
  CALL AssertError1(isOK, myName, &
                    'The size of obj (' // ToString(tfield) // &
                    'exceeds the maximum limit of ' // ToString(maxObjSize))
#endif

#ifdef DEBUG_VER
  isOK = vtk%isOpen()
  CALL AssertError1(isOK, myName, 'VTKFile_::vtk is not open.')
#endif

#ifdef DEBUG_VER
  loop1: DO iobj = 1, tfield
    obj0 => obj(iobj)%ptr
    isOK = ASSOCIATED(obj0)
    IF (.NOT. isOK) CYCLE loop1

    isOK = obj0%IsInitiated()
    CALL AssertError1(isOK, myName, &
                      'AbstractNodeField_::obj(' // &
                      ToString(iobj) // ') is not Initiated.')

    isOK = ASSOCIATED(obj0%fedof)
    CALL AssertError1(isOK, myName, &
                      'AbstractNodeField_::obj(' // &
                      ToString(iobj) // ')%fedof is not associated')
  END DO loop1
#endif

  tsize = 0
  DO iobj = 1, tfield

    obj0 => obj(iobj)%ptr
    isOK = ASSOCIATED(obj0)

    IF (.NOT. isOK) THEN
      tPhysicalVars(iobj) = 0
      CALL IntVectorInitiate(spaceCompo(iobj), 0)
      CALL IntVectorInitiate(timeCompo(iobj), 0)

    ELSE
      tPhysicalVars(iobj) = obj0%GetTotalPhysicalVars()
      spaceCompo(iobj) = obj0%GetSpaceCompo(tPhysicalVars(iobj))
      timeCompo(iobj) = obj0%GetTimeCompo(tPhysicalVars(iobj))
    END IF

    tsize = tsize + tPhysicalVars(iobj)

  END DO

  ALLOCATE(dofNames(tsize))

  tsize = 0
  loop2: DO iobj = 1, tfield
    obj0 => obj(iobj)%ptr
    isOK = ASSOCIATED(obj0)
    IF (.NOT. isOK) CYCLE loop2

    ALLOCATE(dofNames_sub(tPhysicalVars(iobj)))
    CALL obj0%GetPhysicalNames(dofNames_sub)
    dofNames(tsize + 1:tsize + tPhysicalVars(iobj)) = dofNames_sub
    tsize = tsize + tPhysicalVars(iobj)
    DEALLOCATE(dofNames_sub)

    fedof => obj0%fedof
  END DO loop2

  meshptr => fedof%GetMeshPointer()
  tnodes = meshptr%GetTotalNodes()
  ALLOCATE(xij(3, tnodes))
  CALL meshptr%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol)

  CALL meshptr%ExportToVTK(vtk=vtk, nodeCoord=xij, &
                           openTag=math%yes, content=math%yes, &
                           closeTag=math%no)

  location = String('node')
  action = String('open')
  CALL vtk%WriteDataArray(location=location, action=action)

  nptrs = meshptr%GetNptrs()

  tsize = 0
  loop3: DO iobj = 1, tfield
    obj0 => obj(iobj)%ptr
    IF (.NOT. ASSOCIATED(obj0)) CYCLE loop3

    aint = tsize + tPhysicalVars(iobj)
    CALL ExportFieldToVTK(obj=obj0, vtk=vtk, nptrs=nptrs, &
                          tPhysicalVars=tPhysicalVars(iobj), &
                          dofNames=dofNames(tsize + 1:aint), &
                          spaceCompo=spaceCompo(iobj)%val, &
                          timeCompo=timeCompo(iobj)%val, islocal=math%no)

    tsize = aint
  END DO loop3

  action = String('close')
  CALL vtk%WriteDataArray(location=location, action=action)
  CALL vtk%WritePiece()

  IF (ALLOCATED(nptrs)) DEALLOCATE(nptrs)
  IF (ALLOCATED(xij)) DEALLOCATE(xij)

  DEALLOCATE(dofNames)
  NULLIFY(meshptr, fedof, obj0, meshptr2)
  location = ''
  action = ''

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  DO ivar = 1, tPhysicalVars
    CALL obj%GetFEVariable(globalNode=nptrs, VALUE=fevar, ivar=ivar, &
                           islocal=islocal)

    name = obj%name%chars() // "_" // dofNames(ivar)
    var_rank = .RANK. fevar
    var_vartype = .vartype. fevar

    SELECT CASE (var_rank)
    CASE (TypeFEVariableOpt%scalar)

      !! ScalarField
      IF (var_vartype .EQ. TypeFEVariableOpt%space) THEN
        r1 = FEVariableGet(fevar, TypeFEVariableScalar, &
                           TypeFEVariableSpace)
        CALL vtk%WriteDataArray(name=String(name), x=r1, &
                                numberOfComponents=spaceCompo(ivar))
      END IF

      !! STScalarField
      IF (var_vartype .EQ. TypeFEVariableOpt%spaceTime) THEN
        r2 = FEVariableGet(fevar, TypeFEVariableScalar, &
                           TypeFEVariableSpaceTime)
        DO itime = 1, timeCompo(ivar)
          CALL vtk%WriteDataArray(name=String(name // "_t" // &
                                              ToString(itime)), x=r2(itime, &
                                                                     :), &
                                  numberOfComponents=spaceCompo(ivar))
        END DO
      END IF

    CASE (TypeFEVariableOpt%vector)

      !! Vector Space
      IF (var_vartype .EQ. TypeFEVariableOpt%space) THEN
        r2 = FEVariableGet(fevar, TypeFEVariableVector, &
                           TypeFEVariableSpace)
        CALL vtk%WriteDataArray(name=String(name), x=r2, &
                                numberOfComponents=spaceCompo(ivar))
      END IF

      !! Vector Space-Time
      IF (var_vartype .EQ. TypeFEVariableOpt%spaceTime) THEN

        r3 = FEVariableGet(fevar, TypeFEVariableVector, &
                           TypeFEVariableSpaceTime)

        DO itime = 1, timeCompo(ivar)
          CALL vtk%WriteDataArray(name=String(name // "_t" // &
                                              ToString(itime)), x=r3(:, :, &
                                                                     itime), &
                                  numberOfComponents=spaceCompo(ivar))
        END DO

      END IF

    CASE DEFAULT

#ifdef DEBUG_VER
      CALL AssertError1(math%no, myName, 'No case found for fevar')
#endif

    END SELECT

    DEALLOCATE(name)
  END DO

  CALL FEVariableDEALLOCATE(fevar)
  IF (ALLOCATED(r1)) DEALLOCATE(r1)
  IF (ALLOCATED(r2)) DEALLOCATE(r2)
  IF (ALLOCATED(r3)) DEALLOCATE(r3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
                    'Either AbstractNodeField_::obj%fedof, ' // &
                    ' or AbstractNodeField_::obj%fedofs not allocated.')
#endif

#ifdef DEBUG_VER
  tPhysicalVars = obj%GetTotalPhysicalVars()
  isok = tPhysicalVars .GT. 0
  CALL AssertError1(isok, myName, &
                    'AbstractNodeField_::obj%GetTotalPhysicalVars() ' // &
                    'returned zero.')
#endif

#ifdef DEBUG_VER
  ALLOCATE(dofnames(tPhysicalVars), spaceCompo(tPhysicalVars), &
           timeCompo(tPhysicalVars))
  CALL obj%GetPhysicalNames(dofnames)
#endif

#ifdef DEBUG_VER
  isok = ALLOCATED(dofnames)
  CALL AssertError1(isok, myName, &
                    'AbstractNodeField_::obj%GetPhysicalNames() ' // &
                    'returned unallocated dofnames.')
#endif

#ifdef DEBUG_VER
  isok = SIZE(dofnames) .GT. 0
  CALL AssertError1(isok, myName, &
                    'AbstractNodeField_::obj%GetPhysicalNames() ' // &
                    'returned zero size dofnames.')
#endif

#ifdef DEBUG_VER
  spaceCompo = obj%GetSpaceCompo(tPhysicalVars)
  timeCompo = obj%GetTimeCompo(tPhysicalVars)

  isok = ALL(spaceCompo .GT. 0)
  CALL AssertError1(isok, myName, &
                    'AbstractNodeField_::obj%GetSpaceCompo() ' // &
                    'returned zero spaceCompo.')
  isok = ALL(timeCompo .GT. 0)
  CALL AssertError1(isok, myName, &
                    'AbstractNodeField_::obj%GetTimeCompo() ' // &
                    'returned zero timeCompo.')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(isSingleFEDOF, myName, &
                    'Multi-FEDOF is not implemented yet')
#endif

#ifdef DEBUG_VER
  fedof => obj%fedof
  isok = ASSOCIATED(fedof)
  CALL AssertError1(isok, myName, &
                    'AbstractNodeField_::obj%fedof is not associated.')
#endif

#ifdef DEBUG_VER
  meshptr => fedof%GetMeshPointer()
  isok = ASSOCIATED(meshptr)
  CALL AssertError1(isok, myName, &
                    'AbstractNodeField_::fedof%GetMeshPointer() ' // &
                    'returned unassociated meshptr.')
#endif

#ifdef DEBUG_VER
  fedof => NULL()
  meshptr => NULL()
  IF (ALLOCATED(dofnames)) DEALLOCATE(dofnames)
  IF (ALLOCATED(spaceCompo)) DEALLOCATE(spaceCompo)
  IF (ALLOCATED(timeCompo)) DEALLOCATE(timeCompo)
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END SUBROUTINE Writedata_vtk1_checkerror

!----------------------------------------------------------------------------
!                                                 GetVtkUnstructuredGridData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVtkUnstructuredGridData1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetVtkUnstructuredGridData1()"
#endif

  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  REAL(DFP), ALLOCATABLE :: refElemCoord(:, :), xij(:, :), &
    elemCoord(:, :)
  INTEGER(I4B), ALLOCATABLE :: vtkindx(:)
  CLASS(FEDOF_), POINTER :: geofedof, fedof
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: maxNNE, maxCon, points_nrow, points_ncol, &
    con_tsize, offsets_tsize, types_tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (PRESENT(ivar)) THEN
    !add checks
#ifdef DEBUG_VER
    isok = ALLOCATED(obj%fedofs)
    CALL AssertError1(isok, myName, "obj%fedofs not allocated.")

    CALL AssertError3(ivar, SIZE(obj%fedofs), myName, &
                      "a=ivar, b=size(obj%fedofs)")

    isok = ASSOCIATED(obj%fedofs(ivar)%ptr)
    CALL AssertError1(isok, myName, "obj%fedofs(ivar) not associated.")

    isok = ALLOCATED(obj%geofedofs)
    CALL AssertError1(isok, myName, "obj%geofedofs not allocated.")

    CALL AssertError3(ivar, SIZE(obj%geofedofs), myName, &
                      "a=ivar, b=size(obj%geofedofs)")

    isok = ASSOCIATED(obj%geofedofs(ivar)%ptr)
    CALL AssertError1(isok, myName, "obj%geofedofs(ivar) not associated.")
#endif

    fedof => obj%fedofs(ivar)%ptr
    geofedof => obj%geofedofs(ivar)%ptr

  ELSE
    fedof => obj%fedof
    geofedof => obj%geofedof
  END IF

#ifdef DEBUG_VER
  isok = geofedof%IsInitiated()
  CALL AssertError1(isok, myName, "geofedof is not initiated.")

  isok = fedof%IsInitiated()
  CALL AssertError1(isok, myName, "fedof is not initiated.")
#endif

  maxNNE = geofedof%GetMaxTotalConnectivity()
  maxCon = fedof%GetMaxTotalConnectivity()

  con_tsize = obj%fedof%GetSumElemTotalDOF()
  points_nrow = 3
  points_ncol = con_tsize

  types_tsize = obj%fedof%GetTotalActiveCells()
  offsets_tsize = types_tsize + 1

  CALL Reallocate(vtkindx, maxCon)
  CALL Reallocate(connectivity, con_tsize)
  CALL Reallocate(points, points_nrow, points_ncol)
  CALL Reallocate(offsets, offsets_tsize)
  CALL Reallocate(types, types_tsize)
  CALL Reallocate(refElemCoord, 3, maxNNE)
  CALL Reallocate(elemCoord, 3, maxNNE)
  CALL Reallocate(xij, 4, maxCon)

  CALL obj%GetVtkUnstructuredGridData2(points=points, &
                                       connectivity=connectivity, &
                                       offsets=offsets, types=types, &
                                       refElemCoord=refElemCoord, xij=xij, &
                                       elemCoord=elemCoord, vtkindx=vtkindx, &
                                       points_nrow=points_nrow, &
                                       points_ncol=points_ncol, &
                                       con_tsize=con_tsize, &
                                       offsets_tsize=offsets_tsize, &
                                       types_tsize=types_tsize, ivar=ivar)

  NULLIFY(feptr, fedof, geofeptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetVtkUnstructuredGridData1

!----------------------------------------------------------------------------
!                                                 GetVtkUnstructuredGridData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVtkUnstructuredGridData2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetVtkUnstructuredGridData2()"
  LOGICAL(LGT) :: isok
#endif

  CLASS(AbstractMesh_), POINTER :: mesh
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  CLASS(FEDOF_), POINTER :: geofedof, fedof
  TYPE(QuadraturePoint_) :: quad
  TYPE(ElemShapeData_) :: geoelemsd
  INTEGER(I4B) :: tElements, iel, ii, nPoints, nCells, &
    icell, ip0, ic0, maxNNE, maxCon, cellOrder(3), &
    ipType(3), topo, vtkindx_tsize, refElemCoord_nrow, &
    refElemCoord_ncol, xij_nrow, xij_ncol, elemCoord_nrow, &
    elemCoord_ncol
  INTEGER(INT8) :: vtktype

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (PRESENT(ivar)) THEN
    !add checks
#ifdef DEBUG_VER
    isok = ALLOCATED(obj%fedofs)
    CALL AssertError1(isok, myName, "obj%fedofs not allocated.")

    CALL AssertError3(ivar, SIZE(obj%fedofs), myName, &
                      "a=ivar, b=size(obj%fedofs)")

    isok = ASSOCIATED(obj%fedofs(ivar)%ptr)
    CALL AssertError1(isok, myName, "obj%fedofs(ivar) not associated.")

    isok = ALLOCATED(obj%geofedofs)
    CALL AssertError1(isok, myName, "obj%geofedofs not allocated.")

    CALL AssertError3(ivar, SIZE(obj%geofedofs), myName, &
                      "a=ivar, b=size(obj%geofedofs)")

    isok = ASSOCIATED(obj%geofedofs(ivar)%ptr)
    CALL AssertError1(isok, myName, "obj%geofedofs(ivar) not associated.")
#endif

    fedof => obj%fedofs(ivar)%ptr
    geofedof => obj%geofedofs(ivar)%ptr

  ELSE
    fedof => obj%fedof
    geofedof => obj%geofedof
  END IF

#ifdef DEBUG_VER
  isok = geofedof%IsInitiated()
  CALL AssertError1(isok, myName, "geofedof is not initiated.")

  isok = fedof%IsInitiated()
  CALL AssertError1(isok, myName, "fedof is not initiated.")
#endif

  mesh => fedof%GetMeshPointer()

  tElements = obj%fedof%GetTotalCells()
  maxNNE = geofedof%GetMaxTotalConnectivity()
  maxCon = fedof%GetMaxTotalConnectivity()
  ipType = TypeInterpolationOpt%Equidistance
  nPoints = obj%fedof%GetSumElemTotalDOF()
  nCells = obj%fedof%GetTotalActiveCells()

#ifdef DEBUG_VER
  CALL AssertError3(maxCon, SIZE(vtkindx), myName, &
                    "a=maxCon, b=size(vtkindx)")

  CALL AssertError3(nPoints, SIZE(connectivity), myName, &
                    "a=nPoints, b=size(connectivity)")

  CALL AssertError3(nCells + 1, SIZE(offsets), myName, &
                    "a=nCells+1, b=size(offsets)")

  CALL AssertError3(nCells, SIZE(types), myName, &
                    "a=nCells, b=size(types)")

  CALL AssertError3(math%three_i, SIZE(points, 1), myName, &
                    "a=3, b=size(points,1)")
  CALL AssertError3(nPoints, SIZE(points, 2), myName, &
                    "a=nPoints, b=size(points,2)")

  CALL AssertError3(math%three_i, SIZE(elemCoord, 1), myName, &
                    "a=3, b=size(elemCoord,1)")
  CALL AssertError3(maxNNE, SIZE(elemCoord, 2), myName, &
                    "a=maxNNE, b=size(elemCoord,2)")

  CALL AssertError3(math%three_i, SIZE(refElemCoord, 1), myName, &
                    "a=3, b=size(refElemCoord,1)")
  CALL AssertError3(maxNNE, SIZE(refElemCoord, 2), myName, &
                    "a=maxNNE, b=size(refElemCoord,2)")

  CALL AssertError3(math%four_i, SIZE(xij, 1), myName, &
                    "a=3, b=size(xij,1)")
  CALL AssertError3(maxNNE, SIZE(xij, 2), myName, &
                    "a=maxNNE, b=size(xij,2)")
#endif

! Second pass: points, cells, and values
  icell = 0
  ip0 = 0
  ic0 = 0
  offsets(1) = 0
  xij(1:4, 1:maxCon) = math%zero

  points_nrow = 3
  points_ncol = nPoints
  con_tsize = nPoints
  offsets_tsize = nCells + 1
  types_tsize = nCells

  loop1: DO iel = 1, tElements

    isok = mesh%isElementActive(globalElement=iel, islocal=math%yes)
    IF (.NOT. isok) CYCLE loop1

    ! Get cellOrder
    CALL fedof%GetCellOrder(ans=cellOrder, tsize=ii, &
                            globalElement=iel, islocal=math%yes)
    cellOrder(2:3) = cellOrder(1)

    ! Get vtk element type from topology
    topo = mesh%GetElemTopology(globalElement=iel, islocal=math%yes)
    CALL GetVTKElementTypeFromTopology_(topoType=topo, &
                                        order=cellOrder(1), &
                                        vtk_type=vtktype, &
                                        nptrs=vtkindx, tsize=vtkindx_tsize)

    ! get feptr
    CALL fedof%SetFE(globalElement=iel, islocal=math%yes)
    feptr => fedof%GetFEPointer(globalElement=iel, islocal=math%yes)

    ! get geofeptr
    CALL geofedof%SetFE(globalElement=iel, islocal=math%yes)
    geofeptr => geofedof%GetFEPointer(globalElement=iel, islocal=math%yes)

    ! get reference element coordinates
    CALL feptr%GetRefElemCoord(ans=refElemCoord, nrow=refElemCoord_nrow, &
                               ncol=refElemCoord_ncol)

    ! xij = math%zero
    CALL feptr%GetInterpolationPoints(xij=refElemCoord(1:refElemCoord_nrow, &
                                                       1:refElemCoord_ncol), &
                                      ans=xij, nrow=xij_nrow, ncol=xij_ncol, &
                                      order=cellOrder, ipType=ipType)

    ! TODO: make the following call allocation free
    CALL QuadraturePointInitiate(obj=quad, points=xij(1:4, 1:xij_ncol))

    ! get local element shape data of geometry
    CALL geofeptr%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)

    ! get nodal coordinates of element
    CALL mesh%GetNodeCoord(globalElement=iel, nodeCoord=elemCoord, &
                           nrow=elemCoord_nrow, ncol=elemCoord_ncol, &
                           islocal=math%yes)

#ifdef DEBUG_VER
    CALL AssertError3(elemCoord_ncol, geoelemsd%nns, myName, &
                      "a=elemCoord_ncol, b=geoelemsd%nns.")

    CALL AssertError3(vtkindx_tsize, geoelemsd%nips, myName, &
                      "a=vtkindx_tsize, b=geoelemsd%nips.")
#endif

    points(1:elemCoord_nrow, ip0 + 1:ip0 + vtkindx_tsize) = &
      MATMUL(elemCoord(1:elemCoord_nrow, 1:elemCoord_ncol), &
                 geoelemsd%N(1:elemCoord_ncol, 1:vtkindx_tsize))

    DO ii = 1, vtkindx_tsize
      connectivity(ic0 + ii) = ip0 + vtkindx(ii) - 1
    END DO

    icell = icell + 1
    offsets(icell + 1) = ic0 + vtkindx_tsize
    types(icell) = vtktype

    ip0 = ip0 + vtkindx_tsize
    ic0 = ic0 + vtkindx_tsize
  END DO loop1

  CALL QuadraturePointDeallocate(quad)
  CALL ElemshapeDataDeallocate(geoelemsd)
  NULLIFY(mesh, fedof, geofedof, feptr, geofeptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetVtkUnstructuredGridData2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE VTKMethods
