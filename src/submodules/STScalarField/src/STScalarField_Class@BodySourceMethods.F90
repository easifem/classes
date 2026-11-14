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

SUBMODULE(STScalarField_Class) BodySourceMethods
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE ElemshapeData_Method, ONLY: ElemshapeData_Deallocate => DEALLOCATE
USE FEVariable_Method, ONLY: FEVariable_Deallocate => DEALLOCATE
USE FEVariable_Method, ONLY: FEVariable_Set => Set
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE ForceVector_Method, ONLY: ForceVector_
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Deallocate => DEALLOCATE
USE ReallocateUtility, ONLY: Reallocate
USE STForceVector_Method, ONLY: STForceVector_

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
USE ElemshapeData_Method, ONLY: ElemshapeData_Display => Display
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Display => Display
#endif

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "STScalarField_Class@BodySourceMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                             ApplyBodySource
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyBodySource1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyBodySource1()"
#endif

INTEGER(I4B) :: iel, tElements, maxNNS, maxNNT, maxNNSGeo, tcellCon, xij_i, &
                xij_j, maxNips, ips, maxNipt, ipt, forceVec_i, forceVec_j
INTEGER(I4B), ALLOCATABLE :: cellcon(:)
REAL(DFP) :: args(4)
REAL(DFP), ALLOCATABLE :: xij(:, :), forceVec(:, :), forceVecQuad(:, :)
TYPE(QuadraturePoint_) :: quad, timeQuad
TYPE(ElemshapeData_) :: elemsd, geoelemsd, timeelemsd, timegeoelemsd
TYPE(FEVariable_) :: forceVar
CLASS(AbstractFE_), POINTER :: feptr, geofeptr
CLASS(AbstractOneDimFE_), POINTER :: timefeptr
CLASS(AbstractMesh_), POINTER :: mesh

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

mesh => obj%fedof%GetMeshPointer()
tElements = mesh%GetTotalElements()

CALL obj%timefedof%SetFE()
timefeptr => obj%timefedof%GetFEPointer()

CALL timefeptr%GetGlobalTimeElemShapeData( &
  elemsd=timeelemsd, geoelemsd=timegeoelemsd, quad=timeQuad, times=times)

maxNNSGeo = obj%geofedof%GetMaxTotalConnectivity()
maxNNS = obj%fedof%GetMaxTotalConnectivity()
maxNNT = obj%timefedof%GetMaxTotalConnectivity()
maxNips = obj%fedof%GetMaxTotalQuadraturePoints()
maxNipt = obj%timefedof%GetMaxTotalQuadraturePoints()

CALL Reallocate(xij, 3, maxNNSGeo)
CALL Reallocate(cellcon, maxNNS)
CALL Reallocate(forceVec, maxNNS, maxNNT)
CALL Reallocate(forceVecQuad, maxNips, maxNipt)

forceVar = QuadratureVariable( &
           nrow=maxNips, ncol=maxNipt, RANK=TypeFEVariableScalar, &
           vartype=TypeFEVariableSpaceTime)

args = 0.0_DFP

DO iel = 1, tElements

  CALL obj%fedof%SetFE(globalElement=iel, islocal=math%yes)
  CALL obj%geofedof%SetFE(globalElement=iel, islocal=math%yes)

  feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=math%yes)
  geofeptr => obj%geofedof%GetFEPointer(globalElement=iel, islocal=math%yes)

  CALL obj%fedof%GetConnectivity_( &
    globalElement=iel, islocal=math%yes, ans=cellcon, tsize=tcellCon, &
    opt="A")

  CALL mesh%GetNodeCoord( &
    nodeCoord=xij, nrow=xij_i, ncol=xij_j, islocal=math%yes, &
    globalElement=iel)

  CALL feptr%GetGlobalElemShapeData2( &
    geofeptr=geofeptr, elemsd=elemsd, geoelemsd=geoelemsd, xij=xij, &
    quad=quad)

  DO ipt = 1, timeelemsd%nips
    args(4) = timeelemsd%coord(1, ipt)
    DO ips = 1, elemsd%nips
      args(1:elemsd%nsd) = elemsd%coord(1:elemsd%nsd, ips)
      CALL bodySource%Get(val=forceVecQuad(ips, ipt), args=args)
    END DO
  END DO

  CALL FEVariable_Set( &
    obj=forceVar, val=forceVecQuad(1:elemsd%nips, 1:timeelemsd%nips), &
    RANK=TypeFEVariableScalar, varType=TypeFEVariableSpaceTime, &
    scale=1.0_DFP, addContribution=math%no)

  CALL STForceVector_( &
    testSpace=elemsd, testTime=timeelemsd, c=forceVar, &
    crank=TypeFEVariableScalar, ans=forceVec, nrow=forceVec_i, &
    ncol=forceVec_j)

  CALL obj%Set( &
    globalNode=cellcon(1:tcellCon), islocal=math%yes, scale=scale, &
    addContribution=math%yes, VALUE=forceVec(1:forceVec_i, 1:forceVec_j), &
    storageFMT=TypeFieldOpt%storageFormatDOF)

END DO

DEALLOCATE (cellcon, xij, forceVec, forceVecQuad)
NULLIFY (feptr, geofeptr, timefeptr, mesh)
CALL QuadraturePoint_Deallocate(quad)
CALL QuadraturePoint_Deallocate(timeQuad)
CALL ElemshapeData_Deallocate(elemsd)
CALL ElemshapeData_Deallocate(geoelemsd)
CALL ElemshapeData_Deallocate(timeelemsd)
CALL ElemshapeData_Deallocate(timegeoelemsd)
CALL FEVariable_Deallocate(forceVar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyBodySource1

!----------------------------------------------------------------------------
!                                                             ApplyBodySource
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyBodySource2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyBodySource2()"
LOGICAL(LGT) :: isok
CLASS(AbstractMesh_), POINTER :: mesh, bodyMesh
#endif

LOGICAL(LGT) :: isSameFEDOF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'STScalarField_::obj is not initiated')

isok = bodySource%IsInitiated()
CALL AssertError1(isok, myName, &
                  'STScalarField_::bodySource is not initiated')

mesh => obj%fedof%GetMeshPointer()
bodyMesh => bodySource%fedof%GetMeshPointer()
isok = ASSOCIATED(mesh, bodyMesh)
CALL AssertError1(isok, myName, &
                  'obj and bodySource should be defined on the same mesh')
NULLIFY (mesh, bodyMesh)
#endif

isSameFEDOF = ASSOCIATED(obj%fedof, bodySource%fedof) .AND. &
              ASSOCIATED(obj%timefedof, bodySource%timefedof)

IF (isSameFEDOF) THEN
  CALL ApplyBodySource2_SameFEDOF( &
    obj=obj, bodySource=bodySource, scale=scale, times=times)
ELSE
  CALL ApplyBodySource2_DiffFEDOF( &
    obj=obj, bodySource=bodySource, scale=scale, times=times)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyBodySource2

!----------------------------------------------------------------------------
!                                                             ApplyBodySource
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyBodySource3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyBodySource3()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: iel, tElements, maxNNE, maxNNEGeo, &
                tcellCon, tforceVec, xij_i, xij_j, maxNips, ips
REAL(DFP) :: args(4)
CHARACTER(*), PARAMETER :: opt_all = "A"

TYPE(QuadraturePoint_) :: quad
TYPE(ElemshapeData_) :: elemsd, geoelemsd
TYPE(FEVariable_) :: forceVar
CLASS(AbstractFE_), POINTER :: feptr, geofeptr
CLASS(AbstractMesh_), POINTER :: mesh

REAL(DFP), ALLOCATABLE :: xij(:, :), forceVec(:), forceVecQuad(:)
INTEGER(I4B), ALLOCATABLE :: cellcon(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = (timeCompo .GE. 1) .AND. (timeCompo .LE. obj%timeCompo)
CALL AssertError1(isok, myName, &
                  'timeCompo should be in [1, obj%timeCompo]')
#endif

mesh => obj%fedof%GetMeshPointer()

tElements = mesh%GetTotalElements()
maxNNEGeo = obj%geofedof%GetMaxTotalConnectivity()
maxNNE = obj%fedof%GetMaxTotalConnectivity()
maxNips = obj%fedof%GetMaxTotalQuadraturePoints()

CALL Reallocate(xij, 3, maxNNEGeo)
CALL Reallocate(cellcon, maxNNE)
CALL Reallocate(forceVec, maxNNE)
CALL Reallocate(forceVecQuad, maxNips)

forceVar = QuadratureVariable( &
           tsize=maxNips, RANK=TypeFEVariableScalar, &
           vartype=TypeFEVariableSpace)

args = 0.0_DFP
args(4) = times

DO iel = 1, tElements

  CALL obj%fedof%SetFE(globalElement=iel, islocal=math%yes)
  CALL obj%geofedof%SetFE(globalElement=iel, islocal=math%yes)

  feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=math%yes)
  geofeptr => obj%geofedof%GetFEPointer(globalElement=iel, islocal=math%yes)

  CALL obj%fedof%GetConnectivity_(globalElement=iel, islocal=math%yes, &
                                  ans=cellcon, tsize=tcellCon, opt=opt_all)

  CALL mesh%GetNodeCoord(nodeCoord=xij, nrow=xij_i, ncol=xij_j, &
                         islocal=math%yes, &
                         globalElement=iel)

  CALL feptr%GetGlobalElemShapeData2(geofeptr=geofeptr, elemsd=elemsd, &
                                     geoelemsd=geoelemsd, xij=xij, &
                                     quad=quad)

  DO ips = 1, elemsd%nips
    args(1:elemsd%nsd) = elemsd%coord(1:elemsd%nsd, ips)
    CALL bodySource%Get(val=forceVecQuad(ips), args=args)
  END DO

  CALL FEVariable_Set( &
    obj=forceVar, val=forceVecQuad(1:elemsd%nips), &
    RANK=TypeFEVariableScalar, varType=TypeFEVariableSpace, scale=math%one, &
    addContribution=math%no)

  CALL ForceVector_(test=elemsd, c=forceVar, crank=TypeFEVariableScalar, &
                    ans=forceVec, tsize=tforceVec)

  CALL obj%Set(VALUE=forceVec(1:tforceVec), globalNode=cellcon(1:tcellCon), &
               islocal=math%yes, timeCompo=timeCompo, scale=scale, &
               addContribution=math%yes)
END DO

IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(cellcon)) DEALLOCATE (cellcon)
IF (ALLOCATED(forceVec)) DEALLOCATE (forceVec)
IF (ALLOCATED(forceVecQuad)) DEALLOCATE (forceVecQuad)
NULLIFY (feptr, geofeptr, mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyBodySource3

!----------------------------------------------------------------------------
!                                                 ApplyBodySource2_SameFEDOF
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: ApplyBodySource2 when bodySource has the fedof and timefedof of obj
!
! On each element the nodal values of bodySource, f(J, b) for space DOF
! J and time component b, are read (Get, DOF format) and used as a
! nodal space-time FEVariable; STForceVector_ interpolates them at the
! quadrature points and integrates N_I T_a f over the element and the
! time slab given by times.

SUBROUTINE ApplyBodySource2_SameFEDOF(obj, bodySource, scale, times)
  CLASS(STScalarField_), INTENT(INOUT) :: obj
  !! space-time scalar field (right hand side)
  CLASS(STScalarField_), INTENT(IN) :: bodySource
  !! body source with the same fedof and timefedof as obj
  REAL(DFP), INTENT(IN) :: scale
  !! scale for body source
  REAL(DFP), INTENT(IN) :: times(:)
  !! times of the time slab

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ApplyBodySource2_SameFEDOF()"
#endif

  INTEGER(I4B) :: iel, tElements, maxNNS, maxNNT, maxNNSGeo, tcellCon, &
                  xij_i, xij_j, forceVec_i, forceVec_j, nodal_i, nodal_j
  INTEGER(I4B), ALLOCATABLE :: cellcon(:)
  REAL(DFP), ALLOCATABLE :: xij(:, :), forceVec(:, :), nodalVal(:, :)
  TYPE(QuadraturePoint_) :: quad, timeQuad
  TYPE(ElemshapeData_) :: elemsd, geoelemsd, timeelemsd, timegeoelemsd
  TYPE(FEVariable_) :: forceVar
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  CLASS(AbstractOneDimFE_), POINTER :: timefeptr
  CLASS(AbstractMesh_), POINTER :: mesh

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  mesh => obj%fedof%GetMeshPointer()
  tElements = mesh%GetTotalElements()

  CALL obj%timefedof%SetFE()
  timefeptr => obj%timefedof%GetFEPointer()

  CALL timefeptr%GetGlobalTimeElemShapeData( &
    elemsd=timeelemsd, geoelemsd=timegeoelemsd, quad=timeQuad, &
    times=times)

  maxNNSGeo = obj%geofedof%GetMaxTotalConnectivity()
  maxNNS = obj%fedof%GetMaxTotalConnectivity()
  maxNNT = obj%timefedof%GetMaxTotalConnectivity()

  CALL Reallocate(xij, 3, maxNNSGeo)
  CALL Reallocate(cellcon, maxNNS)
  CALL Reallocate(forceVec, maxNNS, maxNNT)
  CALL Reallocate(nodalVal, maxNNS, maxNNT)

  forceVar = NodalVariable( &
             nrow=maxNNS, ncol=maxNNT, RANK=TypeFEVariableScalar, &
             vartype=TypeFEVariableSpaceTime)

  DO iel = 1, tElements

    CALL obj%fedof%SetFE(globalElement=iel, islocal=math%yes)
    CALL obj%geofedof%SetFE(globalElement=iel, islocal=math%yes)

    feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=math%yes)
    geofeptr => obj%geofedof%GetFEPointer(globalElement=iel, &
                                          islocal=math%yes)

    CALL obj%fedof%GetConnectivity_( &
      globalElement=iel, islocal=math%yes, ans=cellcon, tsize=tcellCon, &
      opt="A")

    CALL mesh%GetNodeCoord( &
      nodeCoord=xij, nrow=xij_i, ncol=xij_j, islocal=math%yes, &
      globalElement=iel)

    CALL feptr%GetGlobalElemShapeData2( &
      geofeptr=geofeptr, elemsd=elemsd, geoelemsd=geoelemsd, xij=xij, &
      quad=quad)

    ! nodalVal(J, b): value of bodySource at space DOF cellcon(J) and
    ! time component b
    CALL bodySource%Get( &
      VALUE=nodalVal, nrow=nodal_i, ncol=nodal_j, &
      globalNode=cellcon(1:tcellCon), islocal=math%yes, &
      storageFMT=TypeFieldOpt%storageFormatDOF)

    CALL FEVariable_Set( &
      obj=forceVar, val=nodalVal(1:nodal_i, 1:nodal_j), &
      RANK=TypeFEVariableScalar, varType=TypeFEVariableSpaceTime, &
      scale=1.0_DFP, addContribution=math%no)

    CALL STForceVector_( &
      testSpace=elemsd, testTime=timeelemsd, c=forceVar, &
      crank=TypeFEVariableScalar, ans=forceVec, nrow=forceVec_i, &
      ncol=forceVec_j)

    CALL obj%Set( &
      globalNode=cellcon(1:tcellCon), islocal=math%yes, scale=scale, &
      addContribution=math%yes, &
      VALUE=forceVec(1:forceVec_i, 1:forceVec_j), &
      storageFMT=TypeFieldOpt%storageFormatDOF)

  END DO

  DEALLOCATE (cellcon, xij, forceVec, nodalVal)
  NULLIFY (feptr, geofeptr, timefeptr, mesh)
  CALL QuadraturePoint_Deallocate(quad)
  CALL QuadraturePoint_Deallocate(timeQuad)
  CALL ElemshapeData_Deallocate(elemsd)
  CALL ElemshapeData_Deallocate(geoelemsd)
  CALL ElemshapeData_Deallocate(timeelemsd)
  CALL ElemshapeData_Deallocate(timegeoelemsd)
  CALL FEVariable_Deallocate(forceVar)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ApplyBodySource2_SameFEDOF

!----------------------------------------------------------------------------
!                                                 ApplyBodySource2_DiffFEDOF
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: ApplyBodySource2 when bodySource has another fedof or timefedof
!
! Not implemented yet: raises an error. bodySource should be
! interpolated at the quadrature points of obj with its own shape
! functions (same mesh, element iel of obj is element iel of
! bodySource).

SUBROUTINE ApplyBodySource2_DiffFEDOF(obj, bodySource, scale, times)
  CLASS(STScalarField_), INTENT(INOUT) :: obj
  !! space-time scalar field (right hand side)
  CLASS(STScalarField_), INTENT(IN) :: bodySource
  !! body source with another fedof or timefedof
  REAL(DFP), INTENT(IN) :: scale
  !! scale for body source
  REAL(DFP), INTENT(IN) :: times(:)
  !! times of the time slab

  CHARACTER(*), PARAMETER :: myName = &
                             "STScalarField_Class@BodySourceMethods.F90::ApplyBodySource2_DiffFEDOF()"

  ! raised also in release mode, the result would be wrong otherwise
  CALL e%RaiseError(myName//' - '// &
                    '[WIP ERROR] :: bodySource with a fedof or timefedof '// &
                    'different from obj is not implemented yet')
END SUBROUTINE ApplyBodySource2_DiffFEDOF

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE BodySourceMethods
