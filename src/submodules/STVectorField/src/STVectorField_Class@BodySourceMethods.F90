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

SUBMODULE(STVectorField_Class) BodySourceMethods
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE BaseType, ONLY: ElemshapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: TypeFEVariableScalar, TypeFEVariableVector
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
USE SwapUtility, ONLY: Swap_

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Display => Display
USE ElemshapeData_Method, ONLY: ElemshapeData_Display => Display
USE FEVariable_Method, ONLY: Display
#endif

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "STVectorField_Class@BodySourceMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyBodySource1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyBodySource1()"
#endif

INTEGER(I4B) :: iel, tElements, maxNNS, maxNNT, maxNNSGeo, tcellCon, xij_i, &
                xij_j, maxNips, ips, maxNipt, ipt, &
                forceVec_i, forceVec_j, forceVec_k, &
                spaceCompo(1), tsize
INTEGER(I4B), ALLOCATABLE :: cellcon(:)
REAL(DFP) :: args(4)
REAL(DFP), ALLOCATABLE :: xij(:, :), forceVec(:, :, :), &
                          forceVecQuad(:, :, :), &
                          forceVec_swap(:, :, :)
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
spaceCompo = obj%GetspaceCompo(1)

CALL Reallocate(xij, 3, maxNNSGeo)
CALL Reallocate(cellcon, maxNNS)
CALL Reallocate(forceVec, spaceCompo(1), maxNNS, maxNNT)
CALL Reallocate(forceVec_swap, spaceCompo(1), maxNNT, maxNNS)
CALL Reallocate(forceVecQuad, spaceCompo(1), maxNips, maxNipt)

forceVar = QuadratureVariable( &
           dim1=spaceCompo(1), dim2=maxNips, dim3=maxNipt, &
           rank=TypeFEVariableVector, &
           vartype=TypeFEVariableSpaceTime)

args = math%zero

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
      CALL bodySource%Get_( &
        val=forceVecQuad(:, ips, ipt), args=args, tsize=tsize)
    END DO
  END DO

  CALL FEVariable_Set( &
    obj=forceVar, &
    val=forceVecQuad(1:spaceCompo(1), 1:elemsd%nips, 1:timeelemsd%nips), &
    rank=TypeFEVariableVector, varType=TypeFEVariableSpaceTime, &
    scale=math%one, addContribution=math%no)

  CALL STForceVector_( &
    testSpace=elemsd, testTime=timeelemsd, c=forceVar, &
    crank=TypeFEVariableVector, ans=forceVec, &
    dim1=forceVec_i, dim2=forceVec_j, dim3=forceVec_k)

  CALL Swap_(a=forceVec_swap, b=forceVec, i1=1, i2=3, i3=2)

  CALL obj%Set(globalNode=cellcon(1:tcellCon), islocal=math%yes, &
               scale=scale, addContribution=math%yes, &
               VALUE=forceVec_swap(1:forceVec_i, 1:forceVec_k, 1:forceVec_j))

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
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyBodySource2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyBodySource2()"
LOGICAL(LGT) :: isok
CLASS(AbstractMesh_), POINTER :: bodyMesh
#endif

LOGICAL(LGT) :: isSameFEDOF
INTEGER(I4B) :: iel, tElements, maxNNS, maxNNT, maxNNSGeo, maxNips, &
                maxNipt, maxNNSB, maxNNTB, tcellCon, tcellConB, xij_i, &
                xij_j, forceVec_i, forceVec_j, forceVec_k, ntimeB, &
                spaceCompo(1), ii, bb, tsize
INTEGER(I4B), ALLOCATABLE :: cellcon(:), cellconB(:)
REAL(DFP), ALLOCATABLE :: xij(:, :), forceVec(:, :, :), &
                          forceVec_swap(:, :, :), forceVecQuad(:, :, :), &
                          nodalVal(:, :, :)
TYPE(QuadraturePoint_) :: quad, timeQuad
TYPE(ElemshapeData_) :: elemsd, geoelemsd, timeelemsd, timegeoelemsd, &
                        elemsdB, geoelemsdB, timeelemsdB, timegeoelemsdB
TYPE(FEVariable_) :: forceVar
CLASS(AbstractFE_), POINTER :: feptr, geofeptr, feptrB
CLASS(AbstractOneDimFE_), POINTER :: timefeptr, timefeptrB
CLASS(AbstractMesh_), POINTER :: mesh

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

mesh => obj%fedof%GetMeshPointer()

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, 'STVectorField_::obj is not initiated')

isok = bodySource%IsInitiated()
CALL AssertError1(isok, myName, &
                  'STVectorField_::bodySource is not initiated')

bodyMesh => bodySource%fedof%GetMeshPointer()
isok = ASSOCIATED(mesh, bodyMesh)
CALL AssertError1(isok, myName, &
                  'obj and bodySource should be defined on the same mesh')
NULLIFY (bodyMesh)

CALL AssertError2(bodySource%spaceCompo, obj%spaceCompo, myName, &
                  'a=bodySource%spaceCompo, b=obj%spaceCompo')
#endif

isSameFEDOF = ASSOCIATED(obj%fedof, bodySource%fedof) .AND. &
              ASSOCIATED(obj%timefedof, bodySource%timefedof)

tElements = mesh%GetTotalElements()
spaceCompo = obj%GetSpaceCompo(1)
ntimeB = bodySource%timeCompo

! time shape data of obj on the time slab; for another timefedof, the
! time shape functions of bodySource at the time quadrature points of
! obj
CALL obj%timefedof%SetFE()
timefeptr => obj%timefedof%GetFEPointer()
CALL timefeptr%GetGlobalTimeElemShapeData( &
  elemsd=timeelemsd, geoelemsd=timegeoelemsd, quad=timeQuad, times=times)

timefeptrB => NULL()
IF (.NOT. isSameFEDOF) THEN
  CALL bodySource%timefedof%SetFE()
  timefeptrB => bodySource%timefedof%GetFEPointer()
  CALL timefeptrB%GetGlobalTimeElemShapeData( &
    elemsd=timeelemsdB, geoelemsd=timegeoelemsdB, quad=timeQuad, &
    times=times, doNotInitQuad=math%yes)
END IF

maxNNSGeo = obj%geofedof%GetMaxTotalConnectivity()
maxNNS = obj%fedof%GetMaxTotalConnectivity()
maxNNT = obj%timefedof%GetMaxTotalConnectivity()
maxNips = obj%fedof%GetMaxTotalQuadraturePoints()
maxNipt = obj%timefedof%GetMaxTotalQuadraturePoints()
maxNNSB = bodySource%fedof%GetMaxTotalConnectivity()
maxNNTB = bodySource%timefedof%GetMaxTotalConnectivity()

CALL Reallocate(xij, 3, maxNNSGeo)
CALL Reallocate(cellcon, maxNNS)
CALL Reallocate(cellconB, maxNNSB)
CALL Reallocate(forceVec, spaceCompo(1), maxNNS, maxNNT)
CALL Reallocate(forceVec_swap, spaceCompo(1), maxNNT, maxNNS)
CALL Reallocate(forceVecQuad, spaceCompo(1), maxNips, maxNipt)
CALL Reallocate(nodalVal, spaceCompo(1), maxNNSB, maxNNTB)

forceVar = QuadratureVariable( &
           dim1=spaceCompo(1), dim2=maxNips, dim3=maxNipt, &
           rank=TypeFEVariableVector, &
           vartype=TypeFEVariableSpaceTime)

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

  IF (isSameFEDOF) THEN
    cellconB(1:tcellCon) = cellcon(1:tcellCon)
    tcellConB = tcellCon
  ELSE
    ! shape functions of bodySource at the quadrature points of obj
    CALL bodySource%fedof%SetFE(globalElement=iel, islocal=math%yes)
    feptrB => bodySource%fedof%GetFEPointer(globalElement=iel, &
                                            islocal=math%yes)
    CALL bodySource%fedof%GetConnectivity_( &
      globalElement=iel, islocal=math%yes, ans=cellconB, &
      tsize=tcellConB, opt="A")
    CALL feptrB%GetGlobalElemShapeData2( &
      geofeptr=geofeptr, elemsd=elemsdB, geoelemsd=geoelemsdB, xij=xij, &
      quad=quad, doNotInitQuad=math%yes)
  END IF

  ! nodalVal(c, J, b): space component c, space DOF cellconB(J) and
  ! time component b of bodySource
  DO bb = 1, ntimeB
    DO ii = 1, spaceCompo(1)
      CALL bodySource%Get( &
        VALUE=nodalVal(ii, :, bb), tsize=tsize, &
        globalNode=cellconB(1:tcellConB), islocal=math%yes, &
        spaceCompo=ii, timeCompo=bb)
    END DO
  END DO

  ! f(c, ips, ipt) = sum_J sum_b N^B_J(ips) f(c, J, b) T^B_b(ipt)
  IF (isSameFEDOF) THEN
    CALL InterpolateNodalVal( &
      nodalVal=nodalVal(:, 1:tcellConB, 1:ntimeB), &
      spaceN=elemsd%N(1:tcellConB, 1:elemsd%nips), &
      timeN=timeelemsd%N(1:ntimeB, 1:timeelemsd%nips), &
      ans=forceVecQuad(:, 1:elemsd%nips, 1:timeelemsd%nips))
  ELSE
    CALL InterpolateNodalVal( &
      nodalVal=nodalVal(:, 1:tcellConB, 1:ntimeB), &
      spaceN=elemsdB%N(1:tcellConB, 1:elemsd%nips), &
      timeN=timeelemsdB%N(1:ntimeB, 1:timeelemsd%nips), &
      ans=forceVecQuad(:, 1:elemsd%nips, 1:timeelemsd%nips))
  END IF

  CALL FEVariable_Set( &
    obj=forceVar, &
    val=forceVecQuad(1:spaceCompo(1), 1:elemsd%nips, 1:timeelemsd%nips), &
    rank=TypeFEVariableVector, varType=TypeFEVariableSpaceTime, &
    scale=math%one, addContribution=math%no)

  CALL STForceVector_( &
    testSpace=elemsd, testTime=timeelemsd, c=forceVar, &
    crank=TypeFEVariableVector, ans=forceVec, &
    dim1=forceVec_i, dim2=forceVec_j, dim3=forceVec_k)

  ! forceVec(c, I, a) -> forceVec_swap(c, a, I)
  CALL Swap_(a=forceVec_swap, b=forceVec, i1=1, i2=3, i3=2)

  CALL obj%Set(globalNode=cellcon(1:tcellCon), islocal=math%yes, &
               scale=scale, addContribution=math%yes, &
               VALUE=forceVec_swap(1:forceVec_i, 1:forceVec_k, &
                                   1:forceVec_j))

END DO

DEALLOCATE (cellcon, cellconB, xij, forceVec, forceVec_swap, &
            forceVecQuad, nodalVal)
NULLIFY (feptr, geofeptr, feptrB, timefeptr, timefeptrB, mesh)
CALL QuadraturePoint_Deallocate(quad)
CALL QuadraturePoint_Deallocate(timeQuad)
CALL ElemshapeData_Deallocate(elemsd)
CALL ElemshapeData_Deallocate(geoelemsd)
CALL ElemshapeData_Deallocate(timeelemsd)
CALL ElemshapeData_Deallocate(timegeoelemsd)
CALL ElemshapeData_Deallocate(elemsdB)
CALL ElemshapeData_Deallocate(geoelemsdB)
CALL ElemshapeData_Deallocate(timeelemsdB)
CALL ElemshapeData_Deallocate(timegeoelemsdB)
CALL FEVariable_Deallocate(forceVar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyBodySource2

!----------------------------------------------------------------------------
!                                                        InterpolateNodalVal
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Values of a space-time vector at the quadrature points
!
!   ans(c, ips, ipt) = sum_J sum_b spaceN(J, ips) nodalVal(c, J, b)
!                      timeN(b, ipt)

PURE SUBROUTINE InterpolateNodalVal(nodalVal, spaceN, timeN, ans)
  REAL(DFP), INTENT(IN) :: nodalVal(:, :, :)
  !! (spaceCompo, space DOF J, time DOF b)
  REAL(DFP), INTENT(IN) :: spaceN(:, :)
  !! space shape functions (space DOF J, space quadrature point)
  REAL(DFP), INTENT(IN) :: timeN(:, :)
  !! time shape functions (time DOF b, time quadrature point)
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  !! (spaceCompo, space quadrature point, time quadrature point)

  INTEGER(I4B) :: ips, ipt, jj, bb

  DO ipt = 1, SIZE(timeN, 2)
    DO ips = 1, SIZE(spaceN, 2)
      ans(:, ips, ipt) = 0.0_DFP
      DO bb = 1, SIZE(timeN, 1)
        DO jj = 1, SIZE(spaceN, 1)
          ans(:, ips, ipt) = ans(:, ips, ipt) + nodalVal(:, jj, bb) &
                             * (spaceN(jj, ips) * timeN(bb, ipt))
        END DO
      END DO
    END DO
  END DO
END SUBROUTINE InterpolateNodalVal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyBodySource3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyBodySource3()"
#endif

INTEGER(I4B) :: iel, tElements, maxNNE, maxNNEGeo, &
                tcellCon, tforceVec, xij_i, xij_j, maxNips, ips, &
                spaceCompo(1), force_i, force_j, tsize, aa, a1, a2, ii
REAL(DFP) :: args(4)

TYPE(QuadraturePoint_) :: quad
TYPE(ElemshapeData_) :: elemsd, geoelemsd
TYPE(FEVariable_) :: forceVar
CLASS(AbstractFE_), POINTER :: feptr, geofeptr
CLASS(AbstractMesh_), POINTER :: mesh

REAL(DFP), ALLOCATABLE :: xij(:, :), forceVec(:, :), forceVecQuad(:, :)
INTEGER(I4B), ALLOCATABLE :: cellcon(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

mesh => obj%fedof%GetMeshPointer()

tElements = mesh%GetTotalElements()
maxNNEGeo = obj%geofedof%GetMaxTotalConnectivity()
maxNNE = obj%fedof%GetMaxTotalConnectivity()
maxNips = obj%fedof%GetMaxTotalQuadraturePoints()
spaceCompo = obj%GetspaceCompo(1)

a1 = 1
a2 = obj%timeCompo
IF (PRESENT(timeCompo)) THEN
  a1 = timeCompo
  a2 = timeCompo
END IF

#ifdef DEBUG_VER
CALL AssertError1(a1 .GE. 1 .AND. a2 .LE. obj%timeCompo, myName, &
                  "timeCompo out of bound")
#endif

CALL Reallocate(xij, 3, maxNNEGeo)
CALL Reallocate(cellcon, maxNNE)
CALL Reallocate(forceVec, spaceCompo(1), maxNNE)
CALL Reallocate(forceVecQuad, spaceCompo(1), maxNips)

forceVar = QuadratureVariable(nrow=spaceCompo(1), &
                              ncol=maxNips, rank=TypeFEVariableVector, &
                              vartype=TypeFEVariableSpace)

args = 0.0_DFP
args(4) = times

DO iel = 1, tElements

  CALL obj%fedof%SetFE(globalElement=iel, islocal=math%yes)
  CALL obj%geofedof%SetFE(globalElement=iel, islocal=math%yes)

  feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=math%yes)
  geofeptr => obj%geofedof%GetFEPointer(globalElement=iel, islocal=math%yes)

  CALL obj%fedof%GetConnectivity_(globalElement=iel, islocal=math%yes, &
                                  ans=cellcon, tsize=tcellCon, opt="A")

  CALL mesh%GetNodeCoord( &
    nodeCoord=xij, nrow=xij_i, ncol=xij_j, islocal=math%yes, &
    globalElement=iel)

  CALL feptr%GetGlobalElemShapeData2( &
    geofeptr=geofeptr, elemsd=elemsd, geoelemsd=geoelemsd, xij=xij, &
    quad=quad)

  DO ips = 1, elemsd%nips
    args(1:elemsd%nsd) = elemsd%coord(1:elemsd%nsd, ips)
    CALL bodySource%Get_( &
      val=forceVecQuad(:, ips), args=args, tsize=tsize)
  END DO

  CALL FEVariable_Set( &
    obj=forceVar, val=forceVecQuad(1:spaceCompo(1), 1:elemsd%nips), &
    rank=TypeFEVariableVector, varType=TypeFEVariableSpace, &
    scale=math%one, addContribution=math%no)

  CALL ForceVector_(test=elemsd, c=forceVar, crank=TypeFEVariableVector, &
                    ans=forceVec, nrow=force_i, ncol=force_j)

  ! forceVec is (spaceCompo, nns), add row ii to the space component ii
  ! of the time components a1 to a2
  DO aa = a1, a2
    DO ii = 1, force_i
      CALL obj%Set(VALUE=forceVec(ii, 1:force_j), &
                   globalNode=cellcon(1:tcellCon), islocal=math%yes, &
                   spaceCompo=ii, timeCompo=aa, scale=scale, &
                   addContribution=math%yes)
    END DO
  END DO

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
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE BodySourceMethods
