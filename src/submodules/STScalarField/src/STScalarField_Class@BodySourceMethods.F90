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
! USE ReallocateUtility, ONLY: Reallocate
! USE ForceVector_Method, ONLY: ForceVector_
! USE FEVariable_Method, ONLY: NodalVariable, QuadratureVariable, &
!                              FEVariable_Set => Set
! USE AbstractFE_Class, ONLY: AbstractFE_
! USE AbstractMesh_Class, ONLY: AbstractMesh_
!
! USE BaseType, ONLY: QuadraturePoint_, &
!                     ElemshapeData_, &
!                     FEVariable_, &
!                     TypeFEVariableScalar, &
!                     TypeFEVariableSpace

CONTAINS

!----------------------------------------------------------------------------
!                                                             ApplyBodySource
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyBodySource1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyBodySource1()"
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

! LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.
!
! INTEGER(I4B) :: iel, tElements, maxNNE, maxNNEGeo, &
!                 tcellCon, tforceVec, xij_i, xij_j, maxNips, ips
! TYPE(QuadraturePoint_) :: quad
! TYPE(ElemshapeData_) :: elemsd, geoelemsd
! TYPE(FEVariable_) :: forceVar
! CLASS(AbstractFE_), POINTER :: feptr, geofeptr
! INTEGER(I4B), ALLOCATABLE :: cellcon(:)
! REAL(DFP), ALLOCATABLE :: xij(:, :), forceVec(:), forceVecQuad(:)
! REAL(DFP) :: args(4)
! CLASS(AbstractMesh_), POINTER :: mesh
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! mesh => obj%fedof%GetMeshPointer()
! tElements = mesh%GetTotalElements()
!
! maxNNEGeo = obj%geofedof%GetMaxTotalConnectivity()
! maxNNE = obj%fedof%GetMaxTotalConnectivity()
! maxNips = obj%fedof%GetMaxTotalQuadraturePoints()
!
! CALL Reallocate(xij, 3, maxNNEGeo)
! CALL Reallocate(cellcon, maxNNE)
! CALL Reallocate(forceVec, maxNNE)
! CALL Reallocate(forceVecQuad, maxNips)
!
! forceVar = QuadratureVariable(tsize=maxNips, rank=TypeFEVariableScalar, &
!                               vartype=TypeFEVariableSpace)
!
! args = 0.0_DFP
!
! DO iel = 1, tElements
!
!   CALL obj%fedof%SetFE(globalElement=iel, islocal=yes)
!   CALL obj%geofedof%SetFE(globalElement=iel, islocal=yes)
!
!   feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=yes)
!   geofeptr => obj%geofedof%GetFEPointer(globalElement=iel, islocal=yes)
!
!   CALL obj%fedof%GetConnectivity_(globalElement=iel, islocal=yes, &
!                                   ans=cellcon, tsize=tcellCon, opt="A")
!
!   CALL mesh%GetNodeCoord( &
!     nodeCoord=xij, nrow=xij_i, ncol=xij_j, islocal=yes, &
!     globalElement=iel)
!
!   CALL feptr%GetGlobalElemShapeData2( &
!     geofeptr=geofeptr, elemsd=elemsd, geoelemsd=geoelemsd, xij=xij, &
!     quad=quad)
!
!   DO ips = 1, elemsd%nips
!     args(1:elemsd%nsd) = elemsd%coord(1:elemsd%nsd, ips)
!     CALL bodySource%GetScalarValue(val=forceVecQuad(ips), args=args)
!   END DO
!
!   CALL FEVariable_Set( &
!     obj=forceVar, val=forceVecQuad(1:elemsd%nips), &
!     rank=TypeFEVariableScalar, varType=TypeFEVariableSpace, scale=1.0_DFP, &
!     addContribution=no)
!
!   CALL ForceVector_(test=elemsd, c=forceVar, crank=TypeFEVariableScalar, &
!                     ans=forceVec, tsize=tforceVec)
!
!   CALL obj%Set( &
!     globalNode=cellcon(1:tcellCon), islocal=yes, scale=scale, &
!     addContribution=yes, VALUE=forceVec(1:tforceVec))
! END DO
!
! IF (ALLOCATED(xij)) DEALLOCATE (xij)
! IF (ALLOCATED(cellcon)) DEALLOCATE (cellcon)
! IF (ALLOCATED(forceVec)) DEALLOCATE (forceVec)
! IF (ALLOCATED(forceVecQuad)) DEALLOCATE (forceVecQuad)
! NULLIFY (feptr, geofeptr, mesh)

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
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

! LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.
! TYPE(QuadraturePoint_) :: quad
! TYPE(ElemshapeData_) :: elemsd, geoelemsd
! TYPE(FEVariable_) :: forceVar
! INTEGER(I4B) :: iel, tElements, maxNNE, maxNNEGeo, &
!                 tcellCon, tforceVec, xij_i, xij_j, &
!                 tfevec
! INTEGER(I4B), ALLOCATABLE :: cellcon(:)
! REAL(DFP), ALLOCATABLE :: xij(:, :), fevec(:), forceVec(:)
! CLASS(AbstractFE_), POINTER :: feptr, geofeptr
! CLASS(AbstractMesh_), POINTER :: mesh
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! mesh => obj%fedof%GetMeshPointer()
! tElements = mesh%GetTotalElements()
! maxNNEGeo = obj%geofedof%GetMaxTotalConnectivity()
! maxNNE = obj%fedof%GetMaxTotalConnectivity()
!
! CALL Reallocate(xij, 3, maxNNEGeo)
! CALL Reallocate(cellcon, maxNNE)
! CALL Reallocate(fevec, maxNNE)
! CALL Reallocate(forceVec, maxNNE)
!
! forceVar = NodalVariable(tsize=maxNNE, rank=TypeFEVariableScalar, &
!                          vartype=TypeFEVariableSpace)
! ! forceVar is nodal value at element level obtained from the bodySource
! ! field
! ! TODO:
! ! We should get the quadrature values of bodySource in forceVar.
! ! This means we should add a method to scalarfield for getting the
! ! quadrature values of a field. This means, the method will first create
! ! local element shape data, and then interpolate the data using nodal
! ! values and the shape data. This interpolated value should be returned.
! ! In this way, we can use any order of interpolation for the bodySource.
!
! DO iel = 1, tElements
!
!   CALL obj%fedof%SetFE(globalElement=iel, islocal=yes)
!   CALL obj%geofedof%SetFE(globalElement=iel, islocal=yes)
!
!   feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=yes)
!   geofeptr => obj%geofedof%GetFEPointer(globalElement=iel, islocal=yes)
!
!   CALL obj%fedof%GetConnectivity_(globalElement=iel, islocal=yes, &
!                                   ans=cellcon, tsize=tcellCon, opt="A")
!
!   CALL mesh%GetNodeCoord(nodeCoord=xij, nrow=xij_i, ncol=xij_j, &
!                          islocal=yes, globalElement=iel)
!
!   CALL feptr%GetGlobalElemShapeData2( &
!     geofeptr=geofeptr, elemsd=elemsd, geoelemsd=geoelemsd, xij=xij, &
!     quad=quad)
!
!   ! Read the above TODO comment
!   CALL bodySource%Get(VALUE=forceVec, globalNode=cellcon(1:tcellCon), &
!                       tsize=tforceVec, islocal=yes)
!
!   CALL FEVariable_Set( &
!     obj=forceVar, val=forceVec(1:tforceVec), rank=TypeFEVariableScalar, &
!     vartype=TypeFEVariableSpace, scale=1.0_DFP, addContribution=no)
!
!   CALL ForceVector_( &
!     test=elemsd, c=forceVar, crank=TypeFEVariableScalar, ans=fevec, &
!     tsize=tfevec)
!
!   CALL obj%Set(globalNode=cellcon(1:tcellCon), islocal=yes, &
!                scale=scale, addContribution=yes, &
!                VALUE=fevec(1:tfevec))
!
! END DO
!
! IF (ALLOCATED(cellcon)) DEALLOCATE (cellcon)
! IF (ALLOCATED(xij)) DEALLOCATE (xij)
! IF (ALLOCATED(forceVec)) DEALLOCATE (forceVec)
! IF (ALLOCATED(fevec)) DEALLOCATE (fevec)
! NULLIFY (feptr, geofeptr, mesh)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyBodySource2

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE BodySourceMethods
