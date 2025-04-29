! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(ElastoDynamics2DFEM_Class) UpdateMethods
USE GlobalData, ONLY: NODES_FMT
USE SymUtility
USE RealMatrix_Method, ONLY: RealMatrix_sym => SYM

USE FEVariable_Method, ONLY: QuadratureVariable, &
                             OPERATOR(*), &
                             fevar_Display => Display, &
                             Get_
USE BaseType, ONLY: TypeFEVariableOpt, &
                    TypeFEVariableSpace, &
                    TypeFEVariableVector, &
                    TypeFEVariableMatrix, &
                    TypeFEVariableConstant

USE ElemshapeData_Method

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Update
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Update
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Update()"
#endif

REAL(DFP) :: scale, dt, dts
LOGICAL(LGT) :: isok

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

dt = obj%timeStepSize(obj%currentTimeStep)
dts = dt * dt
obj%currentTime = obj%currentTime + dt
obj%currentTimeStep = obj%currentTimeStep + 1

CALL obj%force1%COPY(obj%force2)

!----------------------------------------------------------------------------
! Update displacement
!----------------------------------------------------------------------------

CALL obj%rhs%Set(VALUE=zero)

scale = obj%algoParam%dis(1)
isok = .NOT. obj%algoParam%dis_zero(1)
IF (isok) &
  CALL obj%rhs%AXPY(x=obj%u0, scale=scale)

scale = obj%algoParam%dis(2) * dt
isok = .NOT. obj%algoParam%dis_zero(2)
IF (isok) &
  CALL obj%rhs%AXPY(x=obj%v0, scale=scale)

scale = obj%algoParam%dis(3) * dts
isok = .NOT. obj%algoParam%dis_zero(3)
IF (isok) &
  CALL obj%rhs%AXPY(x=obj%a0, scale=scale)

scale = obj%algoParam%dis(4)
isok = .NOT. obj%algoParam%dis_zero(4)
IF (isok) &
  CALL obj%rhs%AXPY(x=obj%sol, scale=scale)

!----------------------------------------------------------------------------
! Update v0
!----------------------------------------------------------------------------

CALL obj%tmp1%Set(VALUE=zero)

scale = obj%algoParam%vel(1) / dt
isok = .NOT. obj%algoParam%vel_zero(1)
IF (isok) &
  CALL obj%tmp1%AXPY(x=obj%u0, scale=scale)

scale = obj%algoParam%vel(2)
isok = .NOT. obj%algoParam%vel_zero(2)
IF (isok) &
  CALL obj%tmp1%AXPY(x=obj%v0, scale=scale)

scale = obj%algoParam%vel(3) * dt
isok = .NOT. obj%algoParam%vel_zero(3)
IF (isok) &
  CALL obj%tmp1%AXPY(x=obj%a0, scale=scale)

scale = obj%algoParam%vel(4) / dt
isok = .NOT. obj%algoParam%vel_zero(4)
IF (isok) &
  CALL obj%tmp1%AXPY(x=obj%sol, scale=scale)

!----------------------------------------------------------------------------
! Update a0
!----------------------------------------------------------------------------

CALL obj%force2%Set(VALUE=zero)

scale = obj%algoParam%acc(1) / dts
isok = .NOT. obj%algoParam%acc_zero(1)
IF (isok) &
  CALL obj%force2%AXPY(x=obj%u0, scale=scale)

scale = obj%algoParam%acc(2) / dt
isok = .NOT. obj%algoParam%acc_zero(2)
IF (isok) &
  CALL obj%force2%AXPY(x=obj%v0, scale=scale)

scale = obj%algoParam%acc(3)
isok = .NOT. obj%algoParam%acc_zero(3)
IF (isok) &
  CALL obj%force2%AXPY(x=obj%a0, scale=scale)

scale = obj%algoParam%acc(4) / dts
isok = .NOT. obj%algoParam%acc_zero(4)
IF (isok) &
  CALL obj%force2%AXPY(x=obj%sol, scale=scale)

!----------------------------------------------------------------------------
! Update u0, v0, a0, and force1
!----------------------------------------------------------------------------

CALL obj%u0%Copy(obj%rhs)
CALL obj%v0%Copy(obj%tmp1)
CALL obj%a0%Copy(obj%force2)
CALL obj%force2%Set(VALUE=zero)
CALL obj%rhs%Set(VALUE=zero)

CALL obj%sol%Set(VALUE=zero)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_Update

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_UpdateStressStrain
CHARACTER(*), PARAMETER :: myName = "obj_UpdateStressStrain()"
INTEGER(I4B) :: iel, nrow, ncol, tsize, ii, order
REAL(DFP) :: xij(3, obj%maxNNE), uiJ(2, obj%maxNNE), &
             strain_val(3, obj%maxNIP), coeff(3, 3)
! NOTE: later it will be replaced with static array
REAL(DFP), ALLOCATABLE :: lgval(:, :, :)
TYPE(FEVariable_) :: strain, stress

IF (obj%saveStressAtCenter) THEN
  order = 1
ELSE
  order = 2 * obj%spaceOrder
END IF

DO iel = 1, obj%totalSpaceElements

  CALL Get_(obj=obj%cijkl, rank=TypeFEVariableMatrix, &
            vartype=TypeFEVariableConstant, &
            val=coeff, nrow=nrow, ncol=ncol)

  CALL obj%fedof%GetQuadraturePoints1(quad=obj%quadForStress, &
                                      globalElement=iel, &
                                      quadratureType=obj%quadTypeForSpace, &
                                      order=order, &
                                      islocal=.TRUE.)

  CALL obj%fedof%GetLocalElemShapeData(globalElement=iel, &
                                       elemsd=obj%elemsdForSpace, &
                                       quad=obj%quadForStress, &
                                       islocal=.TRUE.)

  CALL obj%cellmesh%GetNodeCoord(nodecoord=xij, nrow=nrow, ncol=ncol, &
                                 globalElement=iel, islocal=.TRUE.)

  CALL obj%fedof%GetGlobalElemShapeData(globalElement=iel, &
                                        elemsd=obj%elemsdForSpace, &
                                        xij=xij, islocal=.TRUE.)

  CALL obj%fedof%GetConnectivity_(globalElement=iel, islocal=.TRUE., &
                                  ans=obj%cellcon, tsize=tsize, opt="A")

  CALL obj%u0%Get(VALUE=uiJ, nrow=nrow, ncol=ncol, storageFMT=NODES_FMT, &
                  globalNode=obj%cellcon(1:tsize), islocal=.TRUE.)

  ! CALL obj%u0%Get(VALUE=u_fevar, globalNode=obj%cellcon(1:tsize), &
  !                 islocal=.TRUE.)

  CALL GetSpatialGradient(obj%elemsdForSpace, lg=lgval, &
                          val=uiJ(1:nrow, 1:ncol))
  tsize = SIZE(lgval, 3)

  DO ii = 1, tsize
    ! strain at integration point
    lgval(:, :, ii) = half * RealMatrix_sym(lgval(:, :, ii))
    strain_val(1, ii) = lgval(1, 1, ii)
    strain_val(2, ii) = lgval(2, 2, ii)
    strain_val(3, ii) = lgval(1, 2, ii) + lgval(2, 1, ii)

  END DO

  strain = QuadratureVariable(val=strain_val(:, 1:tsize), rank=TypeFEVariableVector, &
                              vartype=TypeFEVariableSpace)

  stress = QuadratureVariable(val=MATMUL(coeff, strain_val(:, 1:tsize)), &
                              rank=TypeFEVariableVector, &
                              vartype=TypeFEVariableSpace)

  CALL obj%strain%Set(iel, fevar=strain, islocal=.TRUE.)
  CALL obj%stress%Set(iel, fevar=stress, islocal=.TRUE.)

END DO

END PROCEDURE obj_UpdateStressStrain

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE UpdateMethods
