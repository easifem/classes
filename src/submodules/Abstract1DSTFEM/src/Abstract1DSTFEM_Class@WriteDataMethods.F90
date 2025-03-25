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
!

SUBMODULE(Abstract1DSTFEM_Class) WriteDataMethods
USE Lapack_Method, ONLY: GetInvMat, SymLinSolve

USE TomlUtility, ONLY: GetValue, GetValue_

USE StringUtility, ONLY: UpperCase

USE GlobalData, ONLY: stdout, &
                      CHAR_LF, &
                      DOF_FMT, &
                      NONE, &
                      LIS_GMRES, &
                      CHAR_SLASH

USE BaseInterpolation_Method, ONLY: BaseInterpolation_ToInteger, &
                                    BaseType_ToInteger, &
                                    BaseType_ToChar, &
                                    BaseInterpolation_ToChar

USE LineInterpolationUtility, ONLY: OrthogonalBasis_Line_

USE ReallocateUtility, ONLY: Reallocate

USE ProductUtility, ONLY: OuterProd_, OTimesTilda

USE BaseType, ONLY: elem => TypeElemNameOpt

USE QuadraturePoint_Method, ONLY: QuadPoint_Initiate => Initiate, &
                                  Quad_Size => Size, &
                                  Quad_Display => Display

USE ElemshapeData_Method, ONLY: LagrangeElemShapeData, &
                                Elemsd_Allocate => ALLOCATE, &
                                HierarchicalElemShapeData, &
                                Elemsd_Set => Set, &
                                OrthogonalElemShapeData

USE SwapUtility, ONLY: SWAP

USE CSRMatrix_Method, ONLY: CSRMatrix_Initiate => Initiate, &
                            CSRMatrix_Add => Add, &
                            CSRMatrix_GetSubMatrix => GetSubMatrix, &
                            CSRMatrix_Display => Display, &
                            CSRMatrix_Size => Size, &
                            CSRMatrix_SetSparsity => SetSparsity, &
                            CSRMatrix_ApplyDBC => ApplyDBC, &
                            CSRMatrix_Set => Set, &
                            CSRMatrix_Matvec => Matvec, &
                            CSRMatrix_LinSolve => CSRMatrix_GMRES, &
                            CSRMatrixLinSolveInitiate

USE DOF_Method, ONLY: DOF_Initiate => Initiate, &
                      DOF_SIZE => Size, &
                      DOF_GetIndex_ => GetIndex_, &
                      DOF_GetNodeLoc => GetNodeLoc

USE RealVector_Method, ONLY: RealVector_Initiate => Initiate, &
                             RealVector_Add => Add, &
                             RealVector_GetValue_ => GetValue_, &
                             RealVector_Set => Set, &
                             RealVector_Display => Display, &
                             RealVector_Scale => SCAL

USE LagrangePolynomialUtility, ONLY: InterpolationPoint_
USE InputUtility

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

CONTAINS

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteData()"
#endif

REAL(DFP) :: t, dx, xij(1, 2), u0(MAX_ORDER_SPACE + 1), &
             v0(MAX_ORDER_SPACE + 1), &
             a0(MAX_ORDER_SPACE + 1), xlim(2), ylim(2), &
             args(2)

REAL(DFP), ALLOCATABLE :: DATA(:, :), ips(:, :), refVal(:)

INTEGER(I4B) :: ielSpace, con(MAX_ORDER_SPACE + 1), nns, &
                totalNodes, ii, jj, n, inds(2)

CHARACTER(:), ALLOCATABLE :: filename_disp, filename_vel, filename_acc, &
                             aline, filename_data
LOGICAL(LGT) :: abool1, abool2, abool3

abool1 = ANY(obj%saveData)
abool2 = ANY(obj%plotData)

IF (.NOT. abool1 .AND. .NOT. abool2) RETURN

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

t = obj%currentTime
xij(1, 1) = obj%spaceDomain(1)

filename_disp = obj%result_dir//CHAR_SLASH//obj%filename//'_disp_'// &
                tostring(obj%currentTimeStep - 1_I4B)

filename_vel = obj%result_dir//CHAR_SLASH//obj%filename//'_vel_'// &
               tostring(obj%currentTimeStep - 1_I4B)

filename_acc = obj%result_dir//CHAR_SLASH//obj%filename//'_acc_'// &
               tostring(obj%currentTimeStep - 1_I4B)

filename_data = obj%result_dir//CHAR_SLASH//obj%filename//'_data_'// &
                tostring(obj%currentTimeStep - 1_I4B)

SELECT CASE (obj%baseInterpolationForSpace)
CASE ("LAGR")
  totalNodes = obj%totalVertexDOFSpace + obj%totalEdgeDOFSpace
  ALLOCATE (ips(1, obj%maxSpaceOrder + 1))
CASE DEFAULT
  totalNodes = obj%totalVertexDOFSpace
END SELECT

ALLOCATE (DATA(totalNodes, 4))

abool1 = obj%saveData(1) .OR. obj%plotData(1) .OR. obj%saveData(4)
abool2 = obj%saveData(2) .OR. obj%plotData(2) .OR. obj%saveData(4)
abool3 = obj%saveData(3) .OR. obj%plotData(3) .OR. obj%saveData(4)
n = 1

DO ielSpace = 1, obj%totalSpaceElements

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=nns)

  dx = obj%spaceElemLength(ielSpace)

  xij(1, 2) = xij(1, 1) + dx

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)

  IF (abool1) &
    CALL RealVector_GetValue_(obj=obj%u0, nodenum=con(1:nns), VALUE=u0, &
                              tsize=nns)

  IF (abool2) &
    CALL RealVector_GetValue_(obj=obj%v0, nodenum=con(1:nns), VALUE=v0, &
                              tsize=nns)

  IF (abool3) &
    CALL RealVector_GetValue_(obj=obj%a0, nodenum=con(1:nns), VALUE=a0, &
                              tsize=nns)

  SELECT CASE (obj%baseInterpolationForSpace)
  CASE ("LAGR")
    CALL InterpolationPoint_(order=obj%spaceOrder(ielSpace), &
                             elemType=elem%line, &
                             ipType=obj%ipTypeForSpace, &
                             xij=xij, layout="VEFC", &
                             ans=ips, nrow=inds(1), ncol=inds(2))

    DATA(n, 1) = ips(1, 1)
    IF (abool1) DATA(n, 2) = u0(1)
    IF (abool2) DATA(n, 3) = v0(1)
    IF (abool3) DATA(n, 4) = a0(1)

    DO jj = 1, nns - 2
      DATA(n + jj, 1) = ips(1, jj + 2)
      IF (abool1) DATA(n + jj, 2) = u0(jj + 2)
      IF (abool2) DATA(n + jj, 3) = v0(jj + 2)
      IF (abool3) DATA(n + jj, 4) = a0(jj + 2)
    END DO

    n = n + inds(2) - 1
    DATA(n, 1) = ips(1, 2)
    IF (abool1) DATA(n, 2) = u0(2)
    IF (abool2) DATA(n, 3) = v0(2)
    IF (abool3) DATA(n, 4) = a0(2)

  CASE DEFAULT
    DATA(con(1), 1) = xij(1, 1)
    DATA(con(2), 1) = xij(1, 2)

    IF (abool1) DATA(con(1), 2) = DOT_PRODUCT(u0(1:nns), &
                                             obj%spaceShapeFuncBndy(1:nns, 1))
    IF (abool1) DATA(con(2), 2) = DOT_PRODUCT(u0(1:nns), &
                                             obj%spaceShapeFuncBndy(1:nns, 2))

    IF (abool2) DATA(con(1), 3) = DOT_PRODUCT(v0(1:nns), &
                                             obj%spaceShapeFuncBndy(1:nns, 1))
    IF (abool2) DATA(con(2), 3) = DOT_PRODUCT(v0(1:nns), &
                                             obj%spaceShapeFuncBndy(1:nns, 2))

    IF (abool3) DATA(con(1), 4) = DOT_PRODUCT(a0(1:nns), &
                                             obj%spaceShapeFuncBndy(1:nns, 1))
    IF (abool3) DATA(con(2), 4) = DOT_PRODUCT(a0(1:nns), &
                                             obj%spaceShapeFuncBndy(1:nns, 2))

  END SELECT

  xij(1, 1) = xij(1, 2)

END DO

! csv file

! disp
IF (obj%saveData(1)) THEN
#ifdef DEBUG_VER
  CALL Display("Writing data to file: "//filename_disp//".csv")
#endif
  CALL obj%dispfile%Initiate(filename=filename_disp//".csv", unit=100, &
                             status="REPLACE", action="WRITE", &
                             comment="#", separator=",")
  CALL obj%dispfile%OPEN()

  aline = "# time-step = "//tostring(obj%currentTimeStep - 1_I4B)// &
          ", time = "//tostring(obj%currentTime)//" s"
  CALL obj%dispfile%WRITE(aline)

  aline = "x, disp"
  CALL obj%dispfile%WRITE(aline)

  DO ii = 1, totalNodes
    aline = tostring(DATA(ii, 1))//", "//tostring(DATA(ii, 2))
    CALL obj%dispfile%WRITE(aline)
  END DO
  CALL obj%dispfile%DEALLOCATE()
END IF

! vel
IF (obj%saveData(2)) THEN
#ifdef DEBUG_VER
  CALL Display("Writing data to file: "//filename_vel//".csv")
#endif
  CALL obj%velfile%Initiate(filename=filename_vel//".csv", &
                 status="REPLACE", action="WRITE", comment="#", separator=",")
  CALL obj%velfile%OPEN()

  aline = "# time-step = "//tostring(obj%currentTimeStep - 1_I4B)// &
          ", time = "//tostring(obj%currentTime)//" s"
  CALL obj%velfile%WRITE(aline)

  aline = "x, vel"
  CALL obj%velfile%WRITE(aline)

  DO ii = 1, totalNodes
    aline = tostring(DATA(ii, 1))//", "//tostring(DATA(ii, 3))
    CALL obj%velfile%WRITE(aline)
  END DO
  CALL obj%velfile%DEALLOCATE()
END IF

! acc
IF (obj%saveData(3)) THEN
#ifdef DEBUG_VER
  CALL Display("Writing data to file: "//filename_acc//".csv")
#endif
  CALL obj%accfile%Initiate(filename=filename_acc//".csv", &
                 status="REPLACE", action="WRITE", comment="#", separator=",")
  CALL obj%accfile%OPEN()

  aline = "# time-step = "//tostring(obj%currentTimeStep - 1_I4B)// &
          ", time = "//tostring(obj%currentTime)//" s"
  CALL obj%accfile%WRITE(aline)
  aline = "x, acc"
  CALL obj%accfile%WRITE(aline)

  DO ii = 1, totalNodes
    aline = tostring(DATA(ii, 1))//", "//tostring(DATA(ii, 4))
    CALL obj%accfile%WRITE(aline)
  END DO
  CALL obj%accfile%DEALLOCATE()
END IF

! write all data
IF (obj%saveData(4)) THEN
#ifdef DEBUG_VER
  CALL Display("Writing data to file: "//filename_data//".csv")
#endif
  CALL obj%datafile%Initiate(filename=filename_data//".csv", &
                 status="REPLACE", action="WRITE", comment="#", separator=",")
  CALL obj%datafile%OPEN()

  aline = "# time-step = "//tostring(obj%currentTimeStep - 1_I4B)// &
          ", time = "//tostring(obj%currentTime)//" s"
  CALL obj%datafile%WRITE(aline)

  aline = "x, disp, vel, acc"
  CALL obj%datafile%WRITE(aline)
  CALL obj%datafile%WRITE(val=DATA(1:totalNodes, 1:4), orient="ROW")

  CALL obj%datafile%DEALLOCATE()
END IF

#ifdef DEBUG_VER
CALL Display("Done writing files csvfiles")
#endif

! plotting

IF (obj%plotData(1)) THEN
  CALL obj%plot%filename(filename_disp//'.plt')
  CALL obj%plot%options('set terminal pngcairo; set output "' &
                        //filename_disp//'.png"')
  xlim = obj%spaceDomain
  ylim(1) = MINVAL(DATA(1:totalNodes, 2))
  ylim(2) = MAXVAL(DATA(1:totalNodes, 2))
  xlim(1) = xlim(1) - 0.1 * (xlim(2) - xlim(1))
  xlim(2) = xlim(2) + 0.1 * (xlim(2) - xlim(1))
  ylim(1) = ylim(1) - 0.1 * (ylim(2) - ylim(1))
  ylim(2) = ylim(2) + 0.1 * (ylim(2) - ylim(1))

  CALL obj%plot%xlim(xlim)
  CALL obj%plot%ylim(ylim)
  CALL obj%plot%xlabel('x')
  CALL obj%plot%ylabel('u')
  IF (obj%plotWithResult(1)) THEN
    ALLOCATE (refVal(totalNodes))
    args(2) = obj%currentTime
    DO ii = 1, totalNodes
      args(1) = DATA(ii, 1)
      CALL obj%refDisp%Get(val=refVal(ii), args=args)
    END DO
    CALL obj%plot%plot(x1=DATA(1:totalNodes, 1), y1=DATA(1:totalNodes, 2), &
                       x2=DATA(1:totalNodes, 1), y2=refVal, &
                       ls2='with lines dt "_"')
    DEALLOCATE (refVal)
  ELSE
    CALL obj%plot%plot(x1=DATA(1:totalNodes, 1), y1=DATA(1:totalNodes, 2))
  END IF
  CALL obj%plot%reset()

END IF

IF (obj%plotData(2)) THEN
  CALL obj%plot%filename(filename_vel//'.plt')
  CALL obj%plot%options('set terminal pngcairo; set output "' &
                        //filename_vel//'.png"')
  xlim = obj%spaceDomain
  ylim(1) = MINVAL(DATA(1:totalNodes, 3))
  ylim(2) = MAXVAL(DATA(1:totalNodes, 3))
  xlim(1) = xlim(1) - 0.1 * (xlim(2) - xlim(1))
  xlim(2) = xlim(2) + 0.1 * (xlim(2) - xlim(1))
  ylim(1) = ylim(1) - 0.1 * (ylim(2) - ylim(1))
  ylim(2) = ylim(2) + 0.1 * (ylim(2) - ylim(1))

  CALL obj%plot%xlim(xlim)
  CALL obj%plot%ylim(ylim)
  CALL obj%plot%xlabel('x')
  CALL obj%plot%ylabel('v')
  IF (obj%plotWithResult(2)) THEN
    ALLOCATE (refVal(totalNodes))
    args(2) = obj%currentTime
    DO ii = 1, totalNodes
      args(1) = DATA(ii, 1)
      CALL obj%refVel%Get(val=refVal(ii), args=args)
    END DO
    CALL obj%plot%plot(x1=DATA(1:totalNodes, 1), y1=DATA(1:totalNodes, 3), &
                       x2=DATA(1:totalNodes, 1), y2=refVal, &
                       ls2='with lines dt "_"')
    DEALLOCATE (refVal)
  ELSE
    CALL obj%plot%plot(x1=DATA(1:totalNodes, 1), y1=DATA(1:totalNodes, 3))
  END IF
  CALL obj%plot%reset()
END IF

IF (obj%plotData(3)) THEN
  CALL obj%plot%filename(filename_acc//'.plt')
CALL obj%plot%options('set terminal pngcairo; set output "'//filename_acc//'.png"')
  xlim = obj%spaceDomain
  ylim(1) = MINVAL(DATA(1:totalNodes, 4))
  ylim(2) = MAXVAL(DATA(1:totalNodes, 4))
  xlim(1) = xlim(1) - 0.1 * (xlim(2) - xlim(1))
  xlim(2) = xlim(2) + 0.1 * (xlim(2) - xlim(1))
  ylim(1) = ylim(1) - 0.1 * (ylim(2) - ylim(1))
  ylim(2) = ylim(2) + 0.1 * (ylim(2) - ylim(1))

  CALL obj%plot%xlim(xlim)
  CALL obj%plot%ylim(ylim)
  CALL obj%plot%xlabel('x')
  CALL obj%plot%ylabel('a')
  CALL obj%plot%plot(x1=DATA(1:totalNodes, 1), y1=DATA(1:totalNodes, 4))
  CALL obj%plot%reset()
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_WriteData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteErrorData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteErrorData()"
#endif

REAL(DFP) :: xlim(2), ylim(2)

REAL(DFP), ALLOCATABLE :: tmpVec(:), timeData(:)

INTEGER(I4B) :: ii

CHARACTER(:), ALLOCATABLE :: filename_disp, filename_vel, filename_acc, &
                             aline
LOGICAL(LGT) :: abool1, abool2

abool1 = ANY(obj%saveErrorNorm)
abool2 = ANY(obj%plotErrorNorm)

IF (.NOT. abool1 .AND. .NOT. abool2) RETURN

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

filename_disp = obj%result_dir//CHAR_SLASH//obj%filename//'_error_disp'

filename_vel = obj%result_dir//CHAR_SLASH//obj%filename//'_error_vel'

ALLOCATE (timeData(obj%totalTimeElements))
timeData(1) = obj%timeElemLength(1)
DO ii = 2, obj%totalTimeElements
  timeData(ii) = timeData(ii - 1) + obj%timeElemLength(ii)
END DO
! csv file
! disp
IF (obj%saveErrorNorm(1)) THEN
#ifdef DEBUG_VER
  CALL Display("Writing data to file: "//filename_disp//".csv")
#endif
  CALL obj%dispfile%Initiate(filename=filename_disp//".csv", unit=100, &
                             status="REPLACE", action="WRITE", &
                             comment="#", separator=",")
  CALL obj%dispfile%OPEN()

  aline = "#  last line is the sum of error norms"
  CALL obj%dispfile%WRITE(aline)
  aline = getHeader(obj%errorType(1))
  CALL obj%dispfile%WRITE(aline)

  ALLOCATE (tmpVec(SIZE(obj%errorDisp, 2) + 1))
  tmpVec = 0.0_DFP
  DO ii = 1, obj%totalTimeElements
    tmpVec(1) = timeData(ii)
    tmpVec(2:) = obj%errorDisp(ii, :)
    CALL obj%dispfile%WRITE(tmpVec, advance="YES", &
                            orient="ROW")
  END DO
  tmpVec(1) = -1.0_DFP
  tmpVec(2:) = SUM(obj%errorDisp, dim=1)
  CALL obj%dispfile%WRITE(tmpVec, advance="YES", &
                          orient="ROW")
  CALL obj%dispfile%DEALLOCATE()
  DEALLOCATE (tmpVec)
END IF

! vel
IF (obj%saveErrorNorm(2)) THEN
#ifdef DEBUG_VER
  CALL Display("Writing data to file: "//filename_vel//".csv")
#endif
  CALL obj%velfile%Initiate(filename=filename_vel//".csv", unit=100, &
                            status="REPLACE", action="WRITE", &
                            comment="#", separator=",")
  CALL obj%velfile%OPEN()

  aline = "#  last line is the sum of error norms"
  CALL obj%velfile%WRITE(aline)
  aline = getHeader(obj%errorType(1))
  CALL obj%velfile%WRITE(aline)

  ALLOCATE (tmpVec(SIZE(obj%errorVel, 2) + 1))
  tmpVec = 0.0_DFP
  DO ii = 1, obj%totalTimeElements
    tmpVec(1) = timeData(ii)
    tmpVec(2:) = obj%errorVel(ii, :)
    CALL obj%velfile%WRITE(tmpVec, advance="YES", &
                           orient="ROW")
  END DO
  tmpVec(1) = -1.0_DFP
  tmpVec(2:) = SUM(obj%errorVel, dim=1)
  CALL obj%velfile%WRITE(tmpVec, advance="YES", &
                         orient="ROW")
  CALL obj%velfile%DEALLOCATE()
  DEALLOCATE (tmpVec)
END IF

#ifdef DEBUG_VER
CALL Display("Done writing files csvfiles")
#endif

! plotting
IF (obj%plotErrorNorm(1)) THEN
  CALL obj%plot%filename(filename_disp//'.plt')
  CALL obj%plot%options('set terminal pngcairo; set output "' &
                        //filename_disp//'.png"')
  xlim = obj%timeDomain
  ylim(1) = MINVAL(obj%errorDisp)
  ylim(2) = MAXVAL(obj%errorDisp)
  xlim(1) = xlim(1) - 0.1 * (xlim(2) - xlim(1))
  xlim(2) = xlim(2) + 0.1 * (xlim(2) - xlim(1))
  ylim(1) = ylim(1) - 0.1 * (ylim(2) - ylim(1))
  ylim(2) = ylim(2) + 0.1 * (ylim(2) - ylim(1))

  CALL obj%plot%xlim(xlim)
  CALL obj%plot%ylim(ylim)
  CALL obj%plot%xlabel('t')
  CALL obj%plot%ylabel('Norm of Error')
  CALL obj%plot%plot(xv=timeData, ymat=obj%errorDisp)
  CALL obj%plot%reset()

END IF

IF (obj%plotErrorNorm(2)) THEN
  CALL obj%plot%filename(filename_vel//'.plt')
  CALL obj%plot%options('set terminal pngcairo; set output "' &
                        //filename_vel//'.png"')
  xlim = obj%timeDomain
  ylim(1) = MINVAL(obj%errorVel)
  ylim(2) = MAXVAL(obj%errorVel)
  xlim(1) = xlim(1) - 0.1 * (xlim(2) - xlim(1))
  xlim(2) = xlim(2) + 0.1 * (xlim(2) - xlim(1))
  ylim(1) = ylim(1) - 0.1 * (ylim(2) - ylim(1))
  ylim(2) = ylim(2) + 0.1 * (ylim(2) - ylim(1))

  CALL obj%plot%xlim(xlim)
  CALL obj%plot%ylim(ylim)
  CALL obj%plot%xlabel('t')
  CALL obj%plot%ylabel('Error Norm of Velocity')
  CALL obj%plot%plot(xv=timeData, ymat=obj%errorVel)
  CALL obj%plot%reset()

END IF

DEALLOCATE (timeData)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

CONTAINS
FUNCTION getHeader(typeName) RESULT(astr)
  TYPE(String), INTENT(inout) :: typeName
  CHARACTER(:), ALLOCATABLE :: astr

  SELECT CASE (typeName%slice(3, 4))
  CASE ("SP")
    astr = "t, L2SP"
  CASE ("ST")
    astr = "t, L2ST"
  CASE ("BO")
    astr = "t, L2SP, L2ST"
  CASE default
    STOP "no case found"
  END SELECT

END FUNCTION getHeader

END PROCEDURE obj_WriteErrorData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteDataMethods
