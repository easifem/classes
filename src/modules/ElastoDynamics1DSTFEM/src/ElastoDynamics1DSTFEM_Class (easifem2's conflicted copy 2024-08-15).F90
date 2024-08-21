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

MODULE ElastoDynamics1DSTFEM_Class
USE GlobalData, ONLY: DFP, I4B, LGT

USE BaseType, ONLY: QuadraturePoint_, &
                    ElemshapeData_, &
                    poly => TypePolynomialOpt, &
                    qp => TypeQuadratureOpt, &
                    ip => TypeInterpolationOpt, &
                    CSRMatrix_, &
                    DOF_, &
                    RealVector_, &
                    iface_SpaceTimeFunction, &
                    iface_TimeFunction

USE tomlf, ONLY: toml_table

USE FPL, ONLY: ParameterList_

USE TxtFile_Class, ONLY: TxtFile_

USE ExceptionHandler_Class, ONLY: e

USE String_Class, ONLY: String

PRIVATE

PUBLIC :: ElastoDynamics1DSTFEM_

CHARACTER(*), PARAMETER :: modName = 'ElastoDynamics1DSTFEM_Class'
CHARACTER(*), PARAMETER :: prefix = "ElastoDyanmics1DSTFEM"
CHARACTER(*), PARAMETER :: default_result_dir = "./results"
CHARACTER(*), PARAMETER :: default_filename = "ElastoDyanmics1DSTFEM"
CHARACTER(*), PARAMETER :: default_baseInterpolationForSpace = "LAGR"
CHARACTER(*), PARAMETER :: default_baseInterpolationForTime = "LAGR"
CHARACTER(*), PARAMETER :: default_baseTypeForSpace = "Monomial"
CHARACTER(*), PARAMETER :: default_baseTypeForTime = "Monomial"
CHARACTER(*), PARAMETER :: default_ipTypeForSpace = "Equidistance"
CHARACTER(*), PARAMETER :: default_ipTypeForTime = "Equidistance"
CHARACTER(*), PARAMETER :: default_quadTypeForSpace = "GaussLegendre"
CHARACTER(*), PARAMETER :: default_quadTypeForTime = "GaussLegendre"
INTEGER(I4B), PARAMETER :: MAX_ORDER_SPACE = 10
INTEGER(I4B), PARAMETER :: MAX_ORDER_TIME = 10

!----------------------------------------------------------------------------
!                                                   ElastoDynamics1DSTFEM_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Spectral VSTFEM 1D class

TYPE :: ElastoDynamics1DSTFEM_
  LOGICAL(LGT) :: isConnectivity = .FALSE.
  !! is connectivity matrix is calculated
  CHARACTER(2) :: baseContinuityForSpace = "H1"
  CHARACTER(2) :: baseContinuityForTime = "H1"
  !! continuity or conformity of basis defined on reference
  !! element, following values are allowed
  !! H1, HCurl, HDiv, DG
  CHARACTER(4) :: baseInterpolationForSpace = "LAGR"
  CHARACTER(4) :: baseInterpolationForTime = "LAGR"
  !! Type of basis functions used for interpolation on reference
  !! element, Following values are allowed
  !! LagrangeInterpolation ! HierarchyInterpolation
  !! OrthogonalInterpolation ! HermitInterpolation
  !! SerendipityInterpolation
  INTEGER(I4B) :: totalSpaceNodes = 0
  !! total nodes in space

  INTEGER(I4B) :: totalSpaceElements = 0
  !! total elements in space

  INTEGER(I4B) :: totalTimeNodes = 0
  !! total nodes in time domain

  INTEGER(I4B) :: totalTimeElements = 0
  !! total elements in time domain

  INTEGER(I4B) :: totalVertexDOFSpace = 0
  !! Total number of degree of freedom in space

  INTEGER(I4B) :: totalEdgeDOFSpace = 0
  !! total number of degree of freedom in space

  INTEGER(I4B) :: totalVertexDOFTime = 0
  !! Total number of degree of freedom in time

  INTEGER(I4B) :: totalEdgeDOFTime = 0
  !! total number of degree of freedom in time

  INTEGER(I4B) :: baseTypeForSpace = poly%monomial
  !! basisType for space
  !! It is used for LagrangeInterpolation

  INTEGER(I4B) :: ipTypeForSpace = ip%Equidistance
  !! interpolation point type for space
  !! It is used for LagrangeInpolation

  INTEGER(I4B) :: baseTypeForTime = poly%monomial
  !! basis type for time
  !! It is used for LagrangeInpolation

  INTEGER(I4B) :: ipTypeForTime = ip%Equidistance
  !! interpolation point type for time
  !! It is used for LagrangeInpolation

  INTEGER(I4B) :: quadTypeForSpace = qp%GaussLegendre
  !! quadrature type for space

  INTEGER(I4B) :: quadTypeForTime = qp%GaussLegendre
  !! quadrature type for time

  INTEGER(I4B) :: maxSpaceOrder = 0
  !! maximum space order

  INTEGER(I4B) :: maxTimeOrder = 0
  !! maximum time order

  REAL(DFP) :: spaceDomain(2) = 0.0_DFP
  !! Length of the space domain

  REAL(DFP) :: timeDomain(2) = 0.0_DFP
  !! Total length of time domain

  INTEGER(I4B), ALLOCATABLE :: spaceOrder(:)
  !! space order of each element

  INTEGER(I4B), ALLOCATABLE :: timeOrder(:)
  !! time order of each element

  INTEGER(I4B), ALLOCATABLE :: totalDOFSpace(:)
  !! total number of degree of freedom in space in space element

  INTEGER(I4B), ALLOCATABLE :: totalDOFTime(:)
  !! total number of degree of freedom in time element

  REAL(DFP), ALLOCATABLE :: spaceElemLength(:)
  !! length of each space element
  !! the size should be totalSpaceElements

  REAL(DFP), ALLOCATABLE :: timeElemLength(:)
  !! length of each time element
  !! the size should be totalTimeElements

  REAL(DFP), ALLOCATABLE :: elasticModulus(:)
  !! elastic modulus
  !! the size should be tElements

  REAL(DFP), ALLOCATABLE :: density(:)
  !! density
  !! the size should be tElements

  REAL(DFP), ALLOCATABLE :: rayleighAlpha(:)
  !! Rayleigh damping coefficient

  REAL(DFP), ALLOCATABLE :: rayleighBeta(:)
  !! Rayleigh damping coefficient

  TYPE(String) :: result_dir
  !! Result directory name

  TYPE(String) :: filename
  !! Filename

  TYPE(QuadraturePoint_) :: quadForSpace
  !! Quadrature points in space

  TYPE(QuadraturePoint_) :: quadForTime
  !! Quadrature points in time

  TYPE(ElemshapeData_) :: elemsdForSpace
  !! Element shape data for space

  TYPE(ElemshapeData_) :: elemsdForTime
  !! Element shape data for time

  REAL(DFP) :: timeShapeFunc0(MAX_ORDER_TIME + 1)
  !! Value of time shape function at theta = -1

  REAL(DFP) :: ct(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1)
  !! Ct matrix, see notes

  REAL(DFP) :: mt(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1)
  !! mass matrix in time

  REAL(DFP) :: mtplus(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1)
  !! mass like matrix (computed at -1)

  REAL(DFP) :: kt_tilda(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1)
  !! space-time displacement matrix

  REAL(DFP) :: bt(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1)
  !! bt matrix, see notes

  REAL(DFP) :: wt(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1)
  REAL(DFP) :: wmt(MAX_ORDER_TIME + 1, MAX_ORDER_TIME + 1)
  !! transpose of wt*mt
  !! needed to make bt
  !! this matrix is made when we call GetWt

  REAL(DFP) :: at(MAX_ORDER_TIME + 1)
  !! At matrix

  REAL(DFP) :: tat(MAX_ORDER_TIME + 1)
  !! integral of shape function of time times at

  REAL(DFP) :: ks(MAX_ORDER_SPACE + 1, MAX_ORDER_SPACE + 1)
  !! space stiffness matrix

  REAL(DFP) :: ms(MAX_ORDER_SPACE + 1, MAX_ORDER_SPACE + 1)
  !! space mass matrix

  REAL(DFP) :: cs(MAX_ORDER_SPACE + 1, MAX_ORDER_SPACE + 1)
  !! space damping matrix

  REAL(DFP) :: ke((MAX_ORDER_SPACE + 1) * (MAX_ORDER_TIME + 1), &
                  (MAX_ORDER_SPACE + 1) * (MAX_ORDER_TIME + 1))

  REAL(DFP) :: rhse((MAX_ORDER_SPACE + 1) * (MAX_ORDER_TIME + 1))

  TYPE(DOF_) :: dof

  TYPE(CSRMatrix_) :: tanmat
  !! tangent matrix

  TYPE(RealVector_) :: rhs, sol, u0, v0, a0
  !! Right hand side, solution, initial displacement, initial
  !! velocity
  !! These are all degree of freedom in general
  !! Size of rhs and sol is same as the size of problem
  !! (space-time dof)
  !! Size of u0, v0, a0 is same as the size of space dof

  INTEGER(I4B), ALLOCATABLE :: conIA(:), conJA(:)
  !!

  PROCEDURE(iface_SpaceTimeFunction), POINTER :: bodyForce => NULL()
  !! body force

  PROCEDURE(iface_TimeFunction), POINTER :: tractionRight => NULL()
  !! traction force on right boundary

  PROCEDURE(iface_TimeFunction), POINTER :: tractionLeft => NULL()
  !! traction force on left boundary

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate data

  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import data from toml file

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml2 => &
    obj_ImportFromToml2
  !! Import data from toml file

  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display data

  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
  !! set the problem

  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadForSpace => obj_SetQuadForSpace
  !! Set quadrature points for space

  PROCEDURE, PUBLIC, PASS(obj) :: SetQuadForTime => obj_SetQuadForTime
  !! Set quadrature points for time

  PROCEDURE, PUBLIC, PASS(obj) :: SetElemsdForSpace => obj_SetElemsdForSpace
  !! Set element shape data for space

  PROCEDURE, PUBLIC, PASS(obj) :: SetElemsdForTime => obj_SetElemsdForTime
  !! Set element shape data for time

  PROCEDURE, PUBLIC, PASS(obj) :: GetMt => obj_GetMt
  !! Get Mt matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetMtPlus => obj_GetMtPlus
  !! Get MtPlus matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetCt => obj_GetCt
  !! Get Ct matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetWt => obj_GetWt
  !! Get Wt matrix
  !! It needs Ct, Mt, Mtplus

  PROCEDURE, PUBLIC, PASS(obj) :: GetAt => obj_GetAt
  !! Get At matrix
  !! it needs wt

  PROCEDURE, PUBLIC, PASS(obj) :: GetBt => obj_GetBt
  !! Get Bt matrix
  !! It needs Wt

  PROCEDURE, PUBLIC, PASS(obj) :: GetKt_Tilda => obj_GetKt_Tilda
  !! Get Kt_tilda matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetMs => obj_GetMs
  !! Get Ms matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetCs => obj_GetCs
  !! Get Cs matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetKs => obj_GetKs
  !! Get Ks matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetBodyForce => obj_GetBodyForce
  !! Get body force function

  PROCEDURE, PUBLIC, PASS(obj) :: GetTractionLeft => obj_GetTractionLeft
  !! Get traction on left

  PROCEDURE, PUBLIC, PASS(obj) :: GetTractionRight => obj_GetTractionRight
  !! Get traction on right

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateConnectivity => &
    obj_InitiateConnectivity

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFields => obj_InitiateFields
  !! Initiate tangent matrix

  PROCEDURE, PUBLIC, PASS(obj) :: AssembleTanmat => obj_AssembleTanmat
  !! Assemble Tangent matrix

  PROCEDURE, PUBLIC, PASS(obj) :: AssembleRHS => obj_AssembleRHS
  !! Assemble RHS matrix

  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity => obj_GetConnectivity
  !! Get connectivity matrix

  PROCEDURE, PUBLIC, PASS(obj) :: Debug => obj_Debug
  !! Debug mode

END TYPE ElastoDynamics1DSTFEM_

!----------------------------------------------------------------------------
!                              -                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-02
! summary:  Initiate by param

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, param)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-02
! summary:  Deallocate the data

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-02
! summary:  Display the data

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-08-02
! summary:  Import data from toml config

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-02
! summary:  Import data from toml config

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-02
! summary:  Set the problem

INTERFACE
  MODULE SUBROUTINE obj_Set(obj)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetQuadForSpace
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetQuadForSpace(obj, iel)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iel
  END SUBROUTINE obj_SetQuadForSpace
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetQuadForTime
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetQuadForTime(obj, iel)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iel
  END SUBROUTINE obj_SetQuadForTime
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetElemsdForSpace@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetElemsdForSpace(obj, iel)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iel
  END SUBROUTINE obj_SetElemsdForSpace
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetElemsdForTime@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetElemsdForTime(obj, iel)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iel
  END SUBROUTINE obj_SetElemsdForTime
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetCt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-03
! summary:  Get Ct matrix (see notes)

INTERFACE
  MODULE SUBROUTINE obj_GetCt(obj, ans, nrow, ncol)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetCt
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetMt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-03
! summary:  Get Mt matrix (see notes)

INTERFACE
  MODULE SUBROUTINE obj_GetMt(obj, ans, nrow, ncol)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetMt
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetMtPlus@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-03
! summary:  get MtPlus matrix (see notes), this matrix is due to the jump

INTERFACE
  MODULE SUBROUTINE obj_GetMtPlus(obj, ans, nrow, ncol)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetMtPlus
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetAt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-08-05
! summary:  It is coefficient of Un in disp-vel relation
!
!# Introduction
!
! Make sure ct, mtplus, timeShapeFunc0 is calculated before calling this

INTERFACE
  MODULE SUBROUTINE obj_GetAt(obj, ans, tsize)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetAt
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetBt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-08-05
! summary:  It is coefficient of Vn in disp-vel relation

INTERFACE
  MODULE SUBROUTINE obj_GetBt(obj, ans, nrow, ncol)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetBt
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetWt@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-07
! summary:  Wt matrix (see notes)

INTERFACE
  MODULE SUBROUTINE obj_GetWt(obj, ans, nrow, ncol)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetWt
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetKt_Tilda@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-05
! summary:  Get Kt tilda matrix

INTERFACE
  MODULE SUBROUTINE obj_GetKt_Tilda(obj, ans, nrow, ncol)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetKt_Tilda
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetMs@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-05
! summary:  Get Ms matrix

INTERFACE
  MODULE SUBROUTINE obj_GetMs(obj, ans, nrow, ncol)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetMs
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetKs@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-05
! summary:  Get Ks matrix

INTERFACE
  MODULE SUBROUTINE obj_GetKs(obj, ans, nrow, ncol)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetKs
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetBodyForce@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-05
! summary:  Get body force function

INTERFACE
  MODULE SUBROUTINE obj_GetBodyForce(obj, ans, tsize)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetBodyForce
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetTractionRight@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-05
! summary:  Get traction on right

INTERFACE
  MODULE SUBROUTINE obj_GetTractionRight(obj, ans, tsize)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetTractionRight
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetTractionLeft@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetTractionLeft(obj, ans, tsize)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetTractionLeft
END INTERFACE

!----------------------------------------------------------------------------
!                                                InitiateConnectivity@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_InitiateConnectivity(obj)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                     InitiateFields@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_InitiateFields(obj, timeElemNum)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: timeElemNum
    !! time step number
  END SUBROUTINE obj_InitiateFields
END INTERFACE

!----------------------------------------------------------------------------
!                                                     AssembleTanmat@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_AssembleTanmat(obj, timeElemNum)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: timeElemNum
  END SUBROUTINE obj_AssembleTanmat
END INTERFACE

!----------------------------------------------------------------------------
!                                                        AssembleRHS@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-12
! summary:  Assemble RHS

INTERFACE
  MODULE SUBROUTINE obj_AssembleRHS(obj, timeElemNum)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: timeElemNum
  END SUBROUTINE obj_AssembleRHS
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetCs@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-05
! summary:  Get Cs matrix

INTERFACE
  MODULE SUBROUTINE obj_GetCs(obj, alpha, beta, ans, nrow, ncol)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: alpha, beta
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetCs
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Debug@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-05
! summary:  Debug mode

INTERFACE
  MODULE SUBROUTINE obj_Debug(obj)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Debug
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetConnectivity@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-09
! summary:  Get connectivity matrix

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity(obj, spaceElemNum, timeElemNum, &
                                        ans, tsize)
    CLASS(ElastoDynamics1DSTFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceElemNum
    INTEGER(I4B), INTENT(IN) :: timeElemNum
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ElastoDynamics1DSTFEM_Class
