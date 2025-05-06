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

MODULE Helmholtz1DFEM_Class
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
                    iface_1DFunction

USE tomlf, ONLY: toml_table

USE FPL, ONLY: ParameterList_

USE TxtFile_Class, ONLY: TxtFile_

USE ExceptionHandler_Class, ONLY: e

USE String_Class, ONLY: String

USE UserFunction_Class, ONLY: UserFunction_

USE GnuPlot_Class, ONLY: GnuPlot_

USE CSVFile_Class, ONLY: CSVFile_

PRIVATE

PUBLIC :: Helmholtz1DFEM_

CHARACTER(*), PARAMETER :: modName = 'Helmholtz1DFEM_Class'
CHARACTER(*), PARAMETER :: prefix = "Helmholtz1DFEM"
CHARACTER(*), PARAMETER :: default_result_dir = "./results"
CHARACTER(*), PARAMETER :: default_filename = "Helmholtz1DFEM"
CHARACTER(*), PARAMETER :: default_baseInterpolationForSpace = "LAGR"
CHARACTER(*), PARAMETER :: default_baseTypeForSpace = "Monomial"
CHARACTER(*), PARAMETER :: default_ipTypeForSpace = "Equidistance"
CHARACTER(*), PARAMETER :: default_quadTypeForSpace = "GaussLegendre"
INTEGER(I4B), PARAMETER :: MAX_ORDER_SPACE = 10
INTEGER(I4B), PARAMETER :: default_verbosity = 0

!----------------------------------------------------------------------------
!                                                   Helmholtz1DFEM_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-17
! summary:  Helmholtz 1D FEM class

TYPE :: Helmholtz1DFEM_
  LOGICAL(LGT) :: isConnectivity = .FALSE.
  !! is connectivity matrix is calculated
  CHARACTER(2) :: baseContinuityForSpace = "H1"
  !! continuity or conformity of basis defined on reference
  !! element, following values are allowed
  !! H1, HCurl, HDiv, DG
  CHARACTER(4) :: baseInterpolationForSpace = "LAGR"
  !! Type of basis functions used for interpolation on reference
  !! element, Following values are allowed
  !! LagrangeInterpolation ! HierarchyInterpolation
  !! OrthogonalInterpolation ! HermitInterpolation
  !! SerendipityInterpolation

  INTEGER(I4B) :: verbosity = 0
  !! verbosity level
  !! 0 means minimum

  INTEGER(I4B) :: totalSpaceNodes = 0
  !! total nodes in space

  INTEGER(I4B) :: totalSpaceElements = 0
  !! total elements in space

  INTEGER(I4B) :: totalVertexDOFSpace = 0
  !! Total number of degree of freedom in space

  INTEGER(I4B) :: totalEdgeDOFSpace = 0
  !! total number of degree of freedom in space

  INTEGER(I4B) :: baseTypeForSpace = poly%monomial
  !! basisType for space
  !! It is used for LagrangeInterpolation

  INTEGER(I4B) :: ipTypeForSpace = ip%Equidistance
  !! interpolation point type for space
  !! It is used for LagrangeInpolation

  INTEGER(I4B) :: quadTypeForSpace = qp%GaussLegendre
  !! quadrature type for space

  INTEGER(I4B) :: maxSpaceOrder = 0
  !! maximum space order

  REAL(DFP) :: spaceDomain(2) = 0.0_DFP
  !! Length of the space domain

  INTEGER(I4B), ALLOCATABLE :: spaceOrder(:)
  !! space order of each element

  INTEGER(I4B), ALLOCATABLE :: totalDOFSpace(:)
  !! total number of degree of freedom in space in space element

  REAL(DFP), ALLOCATABLE :: spaceElemLength(:)
  !! length of each space element
  !! the size should be totalSpaceElements

  REAL(DFP), ALLOCATABLE :: wavenumber(:)
  !! elastic modulus
  !! the size should be tElements

  TYPE(String) :: result_dir
  !! Result directory name

  TYPE(String) :: filename
  !! Filename

  TYPE(QuadraturePoint_) :: quadForSpace
  !! Quadrature points in space

  TYPE(ElemshapeData_) :: linElemsdForSpace
  !! linearElement shape data for space

  TYPE(ElemshapeData_) :: elemsdForSpace
  !! Element shape data for space

  REAL(DFP) :: spaceShapeFuncBndy(MAX_ORDER_SPACE + 1, 2)
  !! Value of space shape function at xi = 1

  REAL(DFP) :: at_right
  !! At matrix at +1

  REAL(DFP) :: ks(MAX_ORDER_SPACE + 1, MAX_ORDER_SPACE + 1)
  !! space stiffness matrix

  REAL(DFP) :: ms(MAX_ORDER_SPACE + 1, MAX_ORDER_SPACE + 1)
  !! space mass matrix

  REAL(DFP) :: cs(MAX_ORDER_SPACE + 1, MAX_ORDER_SPACE + 1)
  !! space damping matrix

  REAL(DFP) :: rhse(MAX_ORDER_SPACE + 1)

  INTEGER(I4B), ALLOCATABLE :: subIndices(:)
  !! subindices, used when getting submatrix
  !! while apply dirichlet boudnary condition

  TYPE(DOF_) :: dof

  TYPE(CSRMatrix_) :: tanmat, submat
  !! tangent matrix

  TYPE(RealVector_) :: rhs, sol
  !! Right hand side, solution
  !! These are all degree of freedom in general
  !! Size of rhs and sol is same as the size of problem
  !! (space dof)

  INTEGER(I4B), ALLOCATABLE :: conIA(:), conJA(:)
  !!

  INTEGER(I4B) :: dbc_idof(+2)
  !! internal use only
  !! used in ApplyDirichletBCLeft

  REAL(DFP) :: dbc_coeff(2)
  !! internal use only
  !! used in ApplyDirichletBCLeft

  REAL(DFP) :: dbc_rhs(1)
  !! internal use only
  !! used in ApplyDirichletBCLeft

  REAL(DFP) :: dbc_value(2)
  !! internal use only
  !! used in ApplyDirichletBCLeft

  INTEGER(I4B), ALLOCATABLE :: ipar(:)
  !! for linear solver

  REAL(DFP), ALLOCATABLE :: fpar(:), work(:)
  !! for linear solver

  TYPE(GnuPlot_) :: plot
  !! for plotting

  TYPE(UserFunction_), POINTER :: bodyForce => NULL()
  !! body force

  TYPE(UserFunction_), POINTER :: tractionRight => NULL()
  !! traction force on right boundary

  TYPE(UserFunction_), POINTER :: tractionLeft => NULL()
  !! traction force on left boundary

  TYPE(UserFunction_), POINTER :: displacementRight => NULL()
  !! displacement on right boundary

  TYPE(UserFunction_), POINTER :: displacementLeft => NULL()
  !! displacement on left boundary

  TYPE(CSVFile_) :: dispfile, datafile
  !! file to write displacement

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

  PROCEDURE, PUBLIC, PASS(obj) :: SetElemsdForSpace => obj_SetElemsdForSpace
  !! Set element shape data for space

  PROCEDURE, PUBLIC, PASS(obj) :: GetMs => obj_GetMs
  !! Get Ms matrix

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

  PROCEDURE, PUBLIC, PASS(obj) :: ApplyDirichletBC => &
    obj_ApplyDirichletBC
  !! Apply Left Dirichlet boundary condition

  PROCEDURE, PUBLIC, PASS(obj) :: Solve => obj_Solve
  !! Solve

  PROCEDURE, PUBLIC, PASS(obj) :: WriteData => obj_WriteData

  PROCEDURE, PUBLIC, PASS(obj) :: Run => obj_Run
  !! Debug mode

END TYPE Helmholtz1DFEM_

!----------------------------------------------------------------------------
!                              -                            Initiate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  Initiate the data

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, param)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  Initiate the data

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  Display the data

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  Import data from toml config

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  Import data from toml config

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  set the data

INTERFACE
  MODULE SUBROUTINE obj_Set(obj)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetQuadForSpace
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  set the data of quadrature points

INTERFACE
  MODULE SUBROUTINE obj_SetQuadForSpace(obj, spaceElemNum)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceElemNum
  END SUBROUTINE obj_SetQuadForSpace
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetElemsdForSpace@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  set the data of element shape functions

INTERFACE
  MODULE SUBROUTINE obj_SetElemsdForSpace(obj, spaceElemNum, xij)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceElemNum
    REAL(DFP), INTENT(IN) :: xij(1, 2)
  END SUBROUTINE obj_SetElemsdForSpace
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetMs@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  calculate the element mass matrix

INTERFACE
  MODULE SUBROUTINE obj_GetMs(obj, ans, nrow, ncol)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetMs
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetKs@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  calculate the element stiffness matrix

INTERFACE
  MODULE SUBROUTINE obj_GetKs(obj, ans, nrow, ncol)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_GetKs
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetBodyForce@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  calculate the element body force vector

INTERFACE
  MODULE SUBROUTINE obj_GetBodyForce(obj, ans, tsize, spaceElemNum, &
                                     anscoeff, scale)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), INTENT(IN) :: spaceElemNum
    REAL(DFP), INTENT(IN) :: anscoeff
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_GetBodyForce
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetTractionRight@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  apply neumann boundary condition at the right side

INTERFACE
  MODULE SUBROUTINE obj_GetTractionRight(obj, ans, tsize, &
                                         anscoeff, scale)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), INTENT(IN) :: anscoeff
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_GetTractionRight
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetTractionLeft@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  apply neumann boundary condition at the left side

INTERFACE
  MODULE SUBROUTINE obj_GetTractionLeft(obj, ans, tsize, &
                                        anscoeff, scale)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), INTENT(IN) :: anscoeff
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_GetTractionLeft
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ApplyDirichletBC@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC(obj)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_ApplyDirichletBC
END INTERFACE

!----------------------------------------------------------------------------
!                                                InitiateConnectivity@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  Initiate the connectivity

INTERFACE
  MODULE SUBROUTINE obj_InitiateConnectivity(obj)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetConnectivity@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  Get the connectivity

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity(obj, spaceElemNum, ans, tsize)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceElemNum
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                     InitiateFields@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  Initiate the fields for Helmholtz1DFEM_

INTERFACE
  MODULE SUBROUTINE obj_InitiateFields(obj)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateFields
END INTERFACE

!----------------------------------------------------------------------------
!                                                     AssembleTanmat@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  assemble the tangent matrix

INTERFACE
  MODULE SUBROUTINE obj_AssembleTanmat(obj)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleTanmat
END INTERFACE

!----------------------------------------------------------------------------
!                                                        AssembleRHS@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  assemble the right hand side vector

INTERFACE
  MODULE SUBROUTINE obj_AssembleRHS(obj)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_AssembleRHS
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Solve@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  Solve the linear system

INTERFACE
  MODULE SUBROUTINE obj_Solve(obj)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Solve
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Debug@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  Solve the helmholtz equation

INTERFACE
  MODULE SUBROUTINE obj_Run(obj)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Run
END INTERFACE

!----------------------------------------------------------------------------
!                                                         WriteData@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-18
! summary:  write the data to csv and plt for gnuplot

INTERFACE
  MODULE SUBROUTINE obj_WriteData(obj)
    CLASS(Helmholtz1DFEM_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_WriteData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Helmholtz1DFEM_Class
