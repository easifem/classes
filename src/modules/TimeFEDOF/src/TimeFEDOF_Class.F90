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

MODULE TimeFEDOF_Class
USE GlobalData, ONLY: DFP, I4B, LGT, INT8
USE ExceptionHandler_Class, ONLY: e
USE BaseType, ONLY: QuadraturePoint_, ElemshapeData_
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE TimeOpt_Class, ONLY: TimeOpt_
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

IMPLICIT NONE
PRIVATE

PUBLIC :: TimeFEDOF_
PUBLIC :: TimeFEDOFPointer_

CHARACTER(*), PARAMETER :: modName = "TimeFEDOF_Class"

!----------------------------------------------------------------------------
!                                                              TimeFEDOF_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: TimeFEDOF data type

TYPE :: TimeFEDOF_
  PRIVATE
  LOGICAL(LGT) :: isinit = .FALSE.
  !! It is set to true when TimeFEDOF is initiated
  LOGICAL(LGT) :: isLagrange = .FALSE.
  !! It is true when baseInterpolation is Lagrange
  LOGICAL(LGT) :: isMaxConSet = .FALSE.
  !! It is set to true when maxCon is set in GetMaxTotalConnectivity
  LOGICAL(LGT) :: isMaxQuadPointSet = .FALSE.
  !! It is set to true when maxQuadPoint is set in
  !! GetMaxTotalQuadraturePoints
  INTEGER(I4B) :: tdof = 0
  !! Total number of degrees of freedom
  INTEGER(I4B) :: maxCon = 0
  !! maximum number of connectivity
  INTEGER(I4B) :: maxQuadPoint = 0
  !! maximum number of quadrature points

  CHARACTER(2) :: baseContinuity = "H1"
  !! continuity or conformity of basis defined on reference
  !! element, following values are allowed
  !! H1, DG
  CHARACTER(4) :: baseInterpolation = "LAGR"
  !! Type of basis functions used for interpolation on reference
  !! element, Following values are allowed
  !! LAGR: LagrangeInterpolation
  !! HIER: HierarchyInterpolation
  !! ORTHO: OrthogonalInterpolation

  INTEGER(INT8) :: scaleForQuadOrder = 2_INT8
  !! Scale for quadrature order
  !! Quadrature order = element order * scaleForQuadOrder
  !! This is used for constructing the quadrature points

  INTEGER(INT8) :: cellOrder = 0
  !! Order of time element

  CLASS(TimeOpt_), POINTER :: opt => NULL()
  !! option related to the time domain discretization
  CLASS(AbstractOneDimFE_), POINTER :: fe => NULL()
  !! pointer to finite element object
  !! point, line, triangle, quadrangle, tetrahedron, hexahedron, prism,
  !! pyramid

CONTAINS
  PRIVATE

  !CONSTRUCTOR:
  !@ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate TimeFEDOF by using inhomogeneous order
  PROCEDURE, PUBLIC, PASS(obj) :: Copy => obj_Copy
  !! Copy
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  !! Returns true of the TimeFEDOF is initiated

  !IO:
  !@IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the contents of TimeFEDOF
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import from toml
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import from toml
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !! Import from toml file

  !GET:
  !@GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetCaseName => obj_GetCaseName
  !! Get the case name of TimeFEDOF, it returns
  !! baseContinuity+baseInterpolation
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalDOF => obj_GetTotalDOF
  !! Retuns the total degrees of freedom in TimeFEDOF
  PROCEDURE, PUBLIC, PASS(obj) :: GetTimeOptPointer => obj_GetTimeOptPointer
  !! Get the pointer to timeOpt
  PROCEDURE, PUBLIC, PASS(obj) :: GetBaseInterpolation => &
    obj_GetBaseInterpolation
  !! Get the base interpolation
  PROCEDURE, PUBLIC, PASS(obj) :: GetCellOrder => obj_GetCellOrder
  !! Get the cell order
  PROCEDURE, PUBLIC, PASS(obj) :: GetFEPointer => obj_GetFEPointer
  !! Get FE pointer
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxTotalConnectivity => &
    obj_GetMaxTotalConnectivity
  !! Get maximum total connectivity
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxTotalQuadraturePoints => &
    obj_GetMaxTotalQuadraturePoints
  !! Get the maximum total quadrature points

  !SET:
  !@SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetFE => obj_SetFE
  !! Set FE object
END TYPE TimeFEDOF_

!----------------------------------------------------------------------------
!                                                             TimeFEDOFPointer_
!----------------------------------------------------------------------------

TYPE :: TimeFEDOFPointer_
  TYPE(TimeFEDOF_), POINTER :: ptr => NULL()
END TYPE TimeFEDOFPointer_

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-14
! summary: Initiate an instance of fe dof
!
!# Introduction
! This method makes order0(1) from order and calls obj_Initiate2.

INTERFACE
  MODULE SUBROUTINE obj_Initiate( &
    obj, order, timeOpt, baseContinuity, baseInterpolation, feType, ipType, &
    basisType, alpha, beta, lambda, dofType, transformType, &
    quadratureType, quadratureOrder, quadratureIsOrder, quadratureNips, &
    quadratureIsNips, quadratureAlpha, quadratureBeta, quadratureLambda, &
    scaleForQuadOrder)
    CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: order
    !! homogeneous value of order
    TYPE(TimeOpt_), TARGET, INTENT(IN) :: timeOpt
    !! cell mesh
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of basis (regularity)
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! basis function used for interpolation
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fetype
    !! Finite element type
    !! Default is Scalar, (Vector)
    !! Read docs of AbstractOneDimFE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    !! interpolation type
    !! Read docs of AbstractOneDimFE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! type of basis function used for constructing the Lagrange polynomial
    !! Used when baseInterpolation is Lagrange
    !! Read docs of AbstractOneDimFE
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha parameter for jacobian polynomial
    !! Read docs of AbstractOneDimFE
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta parameter for jacobian polynomial
    !! Read docs of AbstractOneDimFE
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda parameter for Ultraspherical parameter
    !! used when baseInterpolation is Lagrange
    !! used when basistype is Ultraspherical
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dofType
    !! Degree of freedom type, default is nodal
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: transformType
    !! transformation type, from reference element to physical element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType
    !! Quadrature type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder
    !! Accuracy of quadrature rule
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsOrder
    !! Is quadrature order considered
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips
    !! Number of integration points
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: quadratureIsNips
    !! Should we consider quadratureNips
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda
    !! Ultraspherical parameter for quadrature
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: scaleForQuadOrder
    !! Scale for quadrature order, if quadratureOrder  or
    !! quadratureNips are both not provided, then you can specify
    !! scaleForQuadOrder to set the quadrature order
    !! In this case quadratureOrder = order * scaleForQuadOrder
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Copy@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: This method Copy obj2 in obj
!
!# Introduction
! This method is used to copy the contents of obj2 in obj.
! This method is same as the assignment operator (=).

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
    CLASS(TimeFEDOF_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Deallocate the data

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                              IsInitiated@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-14
! summary:  Returns true if the TimeFEDOF is initiated

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(TimeFEDOF_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-19
! summary: Display the content of FE DOF

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, timeOpt)
    CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(TimeOpt_), TARGET, INTENT(IN) :: timeOpt
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, &
                                        filename, printToml, timeOpt)
    CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    CLASS(TimeOpt_), OPTIONAL, INTENT(IN) :: timeOpt
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetCaseName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-27
! summary:  Get the case name of TimeFEDOF

INTERFACE
  MODULE FUNCTION obj_GetCaseName(obj) RESULT(ans)
    CLASS(TimeFEDOF_), INTENT(IN) :: obj
    CHARACTER(6) :: ans
  END FUNCTION obj_GetCaseName
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-21
! summary: Returns total number of dof in the TimeFEDOF

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF(obj) RESULT(ans)
    CLASS(TimeFEDOF_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTimeOptPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-23
! summary: Get the pointer to TimeOpt object

INTERFACE
  MODULE FUNCTION obj_GetTimeOptPointer(obj) RESULT(ans)
    CLASS(TimeFEDOF_), INTENT(IN) :: obj
    CLASS(TimeOpt_), POINTER :: ans
  END FUNCTION obj_GetTimeOptPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetBaseInterpolation@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Get the base interpolation

INTERFACE
  MODULE FUNCTION obj_GetBaseInterpolation(obj) RESULT(ans)
    CLASS(TimeFEDOF_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetBaseInterpolation
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetCellOrder@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-17
! summary:  Get the order of cell

INTERFACE
  MODULE FUNCTION obj_GetCellOrder(obj) RESULT(ans)
    CLASS(TimeFEDOF_), INTENT(IN) :: obj
    !! TimeFEDOF object
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetCellOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetFEPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-20
! summary:  Get FE pointer

INTERFACE
  MODULE FUNCTION obj_GetFEPointer(obj) RESULT(ans)
    CLASS(TimeFEDOF_), INTENT(IN) :: obj
    CLASS(AbstractOneDimFE_), POINTER :: ans
  END FUNCTION obj_GetFEPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetMaxTotalConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-21
! summary: Returns maxCon

INTERFACE
  MODULE FUNCTION obj_GetMaxTotalConnectivity(obj) RESULT(ans)
    CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxTotalConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetMaxTotalConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-21
! summary: Returns maximum total quadrature points

INTERFACE
  MODULE FUNCTION obj_GetMaxTotalQuadraturePoints(obj) RESULT(ans)
    CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxTotalQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetFE@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetFE(obj)
    CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetFE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE TimeFEDOF_Class
