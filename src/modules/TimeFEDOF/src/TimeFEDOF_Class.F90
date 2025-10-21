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
  TYPE(TimeOpt_), POINTER :: opt => NULL()
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
  PROCEDURE, PASS(obj) :: GetQuadraturePoints => obj_GetQuadraturePoints
  !! Get quadrature points for isotropic order
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemShapeData => &
    obj_GetLocalElemShapeData
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalElemShapeData => &
    obj_GetGlobalElemShapeData
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
    obj, order, timeOpt, baseContinuity, baseInterpolation, fetype, ipType, &
    basisType, alpha, beta, lambda, quadratureType, quadratureOrder, &
    quadratureNips, quadratureAlpha, quadratureBeta, quadratureLambda)
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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType
    !! Quadrature type
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureOrder
    !! Accuracy of quadrature rule
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureNips(1)
    !! Number of integration points
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureAlpha
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureBeta
    !! Jacobi parameter for quadrature
    REAL(DFP), OPTIONAL, INTENT(IN) :: quadratureLambda
    !! Ultraspherical parameter for quadrature
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
!                                                         GetQuadraturePoints
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetQuadraturePoints(obj, quad, quadratureType, &
                                            order, nips, alpha, beta, lambda)
    CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
    !! TimeFEDOF object
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
    !! quadrature points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType
    !! Type of quadrature points
    !! Read the docs of AbstractOneDimFE and OneDimQuadratureOpt
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Order of integrand
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nips(1)
    !! Number of integration points
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE obj_GetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetLocalElemShapeData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-13
! summary:  Get local element shape data

INTERFACE
  MODULE SUBROUTINE obj_GetLocalElemShapeData(obj, elemsd, quad)
    CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
    TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
    TYPE(QuadraturePoint_), INTENT(IN) :: quad
  END SUBROUTINE obj_GetLocalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-13
! summary:  Get global element shape data

INTERFACE
  MODULE SUBROUTINE obj_GetGlobalElemShapeData(obj, elemsd, xij, geoElemsd)
    CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
    !! Abstract finite element
    TYPE(ElemshapeData_), INTENT(INOUT) :: elemsd
    !! global element shape data
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates of element
    !! The number of rows in xij should be same as the spatial dimension
    !! The number of columns should be same as the number of nodes
    !! present in the reference element in geoElemsd.
    TYPE(ElemShapeData_), OPTIONAL, INTENT(INOUT) :: geoElemsd
    !! shape function data for geometry which contains local shape function
    !! data. If not present then the local shape function in elemsd
    !! will be used for geometry. This means we are dealing with
    !! isoparametric shape functions.
  END SUBROUTINE obj_GetGlobalElemShapeData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE TimeFEDOF_Class
