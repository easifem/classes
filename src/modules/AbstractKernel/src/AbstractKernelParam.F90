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

MODULE AbstractKernelParam
USE GlobalData, ONLY: DFP, I4B, LGT
USE String_Class, ONLY: String
USE StringUtility, ONLY: Uppercase

USE BaseType, ONLY: varopt => TypeFEVariableOpt, &
                    ipopt => TypeInterpolationOpt, &
                    polyopt => TypePolynomialOpt, &
                    QuadraturePoint_

! USE BaseMethod

IMPLICIT NONE

PRIVATE

PUBLIC :: KernelGetNSDFromID
PUBLIC :: KernelGetNSDFromName
PUBLIC :: KernelGetCoordinateSystemName
PUBLIC :: KernelGetCoordinateSystemID

PUBLIC :: TypeKernelCoordOpt
PUBLIC :: TypeKernelTimeOpt
PUBLIC :: TypeKernelProblemOpt
PUBLIC :: TypeKernelTomlOpt
PUBLIC :: TypeKernelNitscheOpt

PUBLIC :: KernelBasisOpt_
PUBLIC :: TypeKernelBasisOpt

PUBLIC :: KernelErrorOpt_
PUBLIC :: TypeKernelErrorOpt

!----------------------------------------------------------------------------
!                                                           KernelErrorOpt_
!----------------------------------------------------------------------------

TYPE :: KernelErrorOpt_
  REAL(DFP) :: atol = 1.0E-6
  !! absolute tolerance for convergence
  REAL(DFP) :: rtol = 1.0E-6
  !! relative tolerance for convergence
  REAL(DFP) :: error0 = 0.0_DFP
  !! initial error
  REAL(DFP) :: error = 0.0_DFP
  !! current error
  INTEGER(I4B) :: maxIter = 100
  !! maximum number iterations
END TYPE KernelErrorOpt_

TYPE(KernelErrorOpt_), PARAMETER :: TypeKernelErrorOpt = KernelErrorOpt_()

!----------------------------------------------------------------------------
!                                                            KernelBasisOpt_
!----------------------------------------------------------------------------

TYPE :: KernelBasisOpt_
  CHARACTER(2) :: baseContinuity = "H1"
  !! conformity of basis functions
  CHARACTER(4) :: baseInterpolation = "LAGR"
  !! interpolation of basis functions
  INTEGER(I4B) :: quadratureType = ipopt%GaussLegendre
  !! quadrature type
  INTEGER(I4B) :: basisType = polyopt%monomial
  !! basis type in case baseInterpolation is Lagrange
  INTEGER(I4B) :: ipType = ipopt%equidistance
  !! interpolation type incase baseInterpolation is Lagrange
  REAL(DFP) :: alpha = 0.0_DFP
  !! Jacobi polynomial parameter
  REAL(DFP) :: beta = 0.0_DFP
  !! Jacobi polynomial parameter
  REAL(DFP) :: lambda = 0.5_DFP
  !! Ultraspherical polynomial parameter
  CHARACTER(128) :: quadratureType_char = "GAUSSLEGENDRE"
  !! quadrature type
  TYPE(QuadraturePoint_) :: qp
  !! QuadraturePoint

  !! quadratureType_char
  !! basisType_char
  !! ipType_char
END TYPE KernelBasisOpt_

TYPE(KernelBasisOpt_), PARAMETER :: TypeKernelBasisOpt = KernelBasisOpt_()

!----------------------------------------------------------------------------
!                                                           KernelNitscheOpt_
!----------------------------------------------------------------------------

TYPE :: KernelNitscheOpt_
  INTEGER(I4B) :: sym = -1
  INTEGER(I4B) :: skewsym = 1
END TYPE KernelNitscheOpt_

TYPE(KernelNitscheOpt_), PARAMETER :: TypeKernelNitscheOpt = &
                                      KernelNitscheOpt_()

!----------------------------------------------------------------------------
!                                                         KernelTomlOpt
!----------------------------------------------------------------------------

TYPE :: KernelTomlOpt_
  CHARACTER(9) :: TOML_LINSOLVER_NAME = "linSolver"
  CHARACTER(11) :: TOML_DIRICHLET_BC_NAME = "dirichletBC"
  CHARACTER(9) :: TOML_NEUMANN_BC_NAME = "neumannBC"
  CHARACTER(12) :: TOML_POINT_SOURCE_NAME = "pointSource"
  CHARACTER(9) :: TOML_NITSCHE_BC_NAME = "nitscheBC"
  CHARACTER(13) :: TOML_SOLID_MATERIAL_NAME = "solidMaterial"
END TYPE KernelTomlOpt_

TYPE(KernelTomlOpt_), PARAMETER :: TypeKernelTomlOpt = KernelTomlOpt_()

!----------------------------------------------------------------------------
!                                                         Space dependency
!----------------------------------------------------------------------------

TYPE :: KernelCoordOpt_
  INTEGER(I4B) :: OneD_H = 1
  !! One dimensional problem in horizontal direction

  INTEGER(I4B) :: OneD_V = 2
  !! One dimensional problem in vertical direction

  INTEGER(I4B) :: TwoD = 3
  !! Two dimension problem in Cartesian coordinate

  INTEGER(I4B) :: TwoD_Axisym = 4
  !! Two dimension problem in Axis-Symmetric coordinate

  INTEGER(I4B) :: ThreeD = 5
  !! Three dimension problem in Cartesian Coordinate system

  INTEGER(I4B) :: PlaneStress = 6
  !! Two dimension plane stress problem

  INTEGER(I4B) :: PlaneStrain = 7
  !! Two dimension plane strain problem

  INTEGER(I4B) :: Cartesian = 8
  !! Cartesian coordinates

  INTEGER(I4B) :: Cylinderical = 9
  !! Cylinderical coordinates

  INTEGER(I4B) :: Spherical = 10
  !! Sperical coordinates

  INTEGER(I4B) :: default = 8
  !! Default coordinate system

  CHARACTER(9) :: default_char = "CARTESIAN"
  !! default coordinate system

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: ToNumber => coordinateSystem_ToNumber

END TYPE KernelCoordOpt_

TYPE(KernelCoordOpt_), PARAMETER :: TypeKernelCoordOpt = KernelCoordOpt_()

!----------------------------------------------------------------------------
!                                                          Time dependency
!----------------------------------------------------------------------------

TYPE :: KernelTimeOpt_
  INTEGER(I4B) :: static = 0
  !! PDE defines a Static problem

  INTEGER(I4B) :: steady = 0
  !! PDE defines a Static problem

  INTEGER(I4B) :: pseudostatic = 1
  !! PDE defines a Static problem

  INTEGER(I4B) :: transient = 2
  !! PDE defines a Transient problem

  INTEGER(I4B) :: dynamic = 2
  !! PDE defines a Transient problem

  INTEGER(I4B) :: default = 2
  !! Default time dependency

  CHARACTER(9) :: default_char = "TRANSIENT"
  !! Default time dependency

  INTEGER(I4B) :: totalTimeStep = 1
  !! Total number of time steps

  REAL(DFP) :: currentTime = 0.0
  !! Current time

  REAL(DFP) :: dt = 0.0
  !! Time step

  REAL(DFP) :: startTime = 0.0
  !! Start time

  REAL(DFP) :: endTime = 0.0
  !! End time

CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: ToNumber => timeDependency_ToNumber

END TYPE KernelTimeOpt_

TYPE(KernelTimeOpt_), PARAMETER :: TypeKernelTimeOpt = KernelTimeOpt_()

!----------------------------------------------------------------------------
!                                                              Problem type
!----------------------------------------------------------------------------

TYPE :: KernelProblemOpt_
  INTEGER(I4B) :: scalar = varopt%Scalar
  INTEGER(I4B) :: vector = varopt%Vector
  INTEGER(I4B) :: multiPhysics = varopt%Vector + 100
  INTEGER(I4B) :: NONE = -1
  INTEGER(I4B) :: default = -1
  CHARACTER(4) :: default_char = "NONE"

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: ToNumber => problemType_ToNumber

END TYPE KernelProblemOpt_

TYPE(KernelProblemOpt_), PARAMETER :: TypeKernelProblemOpt = KernelProblemOpt_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

FUNCTION coordinateSystem_ToNumber(obj, name) RESULT(ans)
  CLASS(KernelCoordOpt_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans
  TYPE(String) :: astr
  astr = Uppercase(name)
  ans = obj%Cartesian
  SELECT CASE (astr%chars())
  CASE ("ONED_H")
    ans = obj%OneD_H
  CASE ("ONED_V")
    ans = obj%OneD_V
  CASE ("TWOD")
    ans = obj%TwoD
  CASE ("TWOD_AXISYM")
    ans = obj%TwoD_Axisym
  CASE ("THREED")
    ans = obj%ThreeD
  CASE ("PLANESTRESS")
    ans = obj%PlaneStress
  CASE ("PLANESTRAIN")
    ans = obj%PlaneStrain
  CASE ("CARTESIAN")
    ans = obj%Cartesian
  CASE ("CYLINDERICAL")
    ans = obj%Cylinderical
  CASE ("SPHERICAL")
    ans = obj%Spherical
  END SELECT
  astr = ""
END FUNCTION coordinateSystem_ToNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:
! summary:  Convert time dependency to number

FUNCTION timeDependency_ToNumber(obj, name) RESULT(ans)
  CLASS(KernelTimeOpt_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans
  TYPE(String) :: astr
  astr = Uppercase(name)
  ans = TypeKernelTimeOpt%default
  SELECT CASE (astr%chars())
  CASE ("STATIC", "STEADY")
    ans = TypeKernelTimeOpt%steady
  CASE ("TRANSIENT", "DYNAMIC")
    ans = TypeKernelTimeOpt%dynamic
  CASE ("PSEUDOSTATIC")
    ans = TypeKernelTimeOpt%pseudostatic
  END SELECT
  astr = ""
END FUNCTION timeDependency_ToNumber

!----------------------------------------------------------------------------
!                                                     problemType_ToNumber
!----------------------------------------------------------------------------

FUNCTION problemType_ToNumber(obj, name) RESULT(ans)
  CLASS(KernelProblemOpt_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans

  CHARACTER(2) :: astr

  astr = Uppercase(name(1:2))

  ans = TypeKernelProblemOpt%default

  SELECT CASE (astr)
  CASE ("SC")
    ans = TypeKernelProblemOpt%scalar
  CASE ("VE")
    ans = TypeKernelProblemOpt%vector
  CASE ("MU")
    ans = TypeKernelProblemOpt%multiPhysics
  END SELECT

END FUNCTION problemType_ToNumber

!----------------------------------------------------------------------------
!                                             KernelGetNSDFromID@GetMethods
!----------------------------------------------------------------------------

FUNCTION KernelGetNSDFromID(uid) RESULT(Ans)
  INTEGER(I4B), INTENT(IN) :: uid
  INTEGER(I4B) :: ans

  SELECT CASE (uid)
  CASE (TypeKernelCoordOpt%OneD_H, TypeKernelCoordOpt%OneD_V)
    ans = 1

  CASE (TypeKernelCoordOpt%TwoD, &
        TypeKernelCoordOpt%TwoD_Axisym, &
        TypeKernelCoordOpt%PlaneStress, &
        TypeKernelCoordOpt%PlaneStrain)
    ans = 2

  CASE DEFAULT
    ans = 3

  END SELECT
END FUNCTION KernelGetNSDFromID

!----------------------------------------------------------------------------
!                                                       KernelGetNSDFromName
!----------------------------------------------------------------------------

FUNCTION KernelGetNSDFromName(name) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans

  CHARACTER(2) :: astr

  astr = Uppercase(name(1:2))

  SELECT CASE (astr)

  CASE ("1D")
    ans = 1

  CASE ("2D", "AX", "PL")
    ans = 2

  CASE DEFAULT
    ans = 3

  END SELECT
END FUNCTION KernelGetNSDFromName

!----------------------------------------------------------------------------
!                                              KernelGetCoordinateSystemName
!----------------------------------------------------------------------------

FUNCTION KernelGetCoordinateSystemName(uid) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: uid
  TYPE(String) :: ans

  SELECT CASE (uid)
  CASE (TypeKernelCoordOpt%OneD_H)
    ans = "1D_H"

  CASE (TypeKernelCoordOpt%OneD_V)
    ans = "1D_V"

  CASE (TypeKernelCoordOpt%TwoD)
    ans = "2D"

  CASE (TypeKernelCoordOpt%TwoD_Axisym)
    ans = "AXISYM"

  CASE (TypeKernelCoordOpt%PlaneStress)
    ans = "PLANE_STRESS"

  CASE (TypeKernelCoordOpt%PlaneStrain)
    ans = "PLANE_STRAIN"

  CASE (TypeKernelCoordOpt%ThreeD)
    ans = "3D"

  CASE (TypeKernelCoordOpt%Cartesian)
    ans = "CARTESTIAN"

  CASE (TypeKernelCoordOpt%Cylinderical)
    ans = "CYLINDRICAL"

  CASE (TypeKernelCoordOpt%Spherical)
    ans = "SPHERICAL"

  CASE DEFAULT
    ans = "CARTESTIAN"
  END SELECT
END FUNCTION KernelGetCoordinateSystemName

!----------------------------------------------------------------------------
!                                    KernelGetCoordinateSystemID@GetMethods
!----------------------------------------------------------------------------

FUNCTION KernelGetCoordinateSystemID(name) RESULT(Ans)
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans

  SELECT CASE (TRIM(name))
  CASE ("1D_H")
    ans = TypeKernelCoordOpt%OneD_H

  CASE ("1D_V")
    ans = TypeKernelCoordOpt%OneD_V

  CASE ("2D")
    ans = TypeKernelCoordOpt%TwoD

  CASE ("AXISYM")
    ans = TypeKernelCoordOpt%TwoD_Axisym

  CASE ("PLANE_STRAIN")
    ans = TypeKernelCoordOpt%PlaneStrain

  CASE ("PLANE_STRESS")
    ans = TypeKernelCoordOpt%PlaneStress

  CASE ("3D")
    ans = TypeKernelCoordOpt%ThreeD

  CASE ("CARTESIAN")
    ans = TypeKernelCoordOpt%Cartesian

  CASE ("CYLINDRICAL")
    ans = TypeKernelCoordOpt%Cylinderical

  CASE ("SPHERICAL")
    ans = TypeKernelCoordOpt%Spherical

  CASE DEFAULT
    ans = TypeKernelCoordOpt%default

  END SELECT
END FUNCTION KernelGetCoordinateSystemID

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractKernelParam
