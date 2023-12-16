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
USE GlobalData
USE String_Class
USE BaseMethod
IMPLICIT NONE
PRIVATE

PUBLIC :: BasisType_ToInteger
PUBLIC :: KernelGetNSDFromID
PUBLIC :: KernelGetNSDFromName
PUBLIC :: KernelGetCoordinateSystemName
PUBLIC :: KernelGetCoordinateSystemID

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CHARACTER(*), PUBLIC, PARAMETER :: TOML_LINSOLVER_NAME = "linSolver"
CHARACTER(*), PUBLIC, PARAMETER :: TOML_DIRICHLET_BC_NAME = "dirichletBC"
CHARACTER(*), PUBLIC, PARAMETER :: TOML_NEUMANN_BC_NAME = "neumannBC"
CHARACTER(*), PUBLIC, PARAMETER :: TOML_NITSCHE_BC_NAME = "nitscheBC"
CHARACTER(*), PUBLIC, PARAMETER :: TOML_SOLID_MATERIAL_NAME = "solidMaterial"
!! Default value of linSolver in toml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER(I4B), PUBLIC, PARAMETER :: Nitsche_Sym = -1
!! Sym or unsymmetric Nitsche formulation
INTEGER(I4B), PUBLIC, PARAMETER :: Nitsche_SkewSym = 1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_TANMAT_PROP = "UNSYM"
!! Default tangent matrix properties
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_OUTPUT_PATH = "./results/"
!! Default outputpath
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_engine = "NATIVE_SERIAL"
!! Default value of engine
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_nitscheAlpha = 100.0
!! Alpha for Nitsche boundary
LOGICAL(LGT), PUBLIC, PARAMETER :: DEFAULT_isSymNitsche = .TRUE.
!! Default value of symmetric Nitsche formulation
LOGICAL(LGT), PUBLIC, PARAMETER :: DEFAULT_isConstantMatProp = .TRUE.
!! Default value of constant material property
INTEGER(I4B), PARAMETER, PUBLIC :: DEFAULT_algorithm = 1_I4B
!! Default value of algorithm
LOGICAL(LGT), PUBLIC, PARAMETER :: DEFAULT_isIsotropic = .TRUE.
!! Default value for isotropicicity
LOGICAL(LGT), PUBLIC, PARAMETER :: DEFAULT_isIncompressible = .FALSE.
!! Default value for incompressibility
REAL(DFP), PARAMETER, PUBLIC :: DEFAULT_atoleranceForDisplacement = 1.0E-6
!! Default absolute tolerance for displacement
REAL(DFP), PARAMETER, PUBLIC :: DEFAULT_rtoleranceForDisplacement = 1.0E-6
!! Default relative tolerance for displacement
REAL(DFP), PARAMETER, PUBLIC :: DEFAULT_atoleranceForVelocity = 1.0E-6
!! Default absolute tolerance for velocity
REAL(DFP), PARAMETER, PUBLIC :: DEFAULT_rtoleranceForVelocity = 1.0E-6
!! Default relative tolerance for velocity
REAL(DFP), PARAMETER, PUBLIC :: DEFAULT_atoleranceForResidual = 1.0E-6
!! Default absolute tolerance for residual
REAL(DFP), PARAMETER, PUBLIC :: DEFAULT_rtoleranceForResidual = 1.0E-6
!! Default relative tolerance for residual
INTEGER(I4B), PARAMETER, PUBLIC :: DEFAULT_tOverlappedMaterials = 1_I4B
!! Total number of overlapped materials

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTEGER(I4B), PUBLIC, PARAMETER :: DEFAULT_NNT = 1_I4B
INTEGER(I4B), PUBLIC, PARAMETER :: DEFAULT_NSD = 0_I4B
INTEGER(I4B), PUBLIC, PARAMETER :: DEFAULT_postProcessOpt = 1_I4B
INTEGER(I4B), PUBLIC, PARAMETER :: DEFAULT_tdof = 0_I4B
LOGICAL(LGT), PUBLIC, PARAMETER :: DEFAULT_isCommonDomain = .TRUE.
INTEGER(I4B), PUBLIC, PARAMETER :: DEFAULT_maxIter = 100
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_gravity(3) = 0.0_DFP

!----------------------------------------------------------------------------
!                                                         Space dependency
!----------------------------------------------------------------------------

INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_1D_H = 1
  !! One dimensional problem in horizontal direction
INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_1D_V = 2
  !! One dimensional problem in vertical direction
INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_2D = 3
  !! Two dimension problem in Cartesian coordinate
INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_2D_AXISYM = 4
  !! Two dimension problem in Axis-Symmetric coordinate
INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_3D = 5
  !! Three dimension problem in Cartesian Coordinate system
INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_PLANE_STRESS = 6
  !! Two dimension plane stress problem
INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_PLANE_STRAIN = 7
  !! Two dimension plane strain problem
INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_CARTESIAN = 8
  !! Cartesian coordinates
INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_CYLINDRICAL = 9
  !! Cylinderical coordinates
INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_SPHERICAL = 10
  !! Sperical coordinates
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_CoordinateSystem_char = "Cartesian"
INTEGER(I4B), PUBLIC, PARAMETER :: DEFAULT_CoordinateSystem = KERNEL_CARTESIAN

TYPE :: KernelCoordinateSystem_
  INTEGER(I4B) :: OneD_H = KERNEL_1D_H
  INTEGER(I4B) :: OneD_V = KERNEL_1D_V
  INTEGER(I4B) :: TwoD = KERNEL_2D
  INTEGER(I4B) :: TwoD_Axisym = KERNEL_2D_AXISYM
  INTEGER(I4B) :: ThreeD = KERNEL_3D
  INTEGER(I4B) :: PlaneStress = KERNEL_PLANE_STRESS
  INTEGER(I4B) :: PlaneStrain = KERNEL_PLANE_STRAIN
  INTEGER(I4B) :: Cartesian = KERNEL_CARTESIAN
  INTEGER(I4B) :: Cylinderical = KERNEL_CYLINDRICAL
  INTEGER(I4B) :: Spherical = KERNEL_SPHERICAL
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: ToNumber => coordinateSystem_ToNumber
END TYPE KernelCoordinateSystem_

TYPE(KernelCoordinateSystem_), PUBLIC, PARAMETER :: &
  & KernelCoordinateSystem = KernelCoordinateSystem_()

!----------------------------------------------------------------------------
!                                                          Time dependency
!----------------------------------------------------------------------------

INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_STATIC = 0
  !! PDE defines a Static problem
INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_STEADY = KERNEL_STATIC
  !! PDE defines a Static problem
INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_PSEUDOSTATIC = 1
  !! PDE defines a Static problem
INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_TRANSIENT = 2
  !! PDE defines a Transient problem
INTEGER(I4B), PUBLIC, PARAMETER :: KERNEL_DYNAMIC = KERNEL_TRANSIENT
  !! PDE defines a Transient problem
INTEGER(I4B), PUBLIC, PARAMETER :: DEFAULT_TimeDependency = KERNEL_TRANSIENT
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_TimeDependency_char = "TRANSIENT"
INTEGER(I4B), PUBLIC, PARAMETER :: DEFAULT_TotalTimeStep = 1_I4B
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_currentTime = 0.0_DFP
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_dt = 0.0_DFP
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_startTime = 0.0_DFP
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_endTime = 0.0_DFP

TYPE :: KernelTimeDependency_
  INTEGER(I4B) :: static = KERNEL_STATIC
  INTEGER(I4B) :: steady = KERNEL_STEADY
  INTEGER(I4B) :: pseudostatic = KERNEL_PSEUDOSTATIC
  INTEGER(I4B) :: transient = KERNEL_TRANSIENT
  INTEGER(I4B) :: dynamic = KERNEL_DYNAMIC
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: ToNumber => timeDependency_ToNumber
END TYPE KernelTimeDependency_

TYPE(KernelTimeDependency_), PARAMETER, PUBLIC :: KernelTimeDependency = &
  & KernelTimeDependency_()

!----------------------------------------------------------------------------
!                                                       Problem type
!----------------------------------------------------------------------------

INTEGER(I4B), PARAMETER :: KERNEL_TYPE_SCALAR = Scalar
INTEGER(I4B), PARAMETER :: KERNEL_TYPE_VECTOR = Vector
INTEGER(I4B), PARAMETER :: KERNEL_TYPE_MULTI_PHYSICS = Vector + 100
INTEGER(I4B), PUBLIC, PARAMETER :: DEFAULT_PROBLEM_TYPE = -1_I4B
INTEGER(I4B), PUBLIC, PARAMETER :: PROBLEM_TYPE_NONE = -1_I4B
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_PROBLEM_TYPE_CHAR = "NONE"

TYPE :: KernelProblemType_
  INTEGER(I4B) :: scalar = KERNEL_TYPE_SCALAR
  INTEGER(I4B) :: vector = KERNEL_TYPE_VECTOR
  INTEGER(I4B) :: multiPhysics = KERNEL_TYPE_MULTI_PHYSICS
  INTEGER(I4B) :: NONE = PROBLEM_TYPE_NONE
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: ToNumber => problemType_ToNumber
END TYPE KernelProblemType_

TYPE(KernelProblemType_), PARAMETER, PUBLIC :: KernelProblemType =  &
  & KernelProblemType_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_baseContinuityForSpace = "H1"
  !! Default continuity type for basis functions in space
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_baseInterpolationForSpace = &
  & "LagrangeInterpolation"
  !! Default interpolation functions for basis functions in space
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_quadratureTypeForSpace = &
  & "GaussLegendre"
  !! Default quadrature type for space
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_baseContinuityForPressure = &
  & "H1"
  !! Default continuity type for basis functions for pressure field
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_baseInterpolationForPressure =&
  & "LagrangeInterpolation"
  !! Default interolation functions for basis functions in pressure
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_quadratureTypeForPressure = &
  & "GaussLegendre"
  !! Default quadrature type for pressure
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_baseContinuityForVelocity = &
  & "H1"
  !! Default continuity type for basis functions for velocity
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_baseInterpolationForVelocity =&
  & "LagrangeInterpolation"
  !! Default interpolation type for basis function for velocity
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_quadratureTypeForVelocity = &
  & "GaussLegendre"
  !! Default quadrature type for velocity
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_baseContinuityForTime = "H1"
  !! Default quadrature type for time
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_baseInterpolationForTime = &
  & "LagrangeInterpolation"
  !! Default interpolation type for basis function in time
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_quadratureTypeForTime = &
  & "GaussLegendre"

INTEGER(I4B), PUBLIC, PARAMETER :: DEFAULT_ipTypeForSpace = Equidistance
INTEGER(I4B), PUBLIC, PARAMETER :: DEFAULT_ipTypeForTime = Equidistance
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_ipTypeForSpace_char = "Equidistance"
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_ipTypeForTime_char = "Equidistance"
INTEGER(I4B), PUBLIC, PARAMETER :: DEFAULT_basisTypeForSpace = Monomial
INTEGER(I4B), PUBLIC, PARAMETER :: DEFAULT_basisTypeForTime = Monomial
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_basisTypeForSpace_char = "Monomial"
CHARACTER(*), PUBLIC, PARAMETER :: DEFAULT_basisTypeForTime_char = "Monomial"
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_alphaForSpace = 0.0_DFP
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_betaForSpace = 0.0_DFP
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_lambdaForSpace = 0.5_DFP
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_alphaForTime = 0.0_DFP
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_betaForTime = 0.0_DFP
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_lambdaForTime = 0.5_DFP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

FUNCTION coordinateSystem_ToNumber(obj, name) RESULT(ans)
  CLASS(KernelCoordinateSystem_), INTENT(IN) :: obj
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
  CLASS(KernelTimeDependency_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans
  TYPE(String) :: astr
  astr = Uppercase(name)
  ans = DEFAULT_TimeDependency
  SELECT CASE (astr%chars())
  CASE ("STATIC", "STEADY")
    ans = obj%steady
  CASE ("TRANSIENT", "DYNAMIC")
    ans = obj%dynamic
  CASE ("PSEUDOSTATIC")
    ans = obj%pseudostatic
  END SELECT
  astr = ""
END FUNCTION timeDependency_ToNumber

!----------------------------------------------------------------------------
!                                                     problemType_ToNumber
!----------------------------------------------------------------------------

FUNCTION problemType_ToNumber(obj, name) RESULT(ans)
  CLASS(KernelProblemType_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans
  CHARACTER(:), ALLOCATABLE :: astr
  astr = Uppercase(name)
  ans = DEFAULT_PROBLEM_TYPE
  SELECT CASE (astr)
  CASE ("SCALAR")
    ans = obj%scalar
  CASE ("VECTOR")
    ans = obj%vector
  CASE ("MULTIPHYSICS")
    ans = obj%multiPhysics
  END SELECT
  astr = ""
END FUNCTION problemType_ToNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-02
! summary:  Convert basis type to integer

FUNCTION BasisType_ToInteger(name) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans

  TYPE(String) :: astr

  astr = Uppercase(name)
  ans = Monomial

  SELECT CASE (astr%chars())
  CASE ("MONOMIAL")
    ans = Monomial
  CASE ("JACOBI")
    ans = Jacobi
  CASE ("LEGENDRE")
    ans = Legendre
  CASE ("CHEBYSHEV")
    ans = Chebyshev
  CASE ("ULTRASPHERICAL")
    ans = Ultraspherical
  CASE ("LOBATTO")
    ans = Lobatto
  CASE ("LAGRANGE")
    ans = Lagrange
  CASE ("HIERACHICAL", "HEIRARCHICAL")
    ans = Hierarchical
  CASE ("ORTHOGONAL")
    ans = Orthogonal
  CASE ("UNSCALEDLOBATTO")
    ans = UnscaledLobatto
  CASE ("HERMIT")
    ans = HermitPolynomial
  CASE ("SERENDIPITY")
    ans = Serendipity
  END SELECT
END FUNCTION

!----------------------------------------------------------------------------
!                                             KernelGetNSDFromID@GetMethods
!----------------------------------------------------------------------------

FUNCTION KernelGetNSDFromID(uid) RESULT(Ans)
  INTEGER(I4B), INTENT(IN) :: uid
  INTEGER(I4B) :: ans

  SELECT CASE (uid)
  CASE (KERNEL_1D_H, KERNEL_1D_V)
    ans = 1
  CASE (KERNEL_2D, KERNEL_2D_AXISYM, KERNEL_PLANE_STRAIN, &
    & KERNEL_PLANE_STRESS)
    ans = 2
  CASE DEFAULT
    ans = 3
  END SELECT
END FUNCTION KernelGetNSDFromID

!----------------------------------------------------------------------------
!                                                       KernelGetNSDFromName
!----------------------------------------------------------------------------

FUNCTION KernelGetNSDFromName(name) RESULT(Ans)
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans

  SELECT CASE (TRIM(name))
  CASE ("1D_H", "1D_V")
    ans = 1
  CASE ("2D", "AXISYM", "PLANE_STRAIN", "PLANE_STRESS")
    ans = 2
  CASE DEFAULT
    ans = 3
  END SELECT
END FUNCTION KernelGetNSDFromName

!----------------------------------------------------------------------------
!                                              KernelGetCoordinateSystemName
!----------------------------------------------------------------------------

FUNCTION KernelGetCoordinateSystemName(uid) RESULT(Ans)
  INTEGER(I4B), INTENT(IN) :: uid
  TYPE(String) :: ans

  SELECT CASE (uid)
  CASE (KERNEL_1D_H)
    ans = "1D_H"
  CASE (KERNEL_1D_V)
    ans = "1D_V"
  CASE (KERNEL_2D)
    ans = "2D"
  CASE (KERNEL_2D_AXISYM)
    ans = "AXISYM"
  CASE (KERNEL_PLANE_STRAIN)
    ans = "PLANE_STRAIN"
  CASE (KERNEL_PLANE_STRESS)
    ans = "PLANE_STRESS"
  CASE (KERNEL_3D)
    ans = "3D"
  CASE (KERNEL_CARTESIAN)
    ans = "CARTESTIAN"
  CASE (KERNEL_CYLINDRICAL)
    ans = "CYLINDRICAL"
  CASE (KERNEL_SPHERICAL)
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
    ans = KERNEL_1D_H
  CASE ("1D_V")
    ans = KERNEL_1D_V
  CASE ("2D")
    ans = KERNEL_2D
  CASE ("AXISYM")
    ans = KERNEL_2D_AXISYM
  CASE ("PLANE_STRAIN")
    ans = KERNEL_PLANE_STRAIN
  CASE ("PLANE_STRESS")
    ans = KERNEL_PLANE_STRESS
  CASE ("3D")
    ans = KERNEL_3D
  CASE ("CARTESIAN")
    ans = KERNEL_CARTESIAN
  CASE ("CYLINDRICAL")
    ans = KERNEL_CYLINDRICAL
  CASE ("SPHERICAL")
    ans = KERNEL_SPHERICAL
  CASE DEFAULT
    ans = KERNEL_CARTESIAN
  END SELECT
END FUNCTION KernelGetCoordinateSystemID

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractKernelParam
