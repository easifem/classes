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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CHARACTER(*), PUBLIC, PARAMETER :: toml_linsolver_name = "linSolver"

!----------------------------------------------------------------------------
!
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

TYPE :: coordinateSystem_
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
END TYPE coordinateSystem_

TYPE(coordinateSystem_), PUBLIC, PARAMETER :: typeCoordinateSystem =  &
  & coordinateSystem_()

!----------------------------------------------------------------------------
!
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

TYPE :: timeDependency_
  INTEGER(I4B) :: static = KERNEL_STATIC
  INTEGER(I4B) :: steady = KERNEL_STEADY
  INTEGER(I4B) :: pseudostatic = KERNEL_PSEUDOSTATIC
  INTEGER(I4B) :: transient = KERNEL_TRANSIENT
  INTEGER(I4B) :: dynamic = KERNEL_DYNAMIC
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: ToNumber => timeDependency_ToNumber
END TYPE timeDependency_

TYPE(timeDependency_), PARAMETER, PUBLIC :: typeTimeDependency = &
  & timeDependency_()

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
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_lambdaForSpace = 0.0_DFP
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_alphaForTime = 0.0_DFP
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_betaForTime = 0.0_DFP
REAL(DFP), PUBLIC, PARAMETER :: DEFAULT_lambdaForTime = 0.0_DFP

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
!
!----------------------------------------------------------------------------

CONTAINS

FUNCTION coordinateSystem_ToNumber(obj, name) RESULT(ans)
  CLASS(coordinateSystem_), INTENT(IN) :: obj
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
  CLASS(timeDependency_), INTENT(IN) :: obj
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

END MODULE AbstractKernelParam
