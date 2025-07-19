! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

MODULE CoordOpt_Class
USE GlobalData, ONLY: I4B, DFP, LGT

USE String_Class, ONLY: String
USE StringUtility, ONLY: Uppercase

IMPLICIT NONE

PRIVATE
PUBLIC :: CoordOpt_, TypeCoordOpt, &
          GetNSDFromID, GetNSDFromName, &
          GetCoordinateSystemName, GetCoordinateSystemID

!----------------------------------------------------------------------------
!                                                                  CoordOpt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-11
! summary:  This class contains options for coordinates

TYPE :: CoordOpt_
  INTEGER(I4B) :: oneD_H = 1
  !! One dimensional problem in horizontal direction

  INTEGER(I4B) :: oneD_V = 2
  !! One dimensional problem in vertical direction

  INTEGER(I4B) :: twoD = 3
  !! Two dimension problem in Cartesian coordinate

  INTEGER(I4B) :: twoD_Axisym = 4
  !! Two dimension problem in Axis-Symmetric coordinate

  INTEGER(I4B) :: threeD = 5
  !! Three dimension problem in Cartesian Coordinate system

  INTEGER(I4B) :: planeStress = 6
  !! Two dimension plane stress problem

  INTEGER(I4B) :: planeStrain = 7
  !! Two dimension plane strain problem

  INTEGER(I4B) :: cartesian = 8
  !! Cartesian coordinates

  INTEGER(I4B) :: cylinderical = 9
  !! Cylinderical coordinates

  INTEGER(I4B) :: spherical = 10
  !! Sperical coordinates

  INTEGER(I4B) :: default = 8
  !! Default coordinate system

  CHARACTER(9) :: default_char = "CARTESIAN"
  !! default coordinate system

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: ToNumber => obj_ToNumber

END TYPE CoordOpt_

!----------------------------------------------------------------------------
!                                                               TypeCoordOpt
!----------------------------------------------------------------------------

TYPE(CoordOpt_), PARAMETER :: TypeCoordOpt = CoordOpt_()

!----------------------------------------------------------------------------
!                                                                   Methods
!----------------------------------------------------------------------------

CONTAINS

FUNCTION obj_ToNumber(obj, name) RESULT(ans)
  CLASS(CoordOpt_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans

  ! Internal variables
  TYPE(String) :: astr

  ! main code
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
END FUNCTION obj_ToNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION GetNSDFromID(uid) RESULT(Ans)
  INTEGER(I4B), INTENT(IN) :: uid
  INTEGER(I4B) :: ans

  SELECT CASE (uid)
  CASE (TypeCoordOpt%oneD_H, TypeCoordOpt%oneD_V)
    ans = 1

  CASE (TypeCoordOpt%twoD, &
        TypeCoordOpt%twoD_Axisym, &
        TypeCoordOpt%planeStress, &
        TypeCoordOpt%planeStrain)
    ans = 2

  CASE DEFAULT
    ans = 3

  END SELECT
END FUNCTION GetNSDFromID

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION GetNSDFromName(name) RESULT(ans)
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
END FUNCTION GetNSDFromName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION GetCoordinateSystemName(uid) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: uid
  TYPE(String) :: ans

  SELECT CASE (uid)
  CASE (TypeCoordOpt%oneD_H)
    ans = "1D_H"

  CASE (TypeCoordOpt%oneD_V)
    ans = "1D_V"

  CASE (TypeCoordOpt%twoD)
    ans = "2D"

  CASE (TypeCoordOpt%twoD_Axisym)
    ans = "AXISYM"

  CASE (TypeCoordOpt%planeStress)
    ans = "PLANE_STRESS"

  CASE (TypeCoordOpt%planeStrain)
    ans = "PLANE_STRAIN"

  CASE (TypeCoordOpt%threeD)
    ans = "3D"

  CASE (TypeCoordOpt%cartesian)
    ans = "CARTESTIAN"

  CASE (TypeCoordOpt%cylinderical)
    ans = "CYLINDRICAL"

  CASE (TypeCoordOpt%spherical)
    ans = "SPHERICAL"

  CASE DEFAULT
    ans = "CARTESTIAN"
  END SELECT
END FUNCTION GetCoordinateSystemName

!----------------------------------------------------------------------------
!                                                     GetCoordinateSystemID
!----------------------------------------------------------------------------

FUNCTION GetCoordinateSystemID(name) RESULT(Ans)
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B) :: ans

  SELECT CASE (TRIM(name))
  CASE ("1D_H")
    ans = TypeCoordOpt%oneD_H

  CASE ("1D_V")
    ans = TypeCoordOpt%oneD_V

  CASE ("2D")
    ans = TypeCoordOpt%twoD

  CASE ("AXISYM")
    ans = TypeCoordOpt%twoD_Axisym

  CASE ("PLANE_STRAIN")
    ans = TypeCoordOpt%planeStrain

  CASE ("PLANE_STRESS")
    ans = TypeCoordOpt%planeStress

  CASE ("3D")
    ans = TypeCoordOpt%threeD

  CASE ("CARTESIAN")
    ans = TypeCoordOpt%cartesian

  CASE ("CYLINDRICAL")
    ans = TypeCoordOpt%cylinderical

  CASE ("SPHERICAL")
    ans = TypeCoordOpt%spherical

  CASE DEFAULT
    ans = TypeCoordOpt%default

  END SELECT
END FUNCTION GetCoordinateSystemID

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE CoordOpt_Class
