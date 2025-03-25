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

SUBMODULE(Abstract1DFEM_Class) GetMethods

USE ProductUtility, ONLY: OuterProd_, OTimesTilda
USE InputUtility

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                     GetMs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMs()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForSpace%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForSpace%nips
  scale = obj%elemsdForSpace%ws(ips) * obj%elemsdForSpace%thickness(ips)
  CALL OuterProd_(a=obj%elemsdForSpace%N(1:nrow, ips), &
                  b=obj%elemsdForSpace%N(1:ncol, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMs

!----------------------------------------------------------------------------
!                                                                    GetKs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetKs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetKs()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForSpace%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForSpace%nips
  scale = obj%elemsdForSpace%ws(ips) * obj%elemsdForSpace%thickness(ips)
  CALL OuterProd_(a=obj%elemsdForSpace%dNdXi(1:nrow, 1, ips), &
                  b=obj%elemsdForSpace%dNdXi(1:ncol, 1, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetKs

!----------------------------------------------------------------------------
!                                                                     GetCs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCs()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForSpace%nns
ncol = nrow
ans(1:nrow, 1:ncol) = alpha * obj%ks(1:nrow, 1:ncol) &
                      + beta * obj%ms(1:nrow, 1:ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetCs

!----------------------------------------------------------------------------
!                                                           GetConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 0

DO ii = obj%conIA(spaceElemNum), obj%conIA(spaceElemNum + 1) - 1
  tsize = tsize + 1
  ans(tsize) = obj%conJA(ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                              GetBodyForce
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBodyForce
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBodyForce()"
#endif

INTEGER(I4B) :: nns, nips, ii, ips
REAL(DFP) :: r(10), args(2), js
LOGICAL(LGT) :: isBodyForce

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isBodyForce = ASSOCIATED(obj%bodyForce)
IF (.NOT. isBodyForce) RETURN

nns = obj%elemsdForSpace%nns
nips = obj%elemsdForSpace%nips
tsize = nns
ans(1:tsize) = ans(1:tsize) * anscoeff

js = obj%spaceElemLength(spaceElemNum) * 0.50_DFP

args(2) = time

DO ips = 1, nips

  args(1) = obj%elemsdForSpace%coord(1, ips)

  CALL obj%bodyForce%Get(val=r(1), args=args)

  r(2) = obj%elemsdForSpace%ws(ips) * js

  r(3) = r(1) * r(2)

  DO ii = 1, nns

    r(4) = obj%elemsdForSpace%N(ii, ips)

    r(5) = scale * r(3) * r(4)

    ans(ii) = ans(ii) + r(5)

  END DO

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetBodyForce

!----------------------------------------------------------------------------
!                                                            GetTractionLeft
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTractionLeft
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTractionLeft()"
#endif

INTEGER(I4B) :: nns, ii
REAL(DFP) :: r(10), args(1)
LOGICAL(LGT) :: isTractionLeft

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isTractionLeft = ASSOCIATED(obj%tractionLeft)
IF (.NOT. isTractionLeft) RETURN

nns = obj%elemsdForSpace%nns
tsize = nns
ans(1:tsize) = ans(1:tsize) * anscoeff

args(1) = Input(default=zero, option=time)

CALL obj%tractionLeft%Get(val=r(1), args=args)

DO ii = 1, nns

  r(2) = obj%spaceShapeFuncBndy(ii, 1)
  r(3) = scale * r(2) * r(1)

  ans(ii) = ans(ii) + r(3)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTractionLeft

!----------------------------------------------------------------------------
!                                                          GetTractionRight
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTractionRight
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTractionRight()"
#endif

INTEGER(I4B) :: nns, ii
REAL(DFP) :: r(10), args(1)
LOGICAL(LGT) :: isTractionRight

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isTractionRight = ASSOCIATED(obj%tractionRight)
IF (.NOT. isTractionRight) RETURN

nns = obj%elemsdForSpace%nns
tsize = nns
ans(1:tsize) = ans(1:tsize) * anscoeff

args(1) = Input(default=zero, option=time)

CALL obj%tractionRight%Get(val=r(1), args=args)

DO ii = 1, nns

  r(2) = obj%spaceShapeFuncBndy(ii, 2)
  r(3) = scale * r(2) * r(1)

  ans(ii) = ans(ii) + r(3)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTractionRight

END SUBMODULE GetMethods
