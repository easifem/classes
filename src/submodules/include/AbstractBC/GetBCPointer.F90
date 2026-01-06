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
!

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-06
! summary:  This macro is used in child classes of AbstractBC
!           In GetXXXBCPointer. The interface and use statement are given
!           below.

! MODULE FUNCTION obj_GetDirichletBCPointer(bc, bcNo) RESULT(ans)
!   CLASS(DirichletBCPointer_), INTENT(IN) :: bc(:)
!   INTEGER(I4B), OPTIONAL, INTENT(IN) :: bcNo
!   !! Dirichlet boundary nunber
!   CLASS(DirichletBC_), POINTER :: ans
! END FUNCTION obj_GetDirichletBCPointer

! use statement:
! USE Display_Method, ONLY: ToString
! USE InputUtility, ONLY: Input

#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: bcNo0, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(bc)

bcNo0 = Input(default=tsize, option=bcNo)

#ifdef DEBUG_VER
isok = bcNo0 .LE. tsize
CALL AssertError1(isok, myName, &
        "bcNo0="//ToString(bcNo0)//" is out of bound tsize="//ToString(tsize))
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(bc(bcNo0)%ptr)
CALL AssertError1(isok, myName, &
                  "bc("//ToString(bcNo0)//")%ptr is not ASSOCIATED")
#endif

ans => bc(bcNo0)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

