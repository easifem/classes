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

SUBMODULE(AbstractLinSolver_Class) IOMethods
USE Display_Method, ONLY: Display
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: isok

CALL Display(msg, unitNo=unitno)
CALL Display("engine: "//obj%engine%chars(), unitNo=unitno)
CALL Display(obj%isInit, "isInitiated: ", unitNo=unitno)
CALL Display(obj%solverName, "solverName: ", unitNo=unitno)
CALL Display(obj%preconditionOption, "preconditionOption: ", unitNo=unitno)
CALL Display(obj%convergenceIn, "convergenceIn: ", unitNo=unitno)
CALL Display(obj%convergenceType, "convergenceType: ", unitNo=unitno)
CALL Display(obj%maxIter, "maxIter: ", unitNo=unitno)
CALL Display(obj%relativeToRHS, "relativeToRHS: ", unitNo=unitno)
CALL Display(obj%KrylovSubspaceSize, "KrylovSubspaceSize: ", unitNo=unitno)
CALL Display(obj%atol, "atol: ", unitNo=unitno)
CALL Display(obj%rtol, "rtol: ", unitNo=unitno)
CALL Display(obj%ierr, "ierr: ", unitNo=unitno)
CALL Display(obj%iter, "iter: ", unitNo=unitno)

isok = ALLOCATED(obj%res)
CALL Display("obj%res is ALLOCATED:", unitNo=unitno)

isok = ASSOCIATED(obj%amat)
CALL Display(isok, "amat is ASSOCIATED: ", unitNo=unitno)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
