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

SUBMODULE(LinSolver_Class) GetMethods

USE StringUtility, ONLY: UpperCase

USE BaseType, ONLY: TypeSolverNameOpt

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                                 GetLinSolverCodeFromName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLinSolverCodeFromName
CHARACTER(:), ALLOCATABLE :: astr

astr = UpperCase(name)

SELECT CASE (astr)
CASE ("CG") !1

  ans = TypeSolverNameOpt%CG

CASE ("BICG", "BCG") !2

  ans = TypeSolverNameOpt%BICG

CASE ("CGS") !3

  ans = TypeSolverNameOpt%CGS

CASE ("BICGSTAB", "BCGSTAB") !4

  ans = TypeSolverNameOpt%BICGSTAB

CASE ("BICGSTABL", "BCGSTABL") !5

  ans = TypeSolverNameOpt%BICGSTABL

CASE ("GPBICG") !6

  ans = TypeSolverNameOpt%GPBICG

CASE ("TFQMR") !7

  ans = TypeSolverNameOpt%TFQMR

CASE ("OMN", "FOM", "ORTHOMIN") !8

  ans = TypeSolverNameOpt%OMN

CASE ("GMRES", "GMR") !9

  ans = TypeSolverNameOpt%GMRES

CASE ("JACOBI") !10

  ans = TypeSolverNameOpt%JACOBI

CASE ("GS") !11

  ans = TypeSolverNameOpt%GS

CASE ("SOR") !12

  ans = TypeSolverNameOpt%SOR

CASE ("BICGSAFE") !13

  ans = TypeSolverNameOpt%BICGSAFE

CASE ("CR") !14

  ans = TypeSolverNameOpt%CR

CASE ("BICR") !15

  ans = TypeSolverNameOpt%BICR

CASE ("CRS") !16

  ans = TypeSolverNameOpt%CRS

CASE ("BICRSTAB") !17

  ans = TypeSolverNameOpt%BICRSTAB

CASE ("GPBICR") !18

  ans = TypeSolverNameOpt%GPBICR

CASE ("BICRSAFE") !19

  ans = TypeSolverNameOpt%BICRSAFE

CASE ("FGMRES") !20

  ans = TypeSolverNameOpt%FGMRES

CASE ("IDRS") !21

  ans = TypeSolverNameOpt%IDRS

CASE ("IDR1") !22

  ans = TypeSolverNameOpt%IDR1

CASE ("MINRES") !23

  ans = TypeSolverNameOpt%MINRES

CASE ("COCG") !24

  ans = TypeSolverNameOpt%COCG

CASE ("COCR") !25

  ans = TypeSolverNameOpt%COCR

CASE ("CGNR", "CGN") !26

  ans = TypeSolverNameOpt%CGNR

CASE ("DBICG", "DBCG") !27

  ans = TypeSolverNameOpt%DBICG

CASE ("DQGMRES") !28

  ans = TypeSolverNameOpt%DQGMRES

CASE ("SUPERLU") !29

  ans = TypeSolverNameOpt%SUPERLU

END SELECT

astr = ""
END PROCEDURE obj_GetLinSolverCodeFromName

!----------------------------------------------------------------------------
!                                                 GetLinSolverNameFromCode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLinSolverNameFromCode
SELECT CASE (name)
CASE (TypeSolverNameOpt%SUPERLU)

  ans = "SUPERLU" !1

CASE (TypeSolverNameOpt%CG)

  ans = "CG" !1

CASE (TypeSolverNameOpt%BICG)

  ans = "BICG" !2

CASE (TypeSolverNameOpt%CGS)

  ans = "CGS" !3

CASE (TypeSolverNameOpt%BICGSTAB)

  ans = "BICGSTAB" !4

CASE (TypeSolverNameOpt%BICGSTABL)

  ans = "BICGSTABL" !5

CASE (TypeSolverNameOpt%GPBICG)

  ans = "GPBICG" !6

CASE (TypeSolverNameOpt%TFQMR)

  ans = "TFQMR" !7

CASE (TypeSolverNameOpt%OMN)

  ans = "ORTHOMIN" !8

CASE (TypeSolverNameOpt%GMRES)

  ans = "GMRES" !9

CASE (TypeSolverNameOpt%JACOBI)

  ans = "JACOBI" !10

CASE (TypeSolverNameOpt%GS)

  ans = "GS" !11

CASE (TypeSolverNameOpt%SOR)

  ans = "SOR" !12

CASE (TypeSolverNameOpt%BICGSAFE)

  ans = "BICGSAFE" !13

CASE (TypeSolverNameOpt%CR)

  ans = "CR" !14

CASE (TypeSolverNameOpt%BICR)

  ans = "BICR" !15

CASE (TypeSolverNameOpt%CRS)

  ans = "CRS" !16

CASE (TypeSolverNameOpt%BICRSTAB)

  ans = "BICRSTAB" !17

CASE (TypeSolverNameOpt%GPBICR)

  ans = "GPBICR" !18

CASE (TypeSolverNameOpt%BICRSAFE)

  ans = "BICRSAFE" !19

CASE (TypeSolverNameOpt%FGMRES)

  ans = "FGMRES" !20

CASE (TypeSolverNameOpt%IDRS)

  ans = "IDRS" !21

CASE (TypeSolverNameOpt%IDR1)

  ans = "IDR1" !22

CASE (TypeSolverNameOpt%MINRES)

  ans = "MINRES" !23

CASE (TypeSolverNameOpt%COCG)

  ans = "COCG" !24

CASE (TypeSolverNameOpt%COCR)

  ans = "COCR" !25

CASE (TypeSolverNameOpt%CGNR)

  ans = "CGNR" !26

CASE (TypeSolverNameOpt%DBICG)

  ans = "DBICG" !27

CASE (TypeSolverNameOpt%DQGMRES)

  ans = "DQGMRES" !28

END SELECT

END PROCEDURE obj_GetLinSolverNameFromCode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
