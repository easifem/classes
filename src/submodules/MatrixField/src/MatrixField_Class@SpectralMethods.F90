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

SUBMODULE(MatrixField_Class) SpectralMethods
USE CSRMatrix_Method, ONLY: SymSchurLargestEigenVal, &
                            SymLargestEigenVal

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SymSchurLargestEigenVal
CHARACTER(*), PARAMETER :: myName = "obj_SymSchurLargestEigenVal()"

SELECT TYPE (B); CLASS IS (MatrixField_)
  ans = SymSchurLargestEigenVal(A=obj%mat, B=B%mat, nev=nev, which=which, &
                                NCV=NCV, maxIter=maxIter, tol=tol)

CLASS DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for type of B')
  RETURN

END SELECT
END PROCEDURE obj_SymSchurLargestEigenVal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SymLargestEigenVal
ans = SymLargestEigenVal(mat=obj%mat, nev=nev, which=which, &
                         NCV=NCV, maxIter=maxIter, tol=tol)
END PROCEDURE obj_SymLargestEigenVal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SpectralMethods
