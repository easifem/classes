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

SUBMODULE(Abstract1DFEM_Class) SolveMethods

USE GlobalData, ONLY: LIS_GMRES, &
                      CHAR_SLASH

USE CSRMatrix_Method, ONLY: CSRmatrix_Size => Size, &
                            CSRMatrix_LinSolve => CSRMatrix_GMRES, &
                            CSRMatrixLinSolveInitiate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                      Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Solve
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Solve()"
#endif

INTEGER(I4B) :: n, solverName

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

n = CSRMatrix_Size(obj%tanmat, 1)
solverName = LIS_GMRES

CALL CSRMatrixLinSolveInitiate(ipar=obj%ipar, fpar=obj%fpar, W=obj%work, &
                               n=n, solverName=solverName)

CALL CSRMatrix_LinSolve(obj=obj%tanmat, sol=obj%sol%val(1:n), &
               rhs=obj%rhs%val(1:n), ipar=obj%ipar, fpar=obj%fpar, W=obj%work)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Solve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SolveMethods
