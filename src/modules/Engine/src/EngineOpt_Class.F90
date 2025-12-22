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

MODULE EngineOpt_Class
IMPLICIT NONE

PRIVATE
PUBLIC :: EngineOpt_, TypeEngineOpt

!-------------------------------------------------------------------------------
!                                                                  EngineOpt_
!-------------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-14
! summary: Engine option

TYPE :: EngineOpt_
  CHARACTER(13) :: native_serial = "NATIVE_SERIAL"
  CHARACTER(10) :: native_omp = "NATIVE_OMP"
  CHARACTER(10) :: native_mpi = "NATIVE_MPI"
  CHARACTER(7) :: lis_omp = "LIS_OMP"
  CHARACTER(7) :: lis_mpi = "LIS_MPI"
  CHARACTER(9) :: petsc_omp = "PETSC_OMP"
  CHARACTER(9) :: petsc_mpi = "PETSC_MPI"
END TYPE EngineOpt_

TYPE(EngineOpt_), PARAMETER :: TypeEngineOpt = EngineOpt_()

END MODULE EngineOpt_Class
