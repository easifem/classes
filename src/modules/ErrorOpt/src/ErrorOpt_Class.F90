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

MODULE ErrorOpt_Class
USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: ErrorOpt_, TypeErrorOpt

!----------------------------------------------------------------------------
!                                                            ErrorOpt_Class
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-11
! summary: This class contains data related to error

TYPE :: ErrorOpt_
  REAL(DFP) :: atol = 1.0E-6
  !! absolute tolerance for convergence
  REAL(DFP) :: rtol = 1.0E-6
  !! relative tolerance for convergence
  REAL(DFP) :: error0 = 0.0_DFP
  !! initial error
  REAL(DFP) :: error = 0.0_DFP
  !! current error
  INTEGER(I4B) :: maxIter = 100
  !! maximum number iterations
END TYPE ErrorOpt_

TYPE(ErrorOpt_), PARAMETER :: TypeErrorOpt = ErrorOpt_()

END MODULE ErrorOpt_Class
