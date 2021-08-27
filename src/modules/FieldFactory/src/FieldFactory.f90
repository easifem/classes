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

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This modules is a factory for linear solver, vector and matrix

MODULE FieldFactory
USE Field
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="FIELDFACTORY"
TYPE( ExceptionHandler_ ) :: e

!----------------------------------------------------------------------------
!                                                         LinSolverFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This function returns child of [[AbstractLinSolver_]] based on engine

INTERFACE
MODULE FUNCTION LinSolverFactory( engine ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: engine
  CLASS( AbstractLinSolver_ ), POINTER :: ans
END FUNCTION LinSolverFactory
END INTERFACE

PUBLIC :: LinSolverFactory

!----------------------------------------------------------------------------
!                                                         MatrixFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This function returns child of [[AbstractMatrixField_]] based on engine

INTERFACE
MODULE FUNCTION MatrixFieldFactory( engine ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: engine
  CLASS( AbstractMatrixField_ ), POINTER :: ans
END FUNCTION MatrixFieldFactory
END INTERFACE

PUBLIC :: MatrixFieldFactory

!----------------------------------------------------------------------------
!                                                         NodeFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This function returns child of [[AbstractNodeField_]] based on engine

INTERFACE
MODULE FUNCTION NodeFieldFactory( engine, datatype ) RESULT( Ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: engine
  CHARACTER( LEN = * ), INTENT( IN ) :: datatype
  CLASS( AbstractNodeField_ ), POINTER :: ans
END FUNCTION NodeFieldFactory
END INTERFACE

PUBLIC :: NodeFieldFactory

END MODULE FieldFactory