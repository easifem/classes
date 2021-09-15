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

MODULE VTKDataStructure_Class
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Sept 2021
! summary: This data type contains topology and geometry of the data

TYPE, ABSTRACT :: VTKDataStructure_
  LOGICAL( LGT ) :: isInitiated = .FALSE.
  LOGICAL( LGT ) :: isStructured = .FALSE.
  TYPE( String ) :: name
    !! vti, vtr, vts, vtp, vtu
    !! pvti, pvtr, pvts, pvtp, pvtu
  INTEGER( I4B ) :: xidim = 0
  CONTAINS
  PRIVATE
  ! PROCEDURE, PUBLIC, PASS( obj ) :: OpenPiece
  ! PROCEDURE, PUBLIC, PASS( obj ) :: ClosePiece

END TYPE VTKDataStructure_

PUBLIC :: VTKDataStructure_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: VTKDataStructurePointer_
  CLASS( VTKDataStructure_ ), POINTER :: ptr => NULL()
END TYPE VTKDataStructurePointer_

PUBLIC :: VTKDataStructurePointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE, EXTENDS( VTKDataStructure_ ) :: VTKImageData_
  REAL( DFP ) :: WholeExtent( 6 ) = 0.0_DFP
  REAL( DFP ) :: Origin( 3 ) = 0.0_DFP
  REAL( DFP ) :: Spacing( 3 ) = 0.0_DFP
END TYPE VTKImageData_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE, EXTENDS( VTKDataStructure_ ) :: VTKRectilinearGrid_
  REAL( DFP ) :: WholeExtent( 6 ) = 0.0_DFP
  REAL( DFP ) :: Origin( 3 ) = 0.0_DFP
END TYPE VTKRectilinearGrid_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE, EXTENDS( VTKDataStructure_ ) :: VTKStructuredGrid_
  REAL( DFP ) :: WholeExtent( 6 ) = 0.0_DFP
END TYPE VTKStructuredGrid_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE, EXTENDS( VTKDataStructure_ ) :: VTKPolyData_
END TYPE VTKPolyData_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE, EXTENDS( VTKDataStructure_ ) :: VTKUnstructuredGrid_
END TYPE VTKUnstructuredGrid_

END MODULE VTKDataStructure_Class