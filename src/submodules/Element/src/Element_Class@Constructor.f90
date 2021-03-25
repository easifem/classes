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
! date: 	24 March 2021
! summary: 	Constructor methods for [[Element_]]


SUBMODULE( Element_Class ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE m_Initiate_obj
  Obj%Nptrs = Nptrs
  Obj%Mat_Type = Mat_Type
  Obj%RefElem => RefElem
END PROCEDURE m_Initiate_obj


MODULE PROCEDURE m_initiate_from_obj
  Obj%Nptrs = AnotherObj%Nptrs
  Obj%MAT_Type = AnotherObj%Mat_Type
  Obj%RefElem => AnotherObj%RefElem
END PROCEDURE m_initiate_from_obj

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE s_Initiate_obj
  Obj%Nptrs = Nptrs
  Obj%Mat_Type = Mat_Type
  Obj%RefElem => RefElem
END PROCEDURE s_Initiate_obj

!----------------------------------------------------------------------------
!                                                                 Element
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL Obj%Initiate( Nptrs = Nptrs, Mat_Type = Mat_Type, RefElem = RefElem )
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                                 Element
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor2
  Obj%Mat_Type = -1
  Obj%Nptrs = [-1]
  Obj%RefElem => NULL( )
END PROCEDURE Constructor2

!----------------------------------------------------------------------------
!                                                                 Element
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor3
  CALL Obj%Initiate( AnotherObj )
END PROCEDURE Constructor3

!----------------------------------------------------------------------------
!                                                            Element_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_1
  ALLOCATE( Obj )
  CALL Obj%Initiate( Nptrs = Nptrs, Mat_Type = Mat_Type, RefElem = RefElem )
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                            Element_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_2
  ALLOCATE( Obj )
  Obj%Mat_Type = -1
  Obj%Nptrs = [-1]
  Obj%RefElem => NULL( )
END PROCEDURE Constructor_2

!----------------------------------------------------------------------------
!                                                            Element_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_3
  ALLOCATE( Obj )
  CALL Obj%Initiate( AnotherObj )
END PROCEDURE Constructor_3

!------------------------------------------------------------------------------
!                                                               DeallocateData
!------------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Data
  IF( ALLOCATED( Obj%Nptrs ) ) DEALLOCATE( Obj%Nptrs )
  Obj%MAT_Type = 0
  Obj%RefElem => NULL( )
END PROCEDURE Deallocate_Data

!----------------------------------------------------------------------------
!                                                         isBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE m_isBoundaryElement
  IF( NSD .NE. Obj%RefElem%XiDimension ) THEN
    Ans = .TRUE.
  ELSE
    Ans = .FALSE.
  END IF
END PROCEDURE m_isBoundaryElement

!----------------------------------------------------------------------------
!                                                                 getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE m_getNptrs
  IF( ALLOCATED( Obj%Nptrs ) ) THEN
    Nptrs = Obj%Nptrs
  ELSE
    ALLOCATE( Nptrs( 0 ) )
  END IF
END PROCEDURE m_getNptrs

!----------------------------------------------------------------------------
!                                                                 setNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE m_setNptrs
  Obj%Nptrs = Nptrs
END PROCEDURE m_setNptrs

!----------------------------------------------------------------------------
!                                                            setMaterialType
!----------------------------------------------------------------------------

MODULE PROCEDURE setMaterialType_1
  Obj%Mat_Type = MatType
END PROCEDURE setMaterialType_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Constructor
