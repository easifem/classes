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

SUBMODULE(Vector_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set1
  CHARACTER( LEN = * ), PARAMETER :: myName="vec_set1"
  !!
#ifdef DEBUG_VER
  !!
  IF( .NOT. obj%isInitiated ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Vector object is not initiated' )
  !!
  IF( obj%tSize .GE. nodenum ) THEN
#endif
  !!
  CALL set( obj%realVec, nodenum=[nodenum], value=[value] )
  !!
#ifdef DEBUG_VER
  ELSE
    CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'nodenum :: '// trim(str(nodenum, .true.)) // " is greater than tSize : "//&
    & trim( str( obj%tSize, .true. )) )
  END IF
#endif
  !!
END PROCEDURE vec_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set2
  CHARACTER( LEN = * ), PARAMETER :: myName = "vec_set2"
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) THEN
    !!
    CALL eVector%raiseError(modName//'::'//myName// " - "// &
      & 'Vector object is not initiated' )
    !!
  END IF
#endif
  !!
  CALL set( obj%realVec, value=value )
  !!
END PROCEDURE vec_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set3
  CHARACTER( LEN = * ), PARAMETER :: myName="vec_set3"
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Vector object is not initiated' )
  !!
  IF( obj%tSize .NE. SIZE(value) ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Size of given value is not same as the size of real vector' )
#endif
  !!
  CALL set( obj%realVec, value=value )
  !!
END PROCEDURE vec_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set4
  CHARACTER( LEN = * ), PARAMETER :: myName="vec_set4"
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Vector object is not initiated' )
  !!
  IF( ANY( nodenum .GT. obj%tSize ) ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Some of the nodenum are out of bound, nodenum>tSize' )
  !!
#endif
  !!
  CALL set( obj%realVec, nodenum=nodenum, value=value )
  !!
END PROCEDURE vec_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set5
  CHARACTER( LEN = * ), PARAMETER :: myName="vec_set5"
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Vector object is not initiated' )
  !!
  IF( istart .GT. obj%tsize .OR. iend .GT. obj%tsize ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Index is out of bound' )
#endif
  !!
  CALL set( obj%realVec, istart=istart, iend=iend, stride=stride, &
    & value=value )
  !!
END PROCEDURE vec_set5

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set6
  CHARACTER( LEN = * ), PARAMETER :: myName="vec_set6"
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Vector object is not initiated' )
  !!
  IF( istart .GT. obj%tsize .OR. iend .GT. obj%tsize &
    & .OR. INT( ( iend - istart + stride )/ stride ) .GT. obj%tsize ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Index is out of bound' )
#endif
  !!
  CALL set( obj%realVec, istart=istart, iend=iend, stride=stride, &
    & value=value )
  !!
END PROCEDURE vec_set6

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set7
  CHARACTER( LEN = * ), PARAMETER :: myName="vec_set7"
  REAL( DFP ), ALLOCATABLE :: localValue( : )
  INTEGER( I4B ) :: ii, jj, kk
  !!
#ifdef DEBUG_VER
  IF( .NOT. obj%isInitiated ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Vector object is not initiated' )
  !!
  IF( storageFMT .NE. FMT_DOF .AND. storageFMT .NE. FMT_NODES ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'The value of storageFMT should be either ' &
    & // trim(str(FMT_DOF,.true.)) &
    & // " or " // trim( str(FMT_NODES, .true. )) )
#endif
  !!
  IF( PRESENT( dofs ) ) THEN
    ! check the size of val
    ii = SIZE( dofs ) * SIZE( nodeNum )
    !!
#ifdef DEBUG_VER
    IF( ii .NE. SIZE( value ) ) &
      & CALL eVector%raiseError( modName//'::'//myName// " - " &
      & // 'The size of val is not correct: needed ' &
      & // trim( str( ii, .true. ) ) &
      & // "but found : " // trim( str( SIZE( value ), .true. ) ) )
#endif
    !!
    ALLOCATE( localValue( SIZE( nodeNum ) ) )
    !!
    DO ii = 1, SIZE( dofs )
      IF( storageFMT .EQ. FMT_DOF ) THEN
        DO jj = 1, SIZE( nodeNum )
          kk = ( ii - 1 ) * SIZE( nodeNum ) + jj
          localValue( jj ) = value( kk )
        END DO
      ELSE
        DO jj = 1, SIZE( nodeNum )
          kk = ( jj - 1 ) * SIZE( dofs ) + ii
          localValue( jj ) = value( kk )
        END DO
      END IF
      CALL set( Vec=obj%realVec%val, &
        & obj=obj%dof, &
        & nodenum=nodeNum, &
        & value=localValue, &
        & idof=dofs( ii ) )
    END DO
    !!
  ELSE
    !! check the size of val
    ii = (.tdof. obj%dof) * SIZE( nodeNum )
    !!
#ifdef DEBUG_VER
    IF( ii .NE. SIZE( value ) ) &
      & CALL eVector%raiseError( modName//'::'//myName// " - " &
      & // 'The size of val is not correct: needed ' &
      & // trim( str( ii, .true. ) ) &
      & // "but found : " // trim( str( SIZE( value ), .true. ) ) )
#endif
    !!
    IF( storageFMT .EQ. obj%dof%storageFMT ) THEN
      CALL set(Vec=obj%realVec%val, obj=obj%dof, nodenum=nodeNum, &
        & value=value, Conversion=[NONE] )
    ELSE
      IF( storageFMT .EQ. FMT_NODES ) THEN
        CALL set(Vec=obj%realVec%val, obj=obj%dof, nodenum=nodeNum, &
          & value=value, Conversion=[NodesToDOF] )
      ELSE
        CALL set(Vec=obj%realVec%val, obj=obj%dof, nodenum=nodeNum, &
          & value=value, Conversion=[DOFtoNodes] )
      END IF
    END IF
    !!
  END IF
  !!
END PROCEDURE vec_set7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------



END SUBMODULE SetMethods