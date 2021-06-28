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

SUBMODULE( Vector_Class ) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set1
  CHARACTER( LEN = * ), PARAMETER :: myName="vec_set1"

  IF( .NOT. obj%isInitiated ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Vector object is not initiated' )

  IF( obj%tSize .GE. indx ) THEN
    CALL set( obj%realVec, indx=[indx], value=[value] )
  ELSE
    CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'indx :: '// trim(str(indx, .true.)) // " is greater than tSize : "//&
    & trim( str( obj%tSize, .true. )) )
  END IF
END PROCEDURE vec_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set2
  CHARACTER( LEN = * ), PARAMETER :: myName = "vec_set2"

  IF( .NOT. obj%isInitiated ) THEN
    CALL eVector%raiseError(modName//'::'//myName// " - "// &
      & 'Vector object is not initiated' )
  ELSE
    CALL set( obj%realVec, value=value )
  END IF
END PROCEDURE vec_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set3
  CHARACTER( LEN = * ), PARAMETER :: myName="vec_set3"

  IF( .NOT. obj%isInitiated ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Vector object is not initiated' )

  IF( obj%tSize .NE. SIZE(value) ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Size of given value is not same as the size of real vector' )

  CALL set( obj%realVec, value=value )
END PROCEDURE vec_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set4
  CHARACTER( LEN = * ), PARAMETER :: myName="vec_set4"

  IF( .NOT. obj%isInitiated ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Vector object is not initiated' )

  IF( ANY( indx .GT. obj%tSize ) ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Some of the indx are out of bound, indx>tSize' )

  CALL set( obj%realVec, indx=indx, value=value )
END PROCEDURE vec_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set5
  CHARACTER( LEN = * ), PARAMETER :: myName="vec_set5"

  IF( .NOT. obj%isInitiated ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Vector object is not initiated' )

  IF( istart .GT. obj%tsize .OR. iend .GT. obj%tsize ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Index is out of bound' )

  CALL set( obj%realVec, istart=istart, iend=iend, stride=stride, &
    & value=value )
END PROCEDURE vec_set5

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set6
  CHARACTER( LEN = * ), PARAMETER :: myName="vec_set6"

  IF( .NOT. obj%isInitiated ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Vector object is not initiated' )

  IF( istart .GT. obj%tsize .OR. iend .GT. obj%tsize &
    & .OR. INT( ( iend - istart + stride )/ stride ) .GT. obj%tsize ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Index is out of bound' )

  CALL set( obj%realVec, istart=istart, iend=iend, stride=stride, &
    & value=value )
END PROCEDURE vec_set6

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set7
  CHARACTER( LEN = * ), PARAMETER :: myName="vec_set7"
  REAL( DFP ), ALLOCATABLE :: localValue( : )
  INTEGER( I4B ) :: ii, jj, kk

  IF( .NOT. obj%isInitiated ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'Vector object is not initiated' )

  IF( storageFMT .NE. FMT_DOF .AND. storageFMT .NE. FMT_NODES ) &
    & CALL eVector%raiseError(modName//'::'//myName// " - "// &
    & 'The value of storageFMT should be either ' &
    & // trim(str(FMT_DOF,.true.)) &
    & // " or " // trim( str(FMT_NODES, .true. )) )

  IF( PRESENT( dofs ) ) THEN
    ! check the size of val
    ii = SIZE( dofs ) * SIZE( nodeNum )
    IF( ii .NE. SIZE( value ) ) &
      & CALL eVector%raiseError( modName//'::'//myName// " - " &
      & // 'The size of val is not correct: needed ' &
      & // trim( str( ii, .true. ) ) &
      & // "but found : " // trim( str( SIZE( value ), .true. ) ) )

    ALLOCATE( localValue( SIZE( nodeNum ) ) )
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
      CALL set( Vec=obj%realVec%val, obj=obj%dof, Nptrs=nodeNum, &
        & Val=localValue, dofno=dofs( ii ) )
    END DO
  ELSE
    ! check the size of val
    ii = (.tdof. obj%dof) * SIZE( nodeNum )
    IF( ii .NE. SIZE( value ) ) &
      & CALL eVector%raiseError( modName//'::'//myName// " - " &
      & // 'The size of val is not correct: needed ' &
      & // trim( str( ii, .true. ) ) &
      & // "but found : " // trim( str( SIZE( value ), .true. ) ) )

    IF( storageFMT .EQ. obj%dof%storageFMT ) THEN
      CALL set(Vec=obj%realVec%val, obj=obj%dof, nptrs=nodeNum, &
        & val=value, Conversion=[NONE] )
    ELSE
      IF( storageFMT .EQ. FMT_NODES ) THEN
        CALL set(Vec=obj%realVec%val, obj=obj%dof, nptrs=nodeNum, &
          & val=value, Conversion=[NodesToDOF] )
      ELSE
        CALL set(Vec=obj%realVec%val, obj=obj%dof, nptrs=nodeNum, &
          & val=value, Conversion=[DOFtoNodes] )
      END IF
    END IF
  END IF
END PROCEDURE vec_set7

END SUBMODULE SetMethods