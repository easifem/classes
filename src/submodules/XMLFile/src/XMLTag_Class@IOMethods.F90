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

SUBMODULE(XMLTag_Class) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_export
  CHARACTER( LEN=16 ) :: sint
  INTEGER( I4B ) :: i,ierr,nspace
  TYPE( String ) :: fmt,tmpTag
  !>
  IF( obj%name%len_trim() > 0 ) THEN
    nspace=0
    IF(PRESENT(nindent)) nspace=2*nindent
    IF(ASSOCIATED(obj%children)) THEN
      !start tag
      tmpTag='<'//obj%name
      !Add attributes
      DO i=1,obj%tAttributes
        tmpTag=tmpTag//' '//obj%attrNames(i)//'="'// &
            obj%attrValues(i)//'"'
      END DO
      tmpTag=tmpTag//'>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unitNo,FMT=trim(fmt%chars()),IOSTAT=ierr) trim(tmpTag%chars())
      !children
      DO i=1,SIZE(obj%children)
        CALL obj%children(i)%export(unitNo,nspace/2+1)
      ENDDO
      !end tag
      tmpTag='</'//obj%name//'>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unitNo,FMT=trim(fmt%chars()),IOSTAT=ierr) trim(tmpTag%chars())
    ELSEIF(obj%content%len() > 0) THEN
      !start tag
      tmpTag='<'//obj%name
      !Add attributes
      DO i=1,obj%tAttributes
        tmpTag=tmpTag//' '//obj%attrNames(i)//'="'// &
          & obj%attrValues(i)//'"'
      END DO
      tmpTag=tmpTag//'>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unitNo,FMT=trim(fmt%chars()),IOSTAT=ierr) trim(tmpTag%chars())
      !content
      WRITE(sint,'(i16)',IOSTAT=ierr) nspace+2
      fmt='('//TRIM(ADJUSTL(sint))//'x'
      WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(obj%content)
      fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      IF(obj%content /= CHAR_LF) &
      & WRITE(unitNo,FMT=TRIM(fmt%chars()),IOSTAT=ierr) TRIM(obj%content%chars())
      !endtag
      tmpTag='</'//obj%name//'>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unitNo,FMT=TRIM(fmt%chars()),IOSTAT=ierr) TRIM(tmpTag%chars())
    ELSE
      !empty element tag
      tmpTag='<'//obj%name
      !Add attributes
      DO i=1,obj%tAttributes
        tmpTag=tmpTag//' '//obj%attrNames(i)//'="'// &
            obj%attrValues(i)//'"'
      END DO
      tmpTag=tmpTag//'/>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unitNo,FMT=TRIM(fmt%chars()),IOSTAT=ierr) TRIM(tmpTag%chars())
    ENDIF
    fmt=''
    tmpTag=''
  ENDIF
END PROCEDURE xmlTag_export

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_Display
  CHARACTER( LEN=16 ) :: sint
  INTEGER( I4B ) :: i,ierr,nspace, unit_No
  TYPE( String ) :: fmt,tmpTag
  !>
  IF( obj%name%len_trim() > 0 ) THEN
    nspace=0
    unit_No = INPUT( option=unitNo, default=stdout )
    ! IF(PRESENT(nindent)) nspace=2*nindent
    IF(ASSOCIATED(obj%children)) THEN
      !start tag
      tmpTag='<'//obj%name
      !Add attributes
      DO i=1,obj%tAttributes
        tmpTag=tmpTag//' '//obj%attrNames(i)//'="'// &
            obj%attrValues(i)//'"'
      END DO
      tmpTag=tmpTag//'>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unit_No,FMT=trim(fmt%chars()),IOSTAT=ierr) trim(tmpTag%chars())
      !children
      DO i=1,SIZE(obj%children)
        CALL obj%children(i)%export(unit_No,nspace/2+1)
      ENDDO
      !end tag
      tmpTag='</'//obj%name//'>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unit_No,FMT=trim(fmt%chars()),IOSTAT=ierr) trim(tmpTag%chars())
    ELSEIF(obj%content%len() > 0) THEN
      !start tag
      tmpTag='<'//obj%name
      !Add attributes
      DO i=1,obj%tAttributes
        tmpTag=tmpTag//' '//obj%attrNames(i)//'="'// &
          & obj%attrValues(i)//'"'
      END DO
      tmpTag=tmpTag//'>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unit_No,FMT=trim(fmt%chars()),IOSTAT=ierr) trim(tmpTag%chars())
      !content
      WRITE(sint,'(i16)',IOSTAT=ierr) nspace+2
      fmt='('//TRIM(ADJUSTL(sint))//'x'
      WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(obj%content)
      fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      IF(obj%content /= CHAR_LF) &
      & WRITE(unit_No,FMT=TRIM(fmt%chars()),IOSTAT=ierr) TRIM(obj%content%chars())
      !endtag
      tmpTag='</'//obj%name//'>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unit_No,FMT=TRIM(fmt%chars()),IOSTAT=ierr) TRIM(tmpTag%chars())
    ELSE
      !empty element tag
      tmpTag='<'//obj%name
      !Add attributes
      DO i=1,obj%tAttributes
        tmpTag=tmpTag//' '//obj%attrNames(i)//'="'// &
            obj%attrValues(i)//'"'
      END DO
      tmpTag=tmpTag//'/>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unit_No,FMT=TRIM(fmt%chars()),IOSTAT=ierr) TRIM(tmpTag%chars())
    ENDIF
    fmt=''
    tmpTag=''
  ENDIF
END PROCEDURE xmlTag_Display

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE xmltag_Write
  CHARACTER( LEN = * ), PARAMETER :: myName="xmltag_Write"
  INTEGER( I4B ) :: iostat
  CHARACTER( LEN = 100 ) :: iomsg
  TYPE( String )  :: endRecord_

  IF( PRESENT( endRecord ) ) THEN
    endRecord_ = endRecord
  ELSE
    endRecord_ = ''
  END IF
  !>
  WRITE( unit=unitNo, fmt='(A)', iostat=iostat, iomsg=iomsg ) &
    & obj%stringify(                        &
    & isIndented=isIndented,                &
    & isContentIndented=isContentIndented,  &
    & onlyStart=onlyStart,                  &
    & onlyContent=onlyContent,              &
    & onlyEnd=onlyEnd) // endRecord_
  !>
  IF( iostat .NE. 0 ) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Error has occured while writing header info in VTKFile &
      & iostat = ' // trim(str(iostat, .true.)) // ' error msg :: ' // &
      & TRIM( iomsg ) )
  END IF
END PROCEDURE xmltag_Write

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods