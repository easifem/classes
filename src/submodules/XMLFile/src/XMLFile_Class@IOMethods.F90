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

SUBMODULE( XMLFile_Class ) IOMethods
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_END, IOSTAT_EOR
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Export
  CHARACTER( LEN = 4 ) :: version
  INTEGER( I4B ) :: ierr
  TYPE( String ) :: header
  TYPE( XMLFile_ ) :: tmpFile
  !>
  IF( ASSOCIATED( obj%root ) ) THEN
    CALL tmpFile%initiate( filename=filename, mode="NEW" )
    CALL tmpFile%open()
    !Write the header
    WRITE(version,FMT='(f4.1)',IOSTAT=ierr) obj%version
    header='<?xml version="'//TRIM(ADJUSTL(version))// &
        '" encoding="'//TRIM(obj%encoding)//'"?>'
    WRITE(tmpFile%unitNo,FMT='(a)') TRIM(header%chars())
    !Write style-sheet info
    IF( obj%style_sheet%LEN() > 0 ) THEN
      WRITE( tmpFile%unitNo, FMT='(a)' ) &
        & '<?xml-stylesheet version="1.0" type="text/xsl" href="'// &
        & obj%style_sheet//'"?>'
    END IF
    !Write the XML Elements
    CALL obj%root%export( unitNo=tmpFile%unitNo, nindent=0 )
    CALL tmpFile%DeallocateData()
  END IF
END PROCEDURE xmlFile_Export

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Display
  IF( obj%isInitiated ) THEN
    CALL Display( "XMLFile is Initiated", unitNo=unitNo )
  ELSE
    CALL Display( "XMLFile is not Initiated", unitNo=unitNo )
    RETURN
  END IF
  !>
  CALL Display( obj%getFilePath(), "Path : ", unitNo=unitNo )
  CALL Display( obj%getFileName(), "File Name  : ", unitNo=unitNo )
  CALL Display( obj%getFileExt(), "File Extension  : ", unitNo=unitNo )
  CALL Display( obj%isOpen(), "isOpen  : ", unitNo=unitNo )
  CALL Display( obj%isEOF(), "isEOF  : ", unitNo=unitNo )
  CALL Display( obj%isRead(), "isRead  : ", unitNo=unitNo )
  CALL Display( obj%isWrite(), "isWrite  : ", unitNo=unitNo )
  CALL Display( obj%unitNo, "unitNo  : ", unitNo=unitNo )
  CALL Display( obj%version, "version  : ", unitNo=unitNo )
  CALL Display( obj%encoding, "encoding  : ", unitNo=unitNo )
  CALL Display( obj%style_sheet, "style_sheet  : ", unitNo=unitNo )
  CALL Display( obj%standalone, "standalone  : ", unitNo=unitNo )
  CALL Display( obj%newstat, "isNew  : ", unitNo=unitNo )
  CALL Display( obj%overwriteStat, "isOverwrite  : ", unitNo=unitNo )
  CALL Display( obj%fullname, "fullname  : ", unitNo=unitNo )
  IF( ASSOCIATED( obj%root ) ) THEN
    CALL Display( "Root is ASSOCIATED : ", unitNo=unitNo )
    CALL obj%root%Display( msg="ROOT ", unitNo=unitNo )
  ELSE
    CALL Display( "Root is NOT ASSOCIATED : ", unitNo=unitNo )
  END IF
END PROCEDURE xmlFile_Display

!----------------------------------------------------------------------------
!                                                                     Import
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Import
  CHARACTER( LEN=* ), PARAMETER :: myName='xmlFile_Import'
  CHARACTER( LEN=1 ), ALLOCATABLE :: cachedFile( : )
  INTEGER( I4B ) :: nchars,nopen,nclose,nTags,ic,i,nlines
  INTEGER( I4B ) :: rootTagEnd,rootTagBegin
  INTEGER( I4B ), ALLOCATABLE :: itag(:,:),lines(:)
  TYPE( String ) :: tagStr
  !> Initialize and open the file if needed
  IF( .NOT. obj%isInitiated ) THEN
    CALL obj%initiate( filename=filename, mode="READ" )
  END IF
  !>
  IF( .NOT. obj%isOpen() ) THEN
    CALL obj%Open()
  END IF
  !>
  IF( .NOT. obj%isRead() ) THEN
    CALL e%raiseError(modName//'::'//myName// &
      & ' - xmlFile does not have read access!')
  END IF
  !> deallocate the root
  CALL obj%root%DeallocateData()
  CALL obj%ParseXMLDeclaration()
  SELECT TYPE(obj); TYPE IS( XMLFile_ )
    CALL obj%BuildCache( nchars=nchars, fileCache=cachedFile )
  END SELECT
  !> Count the number of markup characters "<" and ">" and lines
  nopen=0; nclose=0; nlines=0
  DO ic=1,nchars
    IF( cachedFile(ic) .EQ. '<' ) nopen=nopen+1
    IF( cachedFile(ic) .EQ. '>' ) nclose=nclose+1
    IF( cachedFile(ic) .EQ. CHAR_LF ) nlines=nlines+1
  END DO
  !>
  IF( nopen .NE. nclose) THEN
    CALL e%raiseError(modName//'::'//myName// &
      & ' - mismatched markup characters!')
  END IF
  !> Store the locations of all the markup characters "<" and ">" lines
  nTags=nopen
  ALLOCATE(itag(3,nTags))
  ALLOCATE(lines(nlines))
  nopen=0; nclose=0; nlines=0
  DO ic=1,nchars
    IF(cachedFile(ic) .EQ. '<') THEN
      nopen=nopen+1
      itag(1,nopen)=ic
    END IF
    IF(cachedFile(ic) .EQ. '>') THEN
      nclose=nclose+1
      itag(2,nopen)=ic
    END IF
    IF(cachedFile(ic) .EQ. CHAR_LF) THEN
      nlines=nlines+1
      lines(nlines)=ic
    END IF
  END DO
  !> Verify that they are all matching (interleaved)
  DO i=1,nTags
    IF(itag(1,i) > itag(2,i)) THEN
      CALL e%raiseError(modName//'::'//myName// &
        & ' - mismatched markup characters!' )
      EXIT
    END IF
  END DO
  !>
  DO i=1,nTags
    !Create temporary string
    CALL ConvertCharArrayToStr( &
      & chars=cachedFile(itag(1,i):itag(2,i)), &
      & strobj=tagStr )
    itag(3,i)=BAD_TAG
    !Determine tag types
    IF( tagStr%INDEX('<?') .EQ. 1 ) THEN
      !Processing Instruction
      !Check closing marker to insure tag validity
      IF(tagStr%INDEX('?>',.TRUE.) .EQ. tagStr%LEN_TRIM()-1) &
        & itag(3,i)=PROCESSING_INST_TAG
    ELSE IF(tagStr%INDEX('<!--') .EQ. 1) THEN
      !Check closing marker to insure tag validity
      IF( tagStr%INDEX('-->',.TRUE.) .NE. tagStr%LEN_TRIM()-1 .AND. &
        & tagStr%INDEX('--->',.TRUE.) .EQ. 0 ) itag(3,i)=COMMENT_TAG
    ELSE IF(tagStr%INDEX('<!') .EQ. 1) THEN
      !Declaration (Not currently treated)
      itag(3,i)=DECLARATION_TAG
    ELSE IF(tagStr%INDEX('</') .EQ. 1) THEN
      !EndTag
      !Check closing marker to insure tag validity
      itag(3,i)=END_TAG
    ELSE
      !Determine if start tag or empty-element tag
      IF(tagStr%INDEX('/>',.TRUE.) .EQ. tagStr%LEN_TRIM()-1) THEN
        itag(3,i)=EMPTY_ELEMENT_TAG
      ELSE
        itag(3,i)=START_TAG
      END IF
    END IF
    IF(itag(3,i) .EQ. BAD_TAG) THEN
      CALL e%raiseError(modName//'::'//myName// &
        & ' - Unrecognizable markup in "'//tagStr//'"!' )
    END IF
  END DO
  !Find first start tag
  DO i=1,nTags
    IF(itag(3,i) .EQ. START_TAG) THEN
      rootTagBegin=i
      EXIT
    ELSE IF(itag(3,i) .EQ. END_TAG .OR. &
      & (itag(3,i) .EQ. EMPTY_ELEMENT_TAG .AND. i < nTags)) THEN
        CALL e%raiseError(modName//'::'//myName// &
        ' - Could not locate start of root element!')
    END IF
  ENDDO
  !> Find last end tag
  DO i=nTags,1,-1
    IF( itag(3,i) .EQ. END_TAG ) THEN
      rootTagEnd=i
      EXIT
    ELSE IF( &
      & itag(3,i) .EQ. START_TAG .OR. &
      & (itag(3,i) .EQ. EMPTY_ELEMENT_TAG &
      & .AND. i < nTags) .OR. &
      & itag(3,i) .EQ. DECLARATION_TAG) THEN
      CALL e%raiseError(modName//'::'//myName// &
        & ' - Could not locate end of root element!')
    END IF
  ENDDO
  !> Process the elements
  CALL obj%root%Initiate( cachedFile=cachedFile, &
    & itag=itag, lines=lines, tagStart=rootTagBegin, &
    & tagEnd=rootTagEnd )
END PROCEDURE xmlFile_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_parseXMLDeclaration
  CHARACTER( LEN = * ), PARAMETER :: myName='xmlFile_parseXMLDeclaration'
  INTEGER( I4B ), PARAMETER :: maxbuf=512
  CHARACTER( LEN=1 ) :: tmpChar
  CHARACTER( LEN=7 ) :: fencoding,curEncoding
  CHARACTER( LEN=maxbuf ) :: ioBuffer
  LOGICAL( LGT ) :: lread
  INTEGER( I4B ) :: i,ierr,ibuf,nattr
  REAL( DFP ) :: version
  TYPE( String ) :: tagStr,firstTag,fname
  TYPE( String ), ALLOCATABLE :: anames(:), avalues(:)
  !> main program
  fencoding='DEFAULT'
  !Rewind the file
  REWIND(obj%unitNo)
  !Get the first tag.
  ioBuffer=''; ierr=0; lread=.TRUE.; tagStr=''
  DO WHILE(lread)
    READ(obj%unitNo,'(a1)',ADVANCE='NO',IOSTAT=ierr) tmpChar
    IF(ierr .EQ. IOSTAT_EOR) tmpChar=CHAR(10)
    IF(ierr .EQ. IOSTAT_END) THEN
      CALL e%raiseError(modName//'::'//myName// &
        & ' - Reached end of file before finding start of first tag "<"!')
      EXIT
    END IF
    !>
    IF(tmpChar .EQ. '<') THEN
      !At the start of the tag
      ibuf=1
      ioBuffer(1:1)=tmpChar
      !Stream to closing marker
      DO WHILE(ierr .NE. IOSTAT_END .AND. tmpChar .NE. '>')
        ibuf=ibuf+1
        READ(obj%unitNo,'(a1)',ADVANCE='NO',IOSTAT=ierr) tmpChar
        ioBuffer(ibuf:ibuf)=tmpChar
        IF(ibuf .EQ. maxbuf) THEN
          tagStr=tagStr//ioBuffer
          ibuf=1
        END IF
      END DO
      !Insure that the last character of the ioBuffer is '>'
      IF(ioBuffer(ibuf:ibuf) .EQ. '>') THEN
        tagStr=tagStr//ioBuffer(1:ibuf)
      ELSE
        !Throw an exception end of file before end marker
        CALL e%raiseError(modName//'::'//myName// &
          & ' - Reached end of file before finding closing marker'// &
          & ' ">" for XML tag!')
      END IF
      EXIT
    ELSE
      !Insure that only whitespace was read
      IF(.NOT.(SCAN(tmpChar,CHAR_SPACE//CHAR_CR//CHAR_LF//CHAR_TAB) > 0)) THEN
        !Throw an exception, illegal characters
        CALL e%raiseError(modName//'::'//myName// &
          & ' - Illegal characters between XML tags!')
        EXIT
      END IF
    END IF
  END DO

  firstTag=tagStr
  !> Check that the first Tag starting and ending markers are
  !the XML declaration
  IF( firstTag%INDEX( '<?xml' ) .EQ. 1 .AND. &
    & firstTag%INDEX('?>',.TRUE.) .EQ. firstTag%LEN_TRIM()-1) THEN
    !Process the attributes if it is an xml declaration
    CALL parseTagAttributes( chars=TRIM(firstTag%chars()), &
      & tAttributes=nattr, attrNames=anames, &
      & attrValues=avalues, ierr=ierr )
    IF(ierr .EQ. 0) THEN
      !The XML declaration only has 3 possible attributes
      ! - version (required, always first)
      ! - encoding (optional)
      ! - standalone (optional)
      IF(0 < nattr .AND. nattr < 4) THEN
        !Set the version
        IF(anames(1) .EQ. 'version') THEN
          IF(avalues(1)%INDEX('1.') .EQ. 1) THEN
            version=avalues(1)%to_number(1.0_DFP)
            IF(1.0_DFP < version .AND. version < 2.0_DFP) &
              & obj%version=version
          ELSE
            !Illegal value for version
            CALL e%raiseError(modName//'::'//myName// &
              & ' - XML Version "'//avalues(1)//'" is not &
              & supported!')
          END IF
        ELSE
          !first attribute must be 'version'!
          CALL e%raiseError(modName//'::'//myName// &
            & ' - The first attribute must be the XML version!')
        END IF
        !Check other attributes
        IF(nattr > 1) THEN
          DO i=2,nattr
            IF(anames(i) .EQ. 'encoding') THEN
              !check if this module can process the encoding
              IF(avalues(i) .EQ. 'UTF-8' .OR. avalues(i) .EQ. 'utf-8') THEN
                obj%encoding='UTF-8'
                fencoding='UTF-8'
              ELSE IF(avalues(i) .EQ. 'US-ASCII' &
                & .OR. avalues(i) .EQ. 'us-ascii') THEN
                obj%encoding='US-ASCII'
              ELSE
                CALL e%raiseError(modName//'::'//myName// &
                  & ' - File encoding "'//avalues(i)%TRIM()//'" is not &
                  & supported!')
              END IF
              !Re-open file if it was not opened with the matching encoding
              INQUIRE(UNIT=obj%unitNo,ENCODING=curEncoding)
              IF(fencoding .NE. curEncoding) THEN
                CALL obj%close()
                CALL obj%open()
              END IF
            ELSE IF(anames(i) .EQ. 'standalone') THEN
              IF(avalues(i) .EQ. 'yes') THEN
                obj%standalone=.TRUE.
              ELSE IF(avalues(i) .EQ. 'no') THEN
                obj%standalone=.FALSE.
              ELSE
                !Illegal value for 'standalone' attribute
                CALL e%raiseError(modName//'::'//myName// &
                  & ' - illegal value "'// avalues(i)%TRIM()// &
                  & '" for "standalone" attribute in XML declaration.')
              END IF
            ELSE
              !Illegal attribute
              CALL e%raiseError(modName//'::'//myName// &
                & ' - illegal attribute "'//anames(i)%TRIM()// &
                & '" for XML declaration.')
            END IF
          ENDDO
        END IF
      ELSE
        !Illegal number of attributes in declaration
        CALL e%raiseError(modName//'::'//myName// &
          & ' - illegal  number of attributes in XML declaration.')
      END IF
    ELSE
      !Failed to process attributes for XMLDecl
      CALL e%raiseError(modName//'::'//myName// &
        & ' - Failed to process attributes in XML declaration.')
    END IF
  END IF
  ! REWIND(obj%unitNo)
END PROCEDURE xmlFile_parseXMLDeclaration

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_BuildCache
  CHARACTER( LEN = * ), PARAMETER :: myName="xmlFile_BuildCache"
  INTEGER( I4B ), PARAMETER :: maxbuf=1024
  CHARACTER( LEN = maxbuf ) :: tmpChar
  CHARACTER( LEN = 256 ) :: fmt
  INTEGER( I4B ) :: i,j,ierr
  !> main program
  IF( ALLOCATED(fileCache) ) DEALLOCATE(fileCache)
  REWIND(obj%unitNo, IOSTAT=ierr )
  IF( ierr .NE. 0 ) THEN
    CALL e%raiseError(modName//'::'//myName// &
      & ' - Some error has occured while rewinding.')
  END IF
  ierr=0; nchars=0
  fmt = '( a' // trim(str(maxbuf, .TRUE.)) // ' )'
  DO WHILE(ierr .NE. IOSTAT_END)
    READ(obj%unitNo, fmt, ADVANCE='NO',IOSTAT=ierr) tmpChar
    IF(ierr .EQ. 0) THEN
      nchars=nchars+maxbuf
    ELSE IF(ierr .EQ. IOSTAT_EOR) THEN
      nchars=nchars+LEN_TRIM(tmpChar)+1
    END IF
  END DO
  !>
  IF(nchars > 0) THEN
    ALLOCATE(fileCache(nchars))
    REWIND(obj%unitNo)
    ierr=0
    i=0
    DO WHILE(ierr .NE. IOSTAT_END)
      READ(obj%unitNo, fmt, ADVANCE='NO',IOSTAT=ierr) tmpChar
      IF(ierr .EQ. 0) THEN
        DO j=1,maxbuf
          fileCache(i+j)=tmpChar(j:j)
        ENDDO
        i=i+maxbuf
      ELSE IF(ierr .EQ. IOSTAT_EOR) THEN
        DO j=1,LEN_TRIM(tmpChar)
          fileCache(i+j)=tmpChar(j:j)
        ENDDO
        fileCache(i+j)=CHAR_LF
        i=i+j
      END IF
    ENDDO
    CALL obj%close()
  ELSE
    nchars=-1
  END IF
END PROCEDURE xmlFile_BuildCache

END SUBMODULE IOMethods