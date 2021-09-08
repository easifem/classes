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

SUBMODULE( XMLTag_Class ) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_addSurrogate
  CALL e%addSurrogate( UserObj )
END PROCEDURE xmlTag_addSurrogate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE getTagName
  CHARACTER( LEN = * ), PARAMETER :: myName="getTagName"
  CHARACTER( LEN=3 ) :: xml
  INTEGER( I4B ) :: nchar,istp,inamechar,i,charval,istt
  !> main program
  tagname=''
  nchar=SIZE(chars)
  IF( chars(1) .EQ. '<' .AND. chars(nchar) .EQ. '>' ) THEN
    istt=2
    IF(chars(2) .EQ. CHAR_FSLASH) istt=3
      !! This is an endtag
    inamechar=IACHAR(chars(istt))
    IF(inamechar .EQ. 58 .OR. inamechar .EQ. 95 .OR. &
        (64 < inamechar .AND. inamechar < 91) .OR. &
        (96 < inamechar .AND. inamechar < 123)) THEN
      IF(nchar-istt > 2) THEN
        xml(1:1)=chars(istt)
        xml(2:2)=chars(istt+1)
        xml(3:3)=chars(istt+2)
        CALL toUpperCase(xml)
        IF(xml .EQ. 'XML') THEN
          CALL e%raiseError(modName//'::'//myName// " - "// &
            & ' Names cannot start with XML')
          ierr=-4
          istp=-1
          nchar=1 !Skip executing the loop
        ENDIF
      ENDIF
      istp=0
      DO i=istt,nchar-1
        IF(ANY(chars(i) .EQ. [CHAR_LF,CHAR_CR,CHAR_SPACE,CHAR_TAB] )) THEN
          istp=i-1
          EXIT
        ENDIF
        charval=IACHAR(chars(i))
        !Check that the character is valid in a name
        IF(.NOT.(charval .EQ. 45 .OR. charval .EQ. 96 .OR. &
          & charval .EQ. 95 .OR. &
          & (64 .LT. charval .AND. charval .LT. 91) .OR. &
          & (96 .LT. charval .AND. charval .LT. 123) .OR. &
          & (47 .LT. charval .AND. charval .LT. 59))) THEN
          istp=-1
          EXIT
        ENDIF
      END DO
      IF(istp .EQ. 0) istp=nchar-1
      IF(istp .GT. 0) THEN
        CALL ConvertCharArrayToStr(chars=chars(istt:istp), strobj=tagname)
        ierr=0
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
          & 'Illegal character in tag name')
        ierr=-3
      ENDIF
    ELSE
      CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'Illegal first character of tag name')
      ierr=-2
    ENDIF
  ELSE
    CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'Bad tag')
    ierr=-1 !Bad Tag
  ENDIF
END PROCEDURE getTagName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvertCharArrayToStr
  CHARACTER(LEN=SIZE(chars)) :: tmpChar
  INTEGER( I4B ) :: i
  DO i=1,SIZE(chars)
    tmpChar(i:i)=chars(i)
  ENDDO
  strobj=tmpChar
END PROCEDURE ConvertCharArrayToStr

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE parseTagAttributes
  CHARACTER( LEN = * ), PARAMETER :: myName="parseTagAttributes"
  CHARACTER( LEN=1 ) :: quote
  CHARACTER( LEN=LEN(chars) ) :: startTag
  INTEGER( I4B ) :: ic,i,nchars,namestt,valstt,valstp,bs_i
  INTEGER( I4B ), ALLOCATABLE :: anchorLoc(:)
  LOGICAL( LGT ) :: word_encountered
  !Initialize return arguments
  ierr=0
  tAttributes=0
  IF( ALLOCATED( attrNames ) ) DEALLOCATE( attrNames )
  IF( ALLOCATED( attrValues ) ) DEALLOCATE( attrValues )
  !Copy input arg to temporary
  startTag=chars
  startTag=ADJUSTL(startTag)
  nchars=LEN_TRIM(startTag)
  !>
  IF(startTag(1:1) .EQ. '<' .AND. startTag(nchars:nchars) .EQ. '>') THEN
    !Make sure this is not an end tag or comment
    IF(startTag(1:2) .NE. '<!') THEN
      !Count the number of attributes (count the number of '=' characters)
      DO ic=2,nchars
        IF(startTag(ic:ic) .EQ. '=') THEN
          tAttributes=tAttributes+1
          !Check to make sure occurrence of '=' is not in '.EQ.'
          IF(startTag(ic-1:ic-1) .EQ. '=') THEN
            ierr=-2
            tAttributes=0
            EXIT
          ENDIF
        ENDIF
      ENDDO
      IF(ierr .NE. -2) THEN
        !Allocate the return arguments
        ALLOCATE(attrNames(tAttributes))
        ALLOCATE(attrValues(tAttributes))
        ALLOCATE(anchorLoc(tAttributes))
        !Get the locations of the '=' characters.
        tAttributes=0

        DO ic=2,nchars
          IF(startTag(ic:ic) .EQ. '=') THEN
            tAttributes=tAttributes+1
            anchorLoc(tAttributes)=ic
          ENDIF
        ENDDO
      ENDIF
      !Get the names (names precede the '=' character with possible whitespace preceeding '=')
      !attribute names cannot contain whitespace, must be unique and preceded
      !by whitespace
      DO i=1,tAttributes
        ! March backwards from anchorLoc(i) through char array if whitespace preceeds the '='
        IF(startTag(anchorLoc(i)-1:anchorLoc(i)-1) .EQ. ' ') THEN
          ! march through word string backwards until whitespace is encountered
          word_encountered=.FALSE.
          DO bs_i=anchorLoc(i)-2,1,-1
            IF(startTag(bs_i:bs_i) /= ' ' .AND. .NOT. word_encountered) word_encountered=.TRUE.
            IF(startTag(bs_i:bs_i) .EQ. ' ' .AND. word_encountered) THEN
              namestt=bs_i
              EXIT
            ENDIF
            IF(bs_i.EQ.1) ierr=-3
          ENDDO
        ELSE
          ! CHAR(9) .EQ. TAB, CHAR(10) .EQ. Newline, CHAR(13) .EQ. Carriage return
          ! SCAN(...,TRUE) returns rightmost position
          namestt=SCAN(startTag(1:anchorLoc(i)),' '//CHAR(9)//CHAR(10)//CHAR(13),.TRUE.)
        ENDIF
        IF(0 < namestt .AND. namestt < anchorLoc(i)) THEN
          attrNames(i)=startTag(namestt+1:anchorLoc(i)-1)
        ELSE
          ierr=-3
        ENDIF
      ENDDO

      !Get the values
      DO i=1,tAttributes
        valstt=anchorLoc(i)+1
        ! In case of whitespace following '='
        IF(startTag(valstt:valstt).EQ.' ') THEN
          ! march forward until quote is encountered
          DO bs_i=anchorLoc(i)+1,nchars
            IF(startTag(bs_i:bs_i) .EQ. '"') THEN
              valstt=bs_i
              EXIT
            ENDIF
            IF(bs_i.EQ.nchars) ierr=-4
          ENDDO
        ENDIF
        quote=startTag(valstt:valstt)
        valstp=INDEX(startTag(valstt+1:nchars),quote)+valstt
        IF(valstt < valstp) THEN
          attrValues(i)=startTag(valstt+1:valstp-1)
        ELSE
          ierr=-4
        ENDIF
      ENDDO
    ENDIF
  ELSE
    ierr=-1
  ENDIF
END PROCEDURE parseTagAttributes

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE getChildTagInfo
  CHARACTER( LEN = * ), PARAMETER :: myName="getChildTagInfo"
  INTEGER( I4B ) :: nTagRemain,iLevel,i,iChild

  tChild=0
  IF(ALLOCATED(childTags)) DEALLOCATE(childTags)

  nTagRemain=tagEnd-tagStart-1
  ierr=-1
  IF(nTagRemain > 0) THEN
    iLevel=0
    DO i=tagStart+1,tagEnd-1
      IF(iTag(3,i) .EQ. EMPTY_ELEMENT_TAG .AND. iLevel .EQ. 0) THEN
        tChild=tChild+1
      ELSEIF(iTag(3,i) .EQ. START_TAG) THEN
        iLevel=iLevel+1
      ELSEIF(iTag(3,i) .EQ. END_TAG) THEN
        iLevel=iLevel-1
        IF(iLevel .EQ. 0) tChild=tChild+1
      ENDIF
    ENDDO
    IF(iLevel /= 0) THEN
      !This means there were not matching end tags!
      ierr=-2
      tChild=0
    ELSE
      ierr=0
      ALLOCATE(childTags(2,tChild))
      iLevel=0
      iChild=0
      DO i=tagStart+1,tagEnd-1
        IF(iTag(3,i) .EQ. EMPTY_ELEMENT_TAG .AND. iLevel .EQ. 0) THEN
          iChild=iChild+1
          childTags(:,iChild)=i
        ELSEIF(iTag(3,i) .EQ. START_TAG) THEN
          iLevel=iLevel+1
          IF(iLevel .EQ. 1) THEN
            iChild=iChild+1
            childTags(1,iChild)=i
          ENDIF
        ELSEIF(iTag(3,i) .EQ. END_TAG) THEN
          iLevel=iLevel-1
          IF(iLevel .EQ. 0) childTags(2,iChild)=i
        ENDIF
      ENDDO
    ENDIF
  ENDIF
END PROCEDURE getChildTagInfo

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_Initiate
  INTEGER( I4B ) :: tChild,ichild,ierr
  INTEGER( I4B ), ALLOCATABLE :: childTags(:,:)
  TYPE( String ) :: startTagName,endTagName,tmpStr
  CHARACTER( LEN = * ), PARAMETER :: myName="xmlTag_Initiate"

  IF(iTag(3,tagStart) .EQ. EMPTY_ELEMENT_TAG) THEN
    !> Empty Element
    IF(tagEnd .EQ. tagStart) THEN
      !Get obj%name
      CALL getTagName( &
        & chars=cachedFile(iTag(1,tagStart):iTag(2,tagStart)), &
        & ierr=ierr, tagname=obj%name )
      !Process the attributes
      CALL ConvertCharArrayToStr( &
        & chars=cachedFile(iTag(1,tagStart):iTag(2,tagStart)), &
        & strobj = tmpStr )
      CALL parseTagAttributes( &
        & chars=TRIM(tmpStr%chars()), &
        & tAttributes = obj%tAttributes, &
        & attrNames = obj%attrNames, &
        & attrValues = obj%attrValues, &
        & ierr = ierr)
      IF(ierr .NE. 0) THEN
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'Some error has occured while parsing tag attributes')
      ENDIF
    ELSE
      CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'Some error has occured while parsing tag attributes')
    ENDIF

  ELSEIF(iTag(3,tagStart) .EQ. START_TAG) THEN
    !Start/End Tagged Element
    IF(tagEnd > tagStart) THEN
      IF(iTag(3,tagEnd) .EQ. END_TAG) THEN
        !Verify matching element names
        CALL getTagName( &
          & chars=cachedFile(iTag(1,tagStart):iTag(2,tagStart)), &
          & ierr=ierr, tagname= startTagName)
        CALL getTagName( &
          & chars=cachedFile(iTag(1,tagEnd):iTag(2,tagEnd)), &
          ierr=ierr, tagname=endTagName)
        IF(startTagName .EQ. endTagName .AND. ierr .EQ. 0) THEN
          !Store the name
          obj%name=startTagName
          !Process attributes
          CALL ConvertCharArrayToStr( &
            & chars=cachedFile(iTag(1,tagStart):iTag(2,tagStart)), &
            & strobj = tmpStr)
          CALL parseTagAttributes( &
            & chars=trim(tmpStr%chars()), &
            & tAttributes = obj%tAttributes, &
            & attrNames = obj%attrNames, &
            & attrValues = obj%attrValues, ierr = ierr)
          IF(ierr .NE. 0) THEN
            CALL e%raiseError(modName//'::'//myName// " - "// &
              & 'Some error has occured while parsing tag attributes')
          ENDIF
          !Determine the number of children
          CALL getChildTagInfo( tagStart=tagStart, tagEnd=tagEnd, &
            & iTag=iTag, tChild=tChild, childTags=childTags, ierr=ierr)
          IF(tChild > 0) THEN
            !Process the children if any
            ALLOCATE(obj%children(tChild))
            DO iChild=1,tChild
              !Find the tag begin and end for the child
              CALL obj%children(ichild)%Initiate( cachedFile=cachedFile, &
                & iTag=iTag, lines=lines, tagStart=childTags(1,iChild), &
                & tagEnd=childTags(2,iChild) )
              SELECT TYPE(xmle => obj)
              TYPE IS( XMLTag_ ); obj%children(ichild)%parent => xmle
              END SELECT
            END DO
          ELSE
            !Store Content
            CALL ConvertCharArrayToStr( &
              & chars=cachedFile(iTag(2,tagStart)+1:iTag(1,tagEnd)-1), &
              & strobj=obj%content )
            obj%content=TRIM(obj%content%chars())
          END IF
        ELSE
          CALL e%raiseError(modName//'::'//myName// " - "// &
            & 'Some error has occured')
        END IF
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
          & 'Some error has occured')
      END IF
    ELSE
      CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'Some error has occured')
    END IF
  ELSE
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'Some error has occured')
  END IF
END PROCEDURE xmlTag_Initiate

END SUBMODULE Methods