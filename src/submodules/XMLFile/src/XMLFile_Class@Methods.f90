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

SUBMODULE( XMLFile_Class ) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE getTagName
  ! CHARACTER( LEN=3 ) :: xml
  ! INTEGER( I4B ) :: nchar,istp,inamechar,i,charval,istt

  ! sname=''
  ! nchar=SIZE(fullTag)

  ! IF( fullTag(1) == '<' .AND. fullTag(nchar) == '>' ) THEN
  !   istt=2
  !   IF(fullTag(2) == '/') istt=3
  !     !! This is an endtag
  !   inamechar=IACHAR(fullTag(istt))

  !   IF(inamechar == 58 .OR. inamechar == 95 .OR. &
  !       (64 < inamechar .AND. inamechar < 91) .OR. &
  !       (96 < inamechar .AND. inamechar < 123)) THEN

  !     IF(nchar-istt > 2) THEN
  !       xml(1:1)=fullTag(istt)
  !       xml(2:2)=fullTag(istt+1)
  !       xml(3:3)=fullTag(istt+2)
  !       CALL toUpper(xml)
  !       IF(xml == 'XML') THEN
  !         !Names cannot start with "xml"
  !         ierr=-4
  !         istp=-1
  !         nchar=1 !Skip executing the loop
  !       ENDIF
  !     ENDIF

  !     istp=0
  !     DO i=istt,nchar-1
  !       IF(ANY(fulltag(i) == (/LF,CR,SP,TB/))) THEN
  !         istp=i-1
  !         EXIT
  !       ENDIF
  !       charval=IACHAR(fulltag(i))

  !       !Check that the character is valid in a name
  !       IF(.NOT.(charval == 45 .OR. charval == 96 .OR. &
  !               charval == 95 .OR. &
  !               (64 < charval .AND. charval < 91) .OR. &
  !               (96 < charval .AND. charval < 123) .OR. &
  !               (47 < charval .AND. charval < 59))) THEN
  !         istp=-1
  !         EXIT
  !       ENDIF
  !     ENDDO
  !     IF(istp == 0) istp=nchar-1
  !     IF(istp > 0) THEN
  !       CALL charArrytoStr(fullTag(istt:istp), sname)
  !       ierr=0
  !     ELSE
  !       ierr=-3 !Illegal character in tag name
  !     ENDIF
  !   ELSE
  !     ierr=-2 !Illegal first character of tag name
  !   ENDIF
  ! ELSE
  !   ierr=-1 !Bad Tag
  ! ENDIF
END PROCEDURE getTagName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods