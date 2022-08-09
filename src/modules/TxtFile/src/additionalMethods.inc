
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Stefano Zaghi, https://github.com/szaghi
! date: 21 July 2021
! summary: Get the DELIM changeable connection mode for the given unit.
!
!# Introduction
!
! If the unit is connected to an internal file, then the default value of
! NONE is always returned.

! non type-bound-procedures
SUBROUTINE get_delimiter_mode(unit, delim, iostat, iomsg)
  USE, INTRINSIC :: iso_fortran_env, ONLY: iostat_inquire_internal_unit
  INTEGER, INTENT(in) :: unit
  !! The unit for the connection.
  CHARACTER(len=1, kind=CK), INTENT(out) :: delim
  !! Represents the value of the DELIM mode.
  INTEGER, INTENT(out) :: iostat
  !! IOSTAT error code, non-zero on error.
  CHARACTER(*), INTENT(inout) :: iomsg
  !! IOMSG explanatory message - only defined if iostat is non-zero.
  CHARACTER(10) :: delim_buffer
  !! Buffer for INQUIRE about DELIM, sized for APOSTROHPE.
  CHARACTER(LEN(iomsg)) :: local_iomsg
  !! Local variant of iomsg, so it doesn't get inappropriately redefined.
  !!
  !! get the string representation of the changeable mode
  !!
  INQUIRE (unit, delim=delim_buffer, iostat=iostat, iomsg=local_iomsg)
  !!
  IF (iostat == iostat_inquire_internal_unit) THEN
    ! no way of determining the DELIM mode for an internal file
    iostat = 0
    delim = ''
    RETURN
  ELSEIF (iostat /= 0) THEN
    iomsg = local_iomsg
    RETURN
  END IF
  ! interpret the DELIM string
  IF (delim_buffer == 'QUOTE') THEN
    delim = '"'
  ELSEIF (delim_buffer == 'APOSTROPHE') THEN
    delim = ''''
  ELSE
    delim = '"'
  END IF
END SUBROUTINE get_delimiter_mode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Stefano Zaghi, https://github.com/szaghi
! date: 21 July 2022
! summary: Get the next non-blank character in the current record.

SUBROUTINE get_next_non_blank_character_this_record(unit, ch, iostat, iomsg)
  INTEGER, INTENT(in) :: unit
  !! Logical unit.
  CHARACTER(kind=CK, len=1), INTENT(out) :: ch
  !! The non-blank character read. Not valid if IOSTAT is non-zero.
  INTEGER, INTENT(out) :: iostat
  !! IO status code.
  CHARACTER(kind=CK, len=*), INTENT(inout) :: iomsg
  !! IO status message.
  !!
  DO
    ! we spcify non-advancing, just in case we want this callable outside the
    ! context of a child input statement
    ! the PAD specifier simply saves the need for the READ statement to
    ! define ch if EOR is hit
    ! read(unit, "(A)", iostat=iostat, iomsg=iomsg, advance='NO') ch
    ! ...but that causes ifort to blow up at runtime
    READ (unit, "(A)", iostat=iostat, iomsg=iomsg, pad='NO') ch
    IF (iostat .NE. 0) RETURN
    IF (ch .NE. '') EXIT
  END DO
END SUBROUTINE get_next_non_blank_character_this_record

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Stefano Zaghi, https://github.com/szaghi
! date: 21 July 2022
! summary: Get the next non-blank character, advancing records if necessary.

SUBROUTINE get_next_non_blank_character_any_record(unit, ch, iostat, iomsg)
  INTEGER, INTENT(in) :: unit
  !! Logical unit.
  CHARACTER(kind=CK, len=1), INTENT(out) :: ch
  !! The non-blank character read. Not valid if IOSTAT is non-zero.
  INTEGER, INTENT(out) :: iostat
  !! IO status code.
  CHARACTER(kind=CK, len=*), INTENT(inout) :: iomsg
  !! IO status message.
  CHARACTER(LEN(iomsg)) :: local_iomsg
  !! Local variant of iomsg, so it doesn't get inappropriately redefined.
  !!
  DO
    CALL get_next_non_blank_character_this_record(unit=unit, ch=ch, &
      & iostat=iostat, iomsg=local_iomsg)
    IF (IS_IOSTAT_EOR(iostat)) THEN
      ! try again on the next record
      READ (unit, "(/)", iostat=iostat, iomsg=iomsg)
      IF (iostat .NE. 0) RETURN
    ELSEIF (iostat .NE. 0) THEN
      ! some sort of problem
      iomsg = local_iomsg
      RETURN
    ELSE
      ! got it
      EXIT
    END IF
  END DO
END SUBROUTINE get_next_non_blank_character_any_record

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Stefano Zaghi, https://github.com/szaghi
! date: 21 July 2022
! summary: Get the DECIMAL changeable connection mode for the given unit.
!
!# Introduction
!
! If the unit is connected to an internal file,
! then the default value of DECIMAL is always returned.
! This may not be the actual value in force at the time of the call
! to this procedure.

SUBROUTINE get_decimal_mode(unit, decimal_point, iostat, iomsg)
  USE, INTRINSIC :: iso_fortran_env, ONLY: iostat_inquire_internal_unit
  INTEGER, INTENT(in) :: unit
  !! Logical unit.
  LOGICAL, INTENT(out) :: decimal_point
  !! True if the decimal mode is POINT, false otherwise.
  INTEGER, INTENT(out) :: iostat
  !! IO status code.
  CHARACTER(kind=CK, len=*), INTENT(inout) :: iomsg
  !! IO status message.
  CHARACTER(5) :: decimal_buffer
  !! Buffer for INQUIRE about DECIMAL, sized for POINT or COMMA.
  CHARACTER(LEN(iomsg)) :: local_iomsg
  !! Local iomsg, so it doesn't get inappropriately redefined.
  !!
  !!
  INQUIRE (unit, decimal=decimal_buffer, iostat=iostat, iomsg=local_iomsg)
  !!
  IF (iostat .EQ. iostat_inquire_internal_unit) THEN
    ! no way of determining the decimal mode for an internal file
    iostat = 0
    decimal_point = .TRUE.
    RETURN
  ELSE IF (iostat .NE. 0) THEN
    iomsg = local_iomsg
    RETURN
  END IF
  decimal_point = decimal_buffer == 'POINT'
END SUBROUTINE get_decimal_mode


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine read_file(file, lines, form, iostat, iomsg)
  !< Read a file as a single string stream.
  !<
  !< The lines are returned as an array of strings that are read until the eof is reached.
  !< The line is read as an ascii stream read until the eor is reached.
  !<
  !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
  !<
  !<```fortran
  !< type(string)              :: astring
  !< type(string), allocatable :: strings(:)
  !< type(string)              :: line(3)
  !< integer                   :: iostat
  !< character(len=99)         :: iomsg
  !< integer                   :: scratch
  !< integer                   :: l
  !< logical                   :: test_passed(8)
  !< line(1) = ' Hello World!   '
  !< line(2) = 'How are you?  '
  !< line(3) = '   All say: "Fine thanks"'
  !< open(newunit=scratch, file='read_file_test.tmp')
  !< write(scratch, "(A)") line(1)%chars()
  !< write(scratch, "(A)") line(2)%chars()
  !< write(scratch, "(A)") line(3)%chars()
  !< close(scratch)
  !< call read_file(file='read_file_test.tmp', lines=strings, iostat=iostat, iomsg=iomsg)
  !< test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+1) = (strings(l)==line(l))
  !< enddo
  !< open(newunit=scratch, file='read_file_test.tmp', form='UNFORMATTED', access='STREAM')
  !< write(scratch) line(1)%chars()//new_line('a')
  !< write(scratch) line(2)%chars()//new_line('a')
  !< write(scratch) line(3)%chars()//new_line('a')
  !< close(scratch)
  !< call read_file(file='read_file_test.tmp', lines=strings, form='unformatted', iostat=iostat, iomsg=iomsg)
  !< test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+5) = (strings(l)==line(l))
  !< enddo
  !< open(newunit=scratch, file='read_file_test.tmp', form='UNFORMATTED', access='STREAM')
  !< close(scratch, status='DELETE')
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  character(len=*), intent(in) :: file       !< File name.
  type(string), intent(out), allocatable :: lines(:)   !< The lines.
  character(len=*), intent(in), optional :: form       !< Format of unit.
  integer, intent(out), optional :: iostat     !< IO status code.
  character(len=*), intent(inout), optional :: iomsg      !< IO status message.
  type(string) :: form_      !< Format of unit, local variable.
  integer :: iostat_    !< IO status code, local variable.
  character(len=:), allocatable :: iomsg_     !< IO status message, local variable.
  integer :: unit       !< Logical unit.
  logical :: does_exist !< Check if file exist.

  iomsg_ = repeat(' ', 99); if (present(iomsg)) iomsg_ = iomsg
  inquire (file=file, iomsg=iomsg_, iostat=iostat_, exist=does_exist)
  if (does_exist) then
    form_ = 'FORMATTED'; if (present(form)) form_ = form; form_ = form_ % upper()
    select case (form_ % chars())
    case ('FORMATTED')
            open (newunit=unit, file=file, status='OLD', action='READ', iomsg=iomsg_, iostat=iostat_, err=10)
    case ('UNFORMATTED')
            open (newunit=unit, file=file, status='OLD', action='READ', form='UNFORMATTED', access='STREAM', &
            iomsg=iomsg_, iostat=iostat_, err=10)
    end select
      call read_lines(unit=unit, lines=lines, form=form, iomsg=iomsg_, &
        & iostat=iostat_)
10  close (unit)
  end if
  if (present(iostat)) iostat = iostat_
  if (present(iomsg)) iomsg = iomsg_
end subroutine read_file

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine read_lines(unit, lines, form, iostat, iomsg)
  !< Read lines (records) from a connected-formatted unit.
  !<
  !< @note The connected unit is rewinded. At a successful exit current record is at eof, at the beginning otherwise.
  !<
  !< The lines are returned as an array of strings that are read until the eof is reached.
  !< The line is read as an ascii stream read until the eor is reached.
  !<
  !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
  !<
  !< @note There is no doctests, this being tested by means of [[read_file]] doctests.
  integer, intent(in) :: unit     !< Logical unit.
  type(string), intent(out), allocatable :: lines(:) !< The lines.
  character(len=*), intent(in), optional :: form     !< Format of unit.
  integer, intent(out), optional :: iostat   !< IO status code.
  character(len=*), intent(inout), optional :: iomsg    !< IO status message.
  type(string) :: form_    !< Format of unit, local variable.
  integer :: iostat_  !< IO status code, local variable.
  character(len=:), allocatable :: iomsg_   !< IO status message, local variable.
  character(kind=CK, len=1) :: ch       !< Character storage.
  integer :: l        !< Counter.

  form_ = 'FORMATTED'; if (present(form)) form_ = form; form_ = form_ % upper()
  iomsg_ = repeat(' ', 99); if (present(iomsg)) iomsg_ = iomsg
  rewind (unit)
  select case (form_ % chars())
  case ('FORMATTED')
    l = 0
    do
      read (unit, *, err=10, end=10)
      l = l + 1
    end do
  case ('UNFORMATTED')
    l = 0
    do
      read (unit, err=10, end=10) ch
      if (ch == new_line('a')) l = l + 1
    end do
  end select
10 rewind (unit)
  if (l > 0) then
    allocate (lines(1:l))
    l = 1
    iostat_ = 0
    do
 call lines(l) % read_line(unit=unit, form=form, iostat=iostat_, iomsg=iomsg_)
            if ((iostat_ /= 0 .and. .not. is_iostat_eor(iostat_)) .or. (l >= size(lines, dim=1))) then
        exit
      end if
      l = l + 1
    end do
  end if
  if (present(iostat)) iostat = iostat_
  if (present(iomsg)) iomsg = iomsg_
end subroutine read_lines

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine write_file(file, lines, form, iostat, iomsg)
  !< Write a single string stream into file.
  !<
  !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
  !<
  !<```fortran
  !< type(string)              :: astring
  !< type(string)              :: anotherstring
  !< type(string), allocatable :: strings(:)
  !< type(string)              :: line(3)
  !< integer                   :: iostat
  !< character(len=99)         :: iomsg
  !< integer                   :: scratch
  !< integer                   :: l
  !< logical                   :: test_passed(8)
  !< line(1) = ' Hello World!   '
  !< line(2) = 'How are you?  '
  !< line(3) = '   All say: "Fine thanks"'
  !< anotherstring = anotherstring%join(array=line, sep=new_line('a'))
  !< call write_file(file='write_file_test.tmp', lines=line, iostat=iostat, iomsg=iomsg)
  !< call astring%read_file(file='write_file_test.tmp', iostat=iostat, iomsg=iomsg)
  !< call astring%split(tokens=strings, sep=new_line('a'))
  !< test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+1) = (strings(l)==line(l))
  !< enddo
  !< call write_file(file='write_file_test.tmp', lines=line, form='unformatted', iostat=iostat, iomsg=iomsg)
  !< call astring%read_file(file='write_file_test.tmp', form='unformatted', iostat=iostat, iomsg=iomsg)
  !< call astring%split(tokens=strings, sep=new_line('a'))
  !< test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
  !< do l=1, size(strings, dim=1)
  !<   test_passed(l+5) = (strings(l)==line(l))
  !< enddo
  !< open(newunit=scratch, file='write_file_test.tmp')
  !< close(scratch, status='DELETE')
  !< print '(L1)', all(test_passed)
  !<```
  !=> T <<<
  character(len=*), intent(in) :: file      !< File name.
  type(string), intent(in) :: lines(1:) !< The lines.
  character(len=*), intent(in), optional :: form      !< Format of unit.
  integer, intent(out), optional :: iostat    !< IO status code.
  character(len=*), intent(inout), optional :: iomsg     !< IO status message.
  type(string) :: form_     !< Format of unit, local variable.
  integer :: iostat_   !< IO status code, local variable.
  character(len=:), allocatable :: iomsg_    !< IO status message, local variable.
  integer :: unit      !< Logical unit.

  iomsg_ = repeat(' ', 99); if (present(iomsg)) iomsg_ = iomsg
  form_ = 'FORMATTED'; if (present(form)) form_ = form; form_ = form_ % upper()
  select case (form_ % chars())
  case ('FORMATTED')
         open (newunit=unit, file=file, action='WRITE', iomsg=iomsg_, iostat=iostat_, err=10)
  case ('UNFORMATTED')
         open (newunit=unit, file=file, action='WRITE', form='UNFORMATTED', access='STREAM', iomsg=iomsg_, iostat=iostat_, err=10)
  end select
      call write_lines(unit=unit, lines=lines, form=form, iomsg=iomsg_, iostat=iostat_)
10 close (unit)
  if (present(iostat)) iostat = iostat_
  if (present(iomsg)) iomsg = iomsg_
end subroutine write_file

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine write_lines(unit, lines, form, iostat, iomsg)
  !< Write lines (records) to a connected-formatted unit.
  !<
  !< @note There is no doctests, this being tested by means of [[write_file]] doctests.
  integer, intent(in) :: unit      !< Logical unit.
  type(string), intent(in) :: lines(1:) !< The lines.
  character(len=*), intent(in), optional :: form      !< Format of unit.
  integer, intent(out), optional :: iostat    !< IO status code.
  character(len=*), intent(inout), optional :: iomsg     !< IO status message.
  integer :: l         !< Counter.

  do l = 1, size(lines, dim=1)
  call lines(l) % write_line(unit=unit, form=form, iostat=iostat, iomsg=iomsg)
  end do
end subroutine write_lines