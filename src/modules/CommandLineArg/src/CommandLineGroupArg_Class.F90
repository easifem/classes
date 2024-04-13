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
! date: 2021-11-08
! update: 2021-11-08
! summary: Command line arguments group type

MODULE CommandLineGroupArg_Class
USE FACE, ONLY: colorize
USE AbstractCommandLine_Class, ONLY: AbstractCommandLineArg_
USE CommandLineArg_Class, ONLY: CommandLineArg_, &
     & ACTION_PRINT_HELP, ACTION_PRINT_VERS, ACTION_STORE, ACTION_STORE_STAR, ARGS_SEP
USE PENF
IMPLICIT NONE
PRIVATE
SAVE
! status codes
INTEGER(I4P), PUBLIC, PARAMETER :: STATUS_PRINT_V = -1
!! Print version status.
INTEGER(I4P), PUBLIC, PARAMETER :: STATUS_PRINT_H = -2
!!  Print help status.
! errors codes
INTEGER(I4P), PARAMETER :: ERROR_CONSISTENCY = 100
!!  CLAs group consistency error.
INTEGER(I4P), PARAMETER :: ERROR_M_EXCLUDE = 101
!!  Two mutually exclusive CLAs group have been called.

!----------------------------------------------------------------------------
!                                                        CommandLineGroupArg_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-08
! update: 2021-11-08
! summary: Command line arguments group class
!
!# Introduction
!
! This class is useful for building nested commands

TYPE, EXTENDS(AbstractCommandLineArg_) :: CommandLineGroupArg_
  PRIVATE
  CHARACTER(len=:), ALLOCATABLE, PUBLIC :: group
  !! Group name (command).
  INTEGER(I4P), PUBLIC :: Na = 0_I4P
  !! Number of CLA.
  INTEGER(I4P) :: Na_required = 0_I4P
  !! Number of required command line arguments.
  INTEGER(I4P) :: Na_optional = 0_I4P
  !! Number of optional command line arguments.
  TYPE(CommandLineArg_), ALLOCATABLE, PUBLIC :: cla(:)
  !! CLA list [1:Na].
  LOGICAL, PUBLIC :: is_called = .FALSE.
  !! Flag for checking if CLAs group has been passed to CLI.
CONTAINS
  ! public methods
  PROCEDURE, PUBLIC :: free
  !! Free dynamic memory.
  PROCEDURE, PUBLIC :: check
  !! Check data consistency.
  PROCEDURE, PUBLIC :: is_required_passed
  !! Check if required CLAs are passed.
  PROCEDURE, PUBLIC :: is_passed
  !! Check if a CLA has been passed.
  PROCEDURE, PUBLIC :: is_defined
  !! Check if a CLA has been defined.
  PROCEDURE, PUBLIC :: raise_error_m_exclude
  !! Raise error mutually exclusive CLAs passed.
  PROCEDURE, PUBLIC :: add
  !! Add CLA to CLAsG.
  PROCEDURE, PUBLIC :: parse
  !! Parse CLAsG arguments.
  PROCEDURE, PUBLIC :: usage
  !! Get correct CLAsG usage.
  PROCEDURE, PUBLIC :: signature
  !! Get CLAsG signature.
  PROCEDURE, PUBLIC :: sanitize_defaults
  !! Sanitize default values.
  ! private methods
  PROCEDURE, PRIVATE :: errored
  !! Trig error occurrence and print meaningful message.
  PROCEDURE, PRIVATE :: check_m_exclusive
  !! Check if two mutually exclusive CLAs have been passed.
  PROCEDURE, PRIVATE :: clasg_assign_clasg
  !! Assignment operator.
  GENERIC, PRIVATE :: ASSIGNMENT(=) => clasg_assign_clasg
  !! Assignment operator overloading.
  FINAL :: finalize
  !! Free dynamic memory when finalizing.
END TYPE CommandLineGroupArg_

PUBLIC :: CommandLineGroupArg_

!----------------------------------------------------------------------------
!                                                                  Contains
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                  free
!----------------------------------------------------------------------------

ELEMENTAL SUBROUTINE free(self)
  !< Free dynamic memory.
  CLASS(CommandLineGroupArg_), INTENT(inout) :: self !< CLAsG data.
  ! object members
  CALL self%free_object()
  ! CommandLineGroupArg_ members
  IF (ALLOCATED(self%group)) DEALLOCATE (self%group)
  IF (ALLOCATED(self%cla)) THEN
    CALL self%cla%free()
    DEALLOCATE (self%cla)
  END IF
  self%Na = 0_I4P
  self%Na_required = 0_I4P
  self%Na_optional = 0_I4P
  self%is_called = .FALSE.
END SUBROUTINE free

!----------------------------------------------------------------------------
!                                                                  check
!----------------------------------------------------------------------------

SUBROUTINE check(self, pref)
  !< Check data consistency.
  CLASS(CommandLineGroupArg_), INTENT(inout) :: self  !< CLAsG data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref  !< Prefixing string.
  INTEGER(I4P) :: a     !< Counter.
  INTEGER(I4P) :: aa    !< Counter.

  ! verify if CLAs switches are unique
  CLA_UNIQUE: DO a = 1, self%Na
    IF (.NOT. self%cla(a)%is_positional) THEN
      DO aa = 1, self%Na
        IF ((a /= aa) .AND. (.NOT. self%cla(aa)%is_positional)) THEN
          IF ((self%cla(a)%switch == self%cla(aa)%switch) &
               & .OR. (self%cla(a)%switch_ab == self%cla(aa)%switch) &
               & .OR. (self%cla(a)%switch == self%cla(aa)%switch_ab) &
               & .OR. (self%cla(a)%switch_ab == self%cla(aa)%switch_ab)) THEN
            CALL self%errored(pref=pref, error=ERROR_CONSISTENCY, a1=a, a2=aa)
            EXIT CLA_UNIQUE
          END IF
        END IF
      END DO
    END IF
  END DO CLA_UNIQUE
  ! update mutually exclusive relations
  CLA_EXCLUDE: DO a = 1, self%Na
    IF (.NOT. self%cla(a)%is_positional) THEN
      IF (self%cla(a)%m_exclude /= '') THEN
        IF (self%is_defined(switch=self%cla(a)%m_exclude, pos=aa)) THEN
          self%cla(aa)%m_exclude = self%cla(a)%switch
        END IF
      END IF
    END IF
  END DO CLA_EXCLUDE
END SUBROUTINE check

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

!< Check if required CLAs are passed.

SUBROUTINE is_required_passed(self, pref)
  CLASS(CommandLineGroupArg_), INTENT(inout) :: self
  !! CLAsG data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref
  !! Prefixing string.
  INTEGER(I4P) :: a
  !! Counter.
  IF (self%is_called) THEN
    DO a = 1, self%Na
      IF (.NOT. self%cla(a)%is_required_passed(pref=pref)) THEN
        self%error = self%cla(a)%error
        WRITE (self%usage_lun, '(A)') self%usage(pref=pref)
        RETURN
      END IF
    END DO
  END IF
END SUBROUTINE is_required_passed

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

!< Check if a CLA has been passed.

PURE FUNCTION is_passed(self, switch, position)
  CLASS(CommandLineGroupArg_), INTENT(in) :: self
  !! CLAsG data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch
  !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position
  !! Position of positional CLA.
  LOGICAL :: is_passed
  !! Check if a CLA has been passed.
  INTEGER(I4P) :: a
  !! CLA counter.
  is_passed = .FALSE.
  IF (self%Na > 0) THEN
    IF (PRESENT(switch)) THEN
      DO a = 1, self%Na
        IF (.NOT. self%cla(a)%is_positional) THEN
     IF ((self%cla(a)%switch==switch).OR.(self%cla(a)%switch_ab==switch)) THEN
            is_passed = self%cla(a)%is_passed
            EXIT
          END IF
        END IF
      END DO
    ELSEIF (PRESENT(position)) THEN
      is_passed = self%cla(position)%is_passed
    END IF
  END IF
END FUNCTION is_passed

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

!< Check if a CLA has been defined.

FUNCTION is_defined(self, switch, pos)
  CLASS(CommandLineGroupArg_), INTENT(in) :: self
  !! CLAsG data.
  CHARACTER(*), INTENT(in) :: switch
  !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: pos
  !! CLA position.
  LOGICAL :: is_defined
  !! Check if a CLA has been defined.
  INTEGER(I4P) :: a
  !! CLA counter.
  is_defined = .FALSE.
  IF (PRESENT(pos)) pos = 0
  IF (self%Na > 0) THEN
    DO a = 1, self%Na
      IF (.NOT. self%cla(a)%is_positional) THEN
     IF ((self%cla(a)%switch==switch).OR.(self%cla(a)%switch_ab==switch)) THEN
          is_defined = .TRUE.
          IF (PRESENT(pos)) pos = a
          EXIT
        END IF
      END IF
    END DO
  END IF
END FUNCTION is_defined

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

!< Raise error mutually exclusive CLAs passed.

SUBROUTINE raise_error_m_exclude(self, pref)
  CLASS(CommandLineGroupArg_), INTENT(inout) :: self
  !! CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref
  !! Prefixing string.
  CALL self%errored(pref=pref, error=ERROR_M_EXCLUDE)
END SUBROUTINE raise_error_m_exclude

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-08
! update: 2021-11-08
! summary: Add
!
! Add CLA to CLAs list.
!
!@note
!If not otherwise declared the action on CLA value is set to "store"
!a value that must be passed after the switch name
!or directly passed in case of positional CLA.
!@endnote

SUBROUTINE add(self, pref, cla)
  CLASS(CommandLineGroupArg_), INTENT(inout) :: self
  !! CLAsG data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref
  !! Prefixing string.
  TYPE(CommandLineArg_), INTENT(in) :: cla
  !! CLA data.
  TYPE(CommandLineArg_), ALLOCATABLE :: cla_list_new(:)
  !! New (extended) CLA list.
  INTEGER(I4P) :: c
  !! Counter.
  IF (self%Na > 0_I4P) THEN
    IF (.NOT. cla%is_positional) THEN
      ALLOCATE (cla_list_new(1:self%Na + 1))
      DO c = 1, self%Na
        cla_list_new(c) = self%cla(c)
      END DO
      cla_list_new(self%Na + 1) = cla
    ELSE
      ALLOCATE (cla_list_new(1:self%Na + 1))
      DO c = 1, cla%position - 1
        cla_list_new(c) = self%cla(c)
      END DO
      cla_list_new(cla%position) = cla
      DO c = cla%position + 1, self%Na + 1
        cla_list_new(c) = self%cla(c - 1)
      END DO
    END IF
  ELSE
    ALLOCATE (cla_list_new(1:1))
    cla_list_new(1) = cla
  END IF
  CALL MOVE_ALLOC(from=cla_list_new, to=self%cla)
  self%Na = self%Na + 1
  IF (cla%is_required) THEN
    self%Na_required = self%Na_required + 1
  ELSE
    self%Na_optional = self%Na_optional + 1
  END IF
  IF (ALLOCATED(cla_list_new)) DEALLOCATE (cla_list_new)
  CALL self%check(pref=pref)
END SUBROUTINE add

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

!< Parse CLAsG arguments.

SUBROUTINE parse(self, args, ignore_unknown_clas, pref, error_unknown_clas)
  CLASS(CommandLineGroupArg_), INTENT(inout) :: self
  !! CLAsG data.
  CHARACTER(*), INTENT(in) :: args(:)
  !! Command line arguments.
  LOGICAL, INTENT(in) :: ignore_unknown_clas
  !! Disable errors-raising for passed unknown CLAs.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref
  !! Prefixing string.
  INTEGER(I4P), INTENT(out) :: error_unknown_clas
  !! Error flag for passed unknown CLAs.
  TYPE(CommandLineArg_) :: cla
  !! CLA data.
  CHARACTER(500) :: envvar
  !! Environment variables buffer.
  INTEGER(I4P) :: arg
  !! Argument counter.
  INTEGER(I4P) :: a
  !! Counter.
  INTEGER(I4P) :: aa
  !! Counter.
  INTEGER(I4P) :: aaa
  !! Counter.
  INTEGER(I4P) :: nargs
  !! Number of arguments consumed by a CLA.
  LOGICAL :: found
  !! Flag for checking if switch is a defined CLA.
  LOGICAL :: found_val
  !! Flag for checking if switch value is found.
  error_unknown_clas = 0
  IF (self%is_called) THEN
    arg = 0
    DO WHILE (arg < SIZE(args, dim=1)) ! loop over CLAs group arguments passed
      arg = arg + 1
      found = .FALSE.
      DO a = 1, self%Na ! loop over CLAs group clas named options
        IF (.NOT. self%cla(a)%is_positional) THEN
      IF (TRIM(ADJUSTL(self%cla(a)%switch)) == TRIM(ADJUSTL(args(arg))) .OR. &
        TRIM(ADJUSTL(self%cla(a)%switch_ab)) == TRIM(ADJUSTL(args(arg)))) THEN
            IF (self%cla(a)%is_passed) THEN
              ! current CLA has been already passed, raise an error
              CALL self%cla(arg)%raise_error_duplicated_clas(&
                   & pref=pref, switch=TRIM(ADJUSTL(args(arg))))
              self%error = self%cla(arg)%error
            END IF
            found_val = .FALSE.
            IF (self%cla(a)%act == action_store) THEN
              IF (ALLOCATED(self%cla(a)%envvar)) THEN
                IF (arg + 1 <= SIZE(args, dim=1)) THEN ! verify if the value has been passed directly to cli
                  ! there are still other arguments to check
                  IF (.NOT. self%is_defined( &
                       & switch=TRIM(ADJUSTL(args(arg + 1))))) THEN
                    ! argument seem good...
                    arg = arg + 1
                    self%cla(a)%val = TRIM(ADJUSTL(args(arg)))
                    found = .TRUE.
                    found_val = .TRUE.
                  END IF
                END IF
                IF (.NOT. found) THEN
                  ! not found, try to take val from environment
                  CALL get_environment_VARIABLE(name=self%cla(a)%envvar, &
                       &  VALUE=envvar, status=aa)
                  IF (aa == 0) THEN
                    self%cla(a)%val = TRIM(ADJUSTL(envvar))
                    found_val = .TRUE.
                  ELSE
                    ! flush default to val if environment is not &
                    ! & set and default is set
                    IF (ALLOCATED(self%cla(a)%def)) THEN
                      self%cla(a)%val = self%cla(a)%def
                      found_val = .TRUE.
                    END IF
                  END IF
                END IF
              ELSEIF (ALLOCATED(self%cla(a)%nargs)) THEN
                self%cla(a)%val = ''
                SELECT CASE (self%cla(a)%nargs)
                CASE ('+')
                  aaa = 0
                  DO aa = arg + 1, SIZE(args, dim=1)
                    IF (.NOT. self%is_defined( &
                         & switch=TRIM(ADJUSTL(args(aa))))) THEN
                      aaa = aa
                    ELSE
                      EXIT
                    END IF
                  END DO
                  IF (aaa >= arg + 1) THEN
                    DO aa = aaa, arg + 1, -1
                       !! decreasing loop due to gfortran bug
                      self%cla(a)%val = TRIM( &
                         & ADJUSTL(args(aa)))//args_sep//TRIM(self%cla(a)%val)
                      found_val = .TRUE.
                    END DO
                    arg = aaa
                  ELSEIF (aaa == 0) THEN
                    CALL self%cla(a)%raise_error_nargs_insufficient(pref=pref)
                    self%error = self%cla(a)%error
                    RETURN
                  END IF
                CASE ('*')
                  aaa = 0
                  DO aa = arg + 1, SIZE(args, dim=1)
                    IF (.NOT. self%is_defined( &
                         &  switch=TRIM(ADJUSTL(args(aa))))) THEN
                      aaa = aa
                    ELSE
                      EXIT
                    END IF
                  END DO
                  IF (aaa >= arg + 1) THEN
                    DO aa = aaa, arg + 1, -1
                       !! decreasing loop due to gfortran bug
                      self%cla(a)%val = TRIM( &
                         & ADJUSTL(args(aa)))//args_sep//TRIM(self%cla(a)%val)
                      found_val = .TRUE.
                    END DO
                    arg = aaa
                  END IF
                CASE default
                 nargs = cton(str=TRIM(ADJUSTL(self%cla(a)%nargs)), knd=1_I4P)
                  IF (arg + nargs > SIZE(args, dim=1)) THEN
                    CALL self%cla(a)%raise_error_nargs_insufficient(pref=pref)
                    self%error = self%cla(a)%error
                    RETURN
                  END IF
                  DO aa = arg + nargs, arg + 1, -1
                     !! decreasing loop due to gfortran bug
                    self%cla(a)%val = TRIM( &
                         & ADJUSTL(args(aa)))//args_sep//TRIM(self%cla(a)%val)
                  END DO
                  found_val = .TRUE.
                  arg = arg + nargs
                END SELECT
              ELSE
                IF (arg + 1 > SIZE(args)) THEN
                  CALL self%cla(a)%raise_error_value_missing(pref=pref)
                  self%error = self%cla(a)%error
                  RETURN
                END IF
                arg = arg + 1
                self%cla(a)%val = TRIM(ADJUSTL(args(arg)))
                found_val = .TRUE.
              END IF
            ELSEIF (self%cla(a)%act == action_store_star) THEN
              IF (arg + 1 <= SIZE(args, dim=1)) THEN
                 !! verify if the value has been passed directly to cli
                 !! there are still other arguments to check
                IF (.NOT. self%is_defined( &
                     & switch=TRIM(ADJUSTL(args(arg + 1))))) THEN
                  ! argument seem good...
                  arg = arg + 1
                  self%cla(a)%val = TRIM(ADJUSTL(args(arg)))
                  found = .TRUE.
                  found_val = .TRUE.
                END IF
              END IF
              IF (.NOT. found) THEN
                ! flush default to val if default is set
                IF (ALLOCATED(self%cla(a)%def)) &
                     & self%cla(a)%val = self%cla(a)%def
              END IF
            ELSEIF (self%cla(a)%act == action_print_help) THEN
              self%error = STATUS_PRINT_H
            ELSEIF (self%cla(a)%act == action_print_vers) THEN
              self%error = STATUS_PRINT_V
            END IF
            self%cla(a)%is_passed = .TRUE.
            found = .TRUE.
            EXIT
          END IF
        END IF
      END DO
      IF (.NOT. found) THEN
        !! current argument (arg-th) does not correspond to a named option
        IF (arg > self%Na) THEN
           !! has been passed too much CLAs
           !! place the error into a new positional dummy CLA
          CALL cla%assign_object(self)
          cla%is_passed = .TRUE.
          cla%m_exclude = ''
          CALL self%add(pref=pref, cla=cla)
          CALL self%cla(self%Na)%raise_error_switch_unknown( &
               & pref=pref, switch=TRIM(ADJUSTL(args(arg))))
          self%error = self%cla(self%Na)%error
          RETURN
        END IF
        IF (.NOT. self%cla(arg)%is_positional) THEN ! current argument (arg-th) is not positional... there is a problem!
          CALL self%cla(arg)%raise_error_switch_unknown( &
               & pref=pref, switch=TRIM(ADJUSTL(args(arg))))
          self%error = self%cla(arg)%error
          error_unknown_clas = self%error
          IF (.NOT. ignore_unknown_clas) RETURN
        ELSE
          ! positional CLA always stores a value
          self%cla(arg)%val = TRIM(ADJUSTL(args(arg)))
          self%cla(arg)%is_passed = .TRUE.
        END IF
      END IF
    END DO
    CALL self%check_m_exclusive(pref=pref)
    CALL self%sanitize_defaults
  END IF
END SUBROUTINE parse

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

!< Get correct CLAsG usage.

FUNCTION usage(self, pref, no_header, markdown)
  CLASS(CommandLineGroupArg_), INTENT(in) :: self
  !! CLAsG data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref
  !! Prefixing string.
  LOGICAL, OPTIONAL, INTENT(in) :: no_header
  !! Avoid insert header to usage.
  LOGICAL, OPTIONAL, INTENT(in) :: markdown
  !! Format things form markdown.
  CHARACTER(len=:), ALLOCATABLE :: usage
  !! Usage string.
  INTEGER(I4P) :: a
  !! Counters.
  CHARACTER(len=:), ALLOCATABLE :: prefd
  !! Prefixing string.
  LOGICAL :: markdownd
  !! Markdonw format, local variable.
  markdownd = .FALSE.; IF (PRESENT(markdown)) markdownd = markdown
  prefd = ''; IF (PRESENT(pref)) prefd = pref
  usage = self%progname; IF (self%group /= '') usage = self%progname//' '//self%group
  usage = prefd//self%help//' '//usage//self%signature()
  IF (self%description/='') usage = usage//new_LINE('a')//new_LINE('a')//prefd//self%description
  IF (PRESENT(no_header)) THEN
    IF (no_header) usage = ''
  END IF
  IF (self%Na_required > 0) THEN
    usage = usage//new_LINE('a')//new_LINE('a')//prefd//'Required switches:'
    DO a = 1, self%Na
      IF (self%cla(a)%is_required .AND. (.NOT. self%cla(a)%is_hidden)) &
           & usage = usage//new_LINE('a')// &
           & self%cla(a)%usage(pref=prefd, markdown=markdownd)
    END DO
  END IF
  IF (self%Na_optional > 0) THEN
    usage = usage//new_LINE('a')//new_LINE('a')//prefd//'Optional switches:'
    DO a = 1, self%Na
      IF (.NOT. self%cla(a)%is_required &
           & .AND. (.NOT. self%cla(a)%is_hidden)) &
           & usage = usage//new_LINE('a')// &
           & self%cla(a)%usage(pref=prefd, markdown=markdownd)
    END DO
  END IF
END FUNCTION usage

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

!< Get CLAsG signature.

FUNCTION signature(self, bash_completion, plain)
  CLASS(CommandLineGroupArg_), INTENT(in) :: self
  !! CLAsG data.
  LOGICAL, OPTIONAL, INTENT(in) :: bash_completion
  !! Return the signature for bash completion.
  LOGICAL, OPTIONAL, INTENT(in) :: plain
  !! Return the signature as plain switches list.
  ! logical                                         :: plain_
  !! Return the signature as plain switches list, local var.
  LOGICAL :: bash_completion_
  !! Return the signature for bash completion, local variable.
  CHARACTER(len=:), ALLOCATABLE :: signature
  !! Signature.
  ! logical                                         :: clas_choices
  !! Flag to check if there are CLAs with choices.
  INTEGER(I4P) :: a!, aa
  !! Counter.
  signature = ''
  bash_completion_ = .FALSE.
  IF (PRESENT(bash_completion)) bash_completion_ = bash_completion
  IF (bash_completion_) THEN
    DO a = 1, self%Na
      signature = signature//self%cla(a)%signature( &
           & bash_completion=bash_completion, plain=.TRUE.)
    END DO
    signature = new_LINE('a')//'    COMPREPLY=( $( compgen -W "' &
         & //signature//'" -- $cur ) )'
  END IF
  DO a = 1, self%Na
    signature = signature//self%cla(a)%signature(&
         & bash_completion=bash_completion, plain=plain)
  END DO
END FUNCTION signature

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

! private methods
SUBROUTINE errored(self, error, pref, a1, a2)
  !< Trig error occurrence and print meaningful message.
  CLASS(CommandLineGroupArg_), INTENT(inout) :: self  !< CLAsG data.
  INTEGER(I4P), INTENT(in) :: error !< Error occurred.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref  !< Prefixing string.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: a1    !< First index CLAs group inconsistent.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: a2    !< Second index CLAs group inconsistent.
  CHARACTER(len=:), ALLOCATABLE :: prefd !< Prefixing string.

  self%error = error
  IF (self%error /= 0) THEN
    prefd = ''; IF (PRESENT(pref)) prefd = pref
    prefd = prefd//self%progname//': '//colorize('error', &
         & color_fg=self%error_color, style=self%error_style)
    SELECT CASE (self%error)
    CASE (ERROR_CONSISTENCY)
      IF (self%group /= '') THEN
        self%error_message = prefd//': group (command) name: "' &
             & //self%group//'" consistency error:'
      ELSE
        self%error_message = prefd//': consistency error:'
      END IF
      self%error_message = self%error_message//' "'// &
           & TRIM(str(a1, .TRUE.))// &
           &  '-th" option has the same switch or abbreviated switch of "'// &
           & TRIM(str(a2, .TRUE.))//'-th" option:'//NEW_LINE('a')
      self%error_message = self%error_message//prefd//' CLA('// &
           & TRIM(str(a1, .TRUE.))//') switches = ' &
           & //self%cla(a1)%switch//' '//self%cla(a1)%switch_ab &
           & //NEW_LINE('a')
      self%error_message = self%error_message//prefd//' CLA('// &
           & TRIM(str(a2, .TRUE.))//') switches = '// &
           & self%cla(a2)%switch//' '// &
           & self%cla(a2)%switch_ab
    CASE (ERROR_M_EXCLUDE)
      self%error_message = prefd//': the group "'// &
           & self%group//'" and "'//self%m_exclude// &
           & '" are mutually'// &
           & ' exclusive, but both have been called!'
    END SELECT
    CALL self%print_error_message
  END IF
END SUBROUTINE errored

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

SUBROUTINE check_m_exclusive(self, pref)
  !< Check if two mutually exclusive CLAs have been passed.
  CLASS(CommandLineGroupArg_), INTENT(inout) :: self !< CLAsG data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref !< Prefixing string.
  INTEGER(I4P) :: a    !< Counter.

  IF (self%is_called) THEN
    DO a = 1, self%Na
      IF (self%cla(a)%is_passed) THEN
        IF (self%cla(a)%m_exclude /= '') THEN
          IF (self%is_passed(switch=self%cla(a)%m_exclude)) THEN
            CALL self%cla(a)%raise_error_m_exclude(pref=pref)
            self%error = self%cla(a)%error
            RETURN
          END IF
        END IF
      END IF
    END DO
  END IF
END SUBROUTINE check_m_exclusive

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

!< Sanitize defaults values.
!<
!< It is necessary to *sanitize* the default values of non-passed, optional CLAs.

SUBROUTINE sanitize_defaults(self)
  CLASS(CommandLineGroupArg_), INTENT(inout) :: self !< CLAsG data.
  INTEGER(I4P) :: a    !< Counter.

  IF (self%is_called) THEN
    DO a = 1, self%Na
      CALL self%cla(a)%sanitize_defaults
    END DO
  END IF
END SUBROUTINE sanitize_defaults

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

!< Assignment operator.

ELEMENTAL SUBROUTINE clasg_assign_clasg(lhs, rhs)
  CLASS(CommandLineGroupArg_), INTENT(INOUT) :: lhs
  !! Left hand side.
  TYPE(CommandLineGroupArg_), INTENT(IN) :: rhs
  !! Right hand side.
  ! object members
  CALL lhs%assign_object(rhs)
  ! CommandLineGroupArg_ members
  IF (ALLOCATED(rhs%group)) lhs%group = rhs%group
  IF (ALLOCATED(rhs%cla)) THEN
    IF (ALLOCATED(lhs%cla)) DEALLOCATE (lhs%cla)
    ALLOCATE (lhs%cla(1:SIZE(rhs%cla, dim=1)), source=rhs%cla)
  END IF
  lhs%Na = rhs%Na
  lhs%Na_required = rhs%Na_required
  lhs%Na_optional = rhs%Na_optional
  lhs%is_called = rhs%is_called
END SUBROUTINE clasg_assign_clasg

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

ELEMENTAL SUBROUTINE finalize(self)
  !< Free dynamic memory when finalizing.
  TYPE(CommandLineGroupArg_), INTENT(inout) :: self !< CLAsG data.
  CALL self%free
END SUBROUTINE finalize

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

ENDMODULE CommandLineGroupArg_Class
