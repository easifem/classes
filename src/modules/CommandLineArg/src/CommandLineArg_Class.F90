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
! summary: Command Line Arguments

MODULE CommandLineArg_Class
USE FACE, ONLY: colorize
USE AbstractCommandLine_Class, ONLY: AbstractCommandLineArg_
USE CommandLineArg_Utils
USE PENF
IMPLICIT NONE
PRIVATE
SAVE
! parameters
CHARACTER(len=*), Public, PARAMETER :: ACTION_STORE = 'STORE'
!! Store value (if invoked a value must be passed).
CHARACTER(len=*), Public, PARAMETER :: ACTION_STORE_STAR = 'STORE*'
!! Store value or revert on default if invoked alone.
CHARACTER(len=*), Public, PARAMETER :: ACTION_STORE_TRUE = 'STORE_TRUE'
!! Store .true. without the necessity of a value.
CHARACTER(len=*), Public, PARAMETER :: ACTION_STORE_FALSE = 'STORE_FALSE'
!! Store .false. without the necessity of a value.
CHARACTER(len=*), Public, PARAMETER :: ACTION_PRINT_HELP = 'PRINT_HELP'
!! Print help message.
CHARACTER(len=*), Public, PARAMETER :: ACTION_PRINT_VERS = 'PRINT_VERSION'
!! Print version.
CHARACTER(len=*), Public, PARAMETER :: ARGS_SEP = '||!||'
!! Arguments separator for multiple valued (list) CLA.
! errors codes
INTEGER(I4P), PARAMETER :: ERROR_OPTIONAL_NO_DEF = 1
!! Optional CLA without default value.
INTEGER(I4P), PARAMETER :: ERROR_REQUIRED_M_EXCLUDE = 2
!! Required CLA cannot exclude others.
INTEGER(I4P), PARAMETER :: ERROR_POSITIONAL_M_EXCLUDE = 3
!! Positional CLA cannot exclude others.
INTEGER(I4P), PARAMETER :: ERROR_NAMED_NO_NAME = 4
!! Named CLA without switch name.
INTEGER(I4P), PARAMETER :: ERROR_POSITIONAL_NO_POSITION = 5
!! Positional CLA without position.
INTEGER(I4P), PARAMETER :: ERROR_POSITIONAL_NO_STORE = 6
!! Positional CLA without action_store.
INTEGER(I4P), PARAMETER :: ERROR_NOT_IN_CHOICES = 7
!! CLA value out of a specified choices.
INTEGER(I4P), PARAMETER :: ERROR_MISSING_REQUIRED = 8
!! Missing required CLA.
INTEGER(I4P), PARAMETER :: ERROR_M_EXCLUDE = 9
!! Two mutually exclusive CLAs have been passed.
INTEGER(I4P), PARAMETER :: ERROR_CASTING_LOGICAL = 10
!! Error casting CLA value to logical type.
INTEGER(I4P), PARAMETER :: ERROR_CHOICES_LOGICAL = 11
!! Error adding choices check for CLA val of logical type.
INTEGER(I4P), PARAMETER :: ERROR_NO_LIST = 12
!! Actual CLA is not list-values.
INTEGER(I4P), PARAMETER :: ERROR_NARGS_INSUFFICIENT = 13
!! Multi-valued CLA with insufficient arguments.
INTEGER(I4P), PARAMETER :: ERROR_VALUE_MISSING = 14
!! Missing value of CLA.
INTEGER(I4P), PUBLIC, PARAMETER :: ERROR_UNKNOWN = 15
!! Unknown CLA (switch name).
INTEGER(I4P), PARAMETER :: ERROR_ENVVAR_POSITIONAL = 16
!! Envvar not allowed for positional CLA.
INTEGER(I4P), PARAMETER :: ERROR_ENVVAR_NOT_STORE = 17
!! Envvar not allowed action different from store;
INTEGER(I4P), PARAMETER :: ERROR_ENVVAR_NARGS = 18
!! Envvar not allowed for list-values CLA.
INTEGER(I4P), PARAMETER :: ERROR_STORE_STAR_POSITIONAL = 19
!! Action store* not allowed for positional CLA.
INTEGER(I4P), PARAMETER :: ERROR_STORE_STAR_NARGS = 20
!! Action store* not allowed for list-values CLA.
INTEGER(I4P), PARAMETER :: ERROR_STORE_STAR_ENVVAR = 21
!! Action store* not allowed for environment variable CLA.
INTEGER(I4P), PARAMETER :: ERROR_ACTION_UNKNOWN = 22
!! Unknown CLA (switch name).
INTEGER(I4P), PARAMETER :: ERROR_DUPLICATED_CLAS = 23
!! Duplicated CLAs passed, passed multiple instance of the same CLA.

!----------------------------------------------------------------------------
!                                                            CommandLineArg_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-08
! update: 2021-11-08
! summary: Command line argument class
!
!{!pages/CommandLineArg_.md}
!
TYPE, EXTENDS(AbstractCommandLineArg_) :: CommandLineArg_
  PRIVATE
  CHARACTER(len=:), ALLOCATABLE, PUBLIC :: switch
  !! Switch name.
  CHARACTER(len=:), ALLOCATABLE, PUBLIC :: switch_ab
  !! Abbreviated switch name.
  LOGICAL, PUBLIC :: is_required = .FALSE.
  !! Flag for set required argument.
  LOGICAL, PUBLIC :: is_positional = .FALSE.
  !! Flag for checking if CLA is a positional or a named CLA.
  INTEGER(I4P), PUBLIC :: position = 0_I4P
  !! Position of positional CLA.
  LOGICAL, PUBLIC :: is_passed = .FALSE.
  !! Flag for checking if CLA has been passed to CLI.
  LOGICAL, PUBLIC :: is_hidden = .FALSE.
  !! Flag for hiding CLA, thus it does not compare into help.
  CHARACTER(len=:), ALLOCATABLE, PUBLIC :: act
  !! CLA value action.
  CHARACTER(len=:), ALLOCATABLE, PUBLIC :: def
  !! Default value.
  CHARACTER(len=:), ALLOCATABLE, PUBLIC :: nargs
  !! Number of arguments consumed by CLA.
  CHARACTER(len=:), ALLOCATABLE, PUBLIC :: choices
  !! List (comma separated) of allowable values for the argument.
  CHARACTER(len=:), ALLOCATABLE, PUBLIC :: val
  !! CLA value.
  CHARACTER(len=:), ALLOCATABLE, PUBLIC :: envvar
  !! Environment variable from which take value.
CONTAINS
  ! public methods
  PROCEDURE, PUBLIC :: free
  !! Free dynamic memory.
  PROCEDURE, PUBLIC :: check
  !! Check data consistency.
  PROCEDURE, PUBLIC :: is_required_passed
  !! Check if required CLA is passed.
  PROCEDURE, PUBLIC :: raise_error_m_exclude
  !! Raise error mutually exclusive CLAs passed.
  PROCEDURE, PUBLIC :: raise_error_nargs_insufficient
  !! Raise error insufficient number of argument values passed.
  PROCEDURE, PUBLIC :: raise_error_value_missing
  !! Raise error missing value.
  PROCEDURE, PUBLIC :: raise_error_switch_unknown
  !! Raise error switch_unknown.
  PROCEDURE, PUBLIC :: raise_error_duplicated_clas
  !! Raise error duplicated CLAs passed.
  GENERIC, PUBLIC :: get => &
    get_cla, &
    get_cla_list
  !! Get CLA value(s).
  GENERIC, PUBLIC :: get_varying => &
#if defined USE_Real128
    get_cla_list_varying_R16P, &
#endif
    get_cla_list_varying_R8P, &
    get_cla_list_varying_R4P, &
    get_cla_list_varying_I8P, &
    get_cla_list_varying_I4P, &
    get_cla_list_varying_I2P, &
    get_cla_list_varying_I1P, &
    get_cla_list_varying_logical, &
    get_cla_list_varying_char
  !! Get CLA value(s) from varying size list.
  PROCEDURE, PUBLIC :: has_choices
  !! Return true if CLA has defined choices.
  PROCEDURE, PUBLIC :: sanitize_defaults
  !! Sanitize default values.
  PROCEDURE, PUBLIC :: signature
  !! Get signature.
  PROCEDURE, PUBLIC :: usage
  !! Get correct usage.
  ! private methods
  PROCEDURE, PRIVATE :: errored
  !! Trig error occurence and print meaningful message.
  PROCEDURE, PRIVATE :: check_envvar_consistency
  !! Check data consistency for envvar CLA.
  PROCEDURE, PRIVATE :: check_action_consistency
  !! Check CLA action consistency.
  PROCEDURE, PRIVATE :: check_optional_consistency
  !! Check optional CLA consistency.
  PROCEDURE, PRIVATE :: check_m_exclude_consistency
  !! Check mutually exclusion consistency.
  PROCEDURE, PRIVATE :: check_named_consistency
  !! Check named CLA consistency.
  PROCEDURE, PRIVATE :: check_positional_consistency
  !! Check positional CLA consistency.
  PROCEDURE, PRIVATE :: check_choices
  !! Check if CLA value is in allowed choices.
  PROCEDURE, PRIVATE :: check_list_size
  !! Check CLA multiple values list size consistency.
  PROCEDURE, PRIVATE :: get_cla
  !! Get CLA (single) value.
  PROCEDURE, PRIVATE :: get_cla_from_buffer
  !! Get CLA (single) value from a buffer.
  PROCEDURE, PRIVATE :: get_cla_list
  !! Get CLA multiple values.
  PROCEDURE, PRIVATE :: get_cla_list_from_buffer
  !! Get CLA (single) value from a buffer.
  PROCEDURE, PRIVATE :: get_cla_list_varying_R16P
  !! Get CLA multiple values, varying size, R16P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_R8P
  !! Get CLA multiple values, varying size, R8P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_R4P
  !! Get CLA multiple values, varying size, R4P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_I8P
  !! Get CLA multiple values, varying size, I8P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_I4P
  !! Get CLA multiple values, varying size, I4P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_I2P
  !! Get CLA multiple values, varying size, I2P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_I1P
  !! Get CLA multiple values, varying size, I1P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_logical
  !! Get CLA multiple values, varying size, bool.
  PROCEDURE, PRIVATE :: get_cla_list_varying_char
  !! Get CLA multiple values, varying size, char.
  PROCEDURE, PRIVATE :: cla_assign_cla
  !! Assignment operator.
  GENERIC, PRIVATE :: ASSIGNMENT(=) => cla_assign_cla
  !! Assignment operator overloading.
  FINAL :: finalize
  !! Free dynamic memory when finalizing.
END TYPE CommandLineArg_

PUBLIC :: CommandLineArg_

!----------------------------------------------------------------------------
!                                                                  Contains
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Free
!----------------------------------------------------------------------------

ELEMENTAL SUBROUTINE free(self)
  !< Free dynamic memory.
  CLASS(CommandLineArg_), INTENT(inout) :: self !< CLA data.
  ! object members
  CALL self%free_object
  ! CommandLineArg_ members
  IF (ALLOCATED(self%switch)) DEALLOCATE (self%switch)
  IF (ALLOCATED(self%switch_ab)) DEALLOCATE (self%switch_ab)
  IF (ALLOCATED(self%act)) DEALLOCATE (self%act)
  IF (ALLOCATED(self%def)) DEALLOCATE (self%def)
  IF (ALLOCATED(self%nargs)) DEALLOCATE (self%nargs)
  IF (ALLOCATED(self%choices)) DEALLOCATE (self%choices)
  IF (ALLOCATED(self%val)) DEALLOCATE (self%val)
  IF (ALLOCATED(self%envvar)) DEALLOCATE (self%envvar)
  self%is_required = .FALSE.
  self%is_positional = .FALSE.
  self%position = 0_I4P
  self%is_passed = .FALSE.
  self%is_hidden = .FALSE.
END SUBROUTINE free

!----------------------------------------------------------------------------
!                                                                  check
!----------------------------------------------------------------------------

SUBROUTINE check(self, pref)
  !< Check data consistency.
  CLASS(CommandLineArg_), INTENT(inout) :: self  !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref  !< Prefixing string.
  CALL self%check_envvar_consistency(pref=pref); IF (self%error /= 0) RETURN
  CALL self%check_action_consistency(pref=pref); IF (self%error /= 0) RETURN
  CALL self%check_optional_consistency(pref=pref); IF (self%error /= 0) RETURN
  CALL self%check_m_exclude_consistency(pref=pref); IF (self%error /= 0) RETURN
  CALL self%check_named_consistency(pref=pref); IF (self%error /= 0) RETURN
  CALL self%check_positional_consistency(pref=pref)
END SUBROUTINE check

!----------------------------------------------------------------------------
!                                                          is_required_passed
!----------------------------------------------------------------------------

FUNCTION is_required_passed(self, pref) RESULT(is_ok)
  !< Check if required CLA is passed.
  CLASS(CommandLineArg_), INTENT(inout) :: self  !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref  !< Prefixing string.
  LOGICAL :: is_ok !< Check result.
  is_ok = .TRUE.
  IF (((.NOT.self%is_passed).AND.self%is_required).OR.((.NOT.self%is_passed).AND.(.NOT.ALLOCATED(self%def)))) THEN
    CALL self%errored(pref=pref, error=ERROR_MISSING_REQUIRED)
    is_ok = .FALSE.
  END IF
END FUNCTION is_required_passed

!----------------------------------------------------------------------------
!                                                      raise_error_m_exclude
!----------------------------------------------------------------------------

SUBROUTINE raise_error_m_exclude(self, pref)
  !< Raise error mutually exclusive CLAs passed.
  CLASS(CommandLineArg_), INTENT(inout) :: self !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref !< Prefixing string.

  CALL self%errored(pref=pref, error=ERROR_M_EXCLUDE)
END SUBROUTINE raise_error_m_exclude

SUBROUTINE raise_error_nargs_insufficient(self, pref)
  !< Raise error insufficient number of argument values passed.
  CLASS(CommandLineArg_), INTENT(inout) :: self !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref !< Prefixing string.

  CALL self%errored(pref=pref, error=ERROR_NARGS_INSUFFICIENT)
END SUBROUTINE raise_error_nargs_insufficient

SUBROUTINE raise_error_value_missing(self, pref)
  !< Raise error missing value.
  CLASS(CommandLineArg_), INTENT(inout) :: self !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref !< Prefixing string.

  CALL self%errored(pref=pref, error=ERROR_VALUE_MISSING)
END SUBROUTINE raise_error_value_missing

SUBROUTINE raise_error_switch_unknown(self, switch, pref)
  !< Raise error switch_unknown.
  CLASS(CommandLineArg_), INTENT(inout) :: self   !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch !< CLA switch name.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref   !< Prefixing string.

  CALL self%errored(pref=pref, error=ERROR_UNKNOWN, switch=switch)
END SUBROUTINE raise_error_switch_unknown

SUBROUTINE raise_error_duplicated_clas(self, switch, pref)
  !< Raise error duplicated CLAs passed.
  CLASS(CommandLineArg_), INTENT(inout) :: self   !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch !< CLA switch name.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref   !< Prefixing string.

  CALL self%errored(pref=pref, error=ERROR_DUPLICATED_CLAS, switch=switch)
END SUBROUTINE raise_error_duplicated_clas

SUBROUTINE sanitize_defaults(self)
  !< Sanitize defaults values.
  !<
  !< It is necessary to *sanitize* the default values of non-passed, optional CLA.
  CLASS(CommandLineArg_), INTENT(inout) :: self !< CLAsG data.

  IF (.NOT. self%is_passed) THEN
    IF (ALLOCATED(self%def)) THEN
      ! strip leading and trailing white spaces
      self%def = wstrip(self%def)
      IF (ALLOCATED(self%nargs)) THEN
        ! replace white space separator with FLAP ARGS_SEP
        self%def = unique(string=self%def, substring=' ')
     self%def = replace_all(string=self%def, substring=' ', restring=ARGS_SEP)
      END IF
    END IF
  END IF
END SUBROUTINE sanitize_defaults

FUNCTION usage(self, pref, markdown)
  !< Get correct usage.
  CLASS(CommandLineArg_), INTENT(in) :: self       !< CLAs group data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref       !< Prefixing string.
  LOGICAL, OPTIONAL, INTENT(in) :: markdown   !< Format for markdown
  CHARACTER(len=:), ALLOCATABLE :: usage      !< Usage string.
  CHARACTER(len=:), ALLOCATABLE :: prefd      !< Prefixing string.
  CHARACTER(len=:), ALLOCATABLE :: switch_    !< Switch name, local variable.
  CHARACTER(len=:), ALLOCATABLE :: switch_ab_ !< Abbreviated switch name, local variable.
  INTEGER(I4P) :: a          !< Counter.
  LOGICAL :: markdownd  !< Format for markdown
  INTEGER :: indent     !< how many spaces to indent

  markdownd = .FALSE.; IF (PRESENT(markdown)) markdownd = markdown
  indent = 4
  switch_ = colorize(TRIM(ADJUSTL(self%switch)), color_fg=self%help_color, style=self%help_style)
  switch_ab_ = colorize(TRIM(ADJUSTL(self%switch_ab)), color_fg=self%help_color, style=self%help_style)
  IF (.NOT. self%is_hidden) THEN
    IF (self%act == action_store) THEN
      IF (.NOT. self%is_positional) THEN
        IF (ALLOCATED(self%nargs)) THEN
          usage = ''
          SELECT CASE (self%nargs)
          CASE ('+')
            usage = usage//' value#1 [value#2...]'
          CASE ('*')
            usage = usage//' [value#1 value#2...]'
          CASE default
            DO a = 1, cton(str=TRIM(ADJUSTL(self%nargs)), knd=1_I4P)
              usage = usage//' value#'//TRIM(str(a, .TRUE.))
            END DO
          END SELECT
         IF (TRIM(ADJUSTL(self%switch)) /= TRIM(ADJUSTL(self%switch_ab))) THEN
            IF (markdownd) THEN
              usage = new_LINE('a')//'* `'//TRIM(ADJUSTL(self%switch))//usage//'`, `'//TRIM(ADJUSTL(self%switch_ab))//usage//'`'
            ELSE
              usage = '   '//switch_//usage//', '//switch_ab_//usage
            END IF
          ELSE
            IF (markdownd) THEN
          usage = new_LINE('a')//'* `'//TRIM(ADJUSTL(self%switch))//usage//'`'
            ELSE
              usage = '   '//switch_//usage
            END IF
          END IF
        ELSE
         IF (TRIM(ADJUSTL(self%switch)) /= TRIM(ADJUSTL(self%switch_ab))) THEN
            IF (markdownd) THEN
              usage = new_LINE('a')//'* `'//TRIM(ADJUSTL(self%switch))//' value`, `'//TRIM(ADJUSTL(self%switch_ab))//' value'//'`'
            ELSE
              usage = '   '//switch_//' value, '//switch_ab_//' value'
            END IF
          ELSE
            IF (markdownd) THEN
           usage = new_LINE('a')//'* `'//TRIM(ADJUSTL(self%switch))//' value`'
            ELSE
              usage = '   '//switch_//' value'
            END IF
          END IF
        END IF
      ELSE
        usage = '  value'
      END IF
      IF (ALLOCATED(self%choices)) THEN
        usage = usage//', value in: `'//self%choices//'`'
      END IF
    ELSEIF (self%act == action_store_star) THEN
      usage = '  [value]'
      IF (ALLOCATED(self%choices)) THEN
        usage = usage//', value in: ('//self%choices//')'
      END IF
    ELSE
      IF (TRIM(ADJUSTL(self%switch)) /= TRIM(ADJUSTL(self%switch_ab))) THEN
        IF (markdownd) THEN
          usage = new_LINE('a')//'* `'//TRIM(ADJUSTL(self%switch))//'`, `'//TRIM(ADJUSTL(self%switch_ab))//'`'
        ELSE
          usage = '   '//switch_//', '//switch_ab_
        END IF
      ELSE
        IF (markdownd) THEN
          usage = new_LINE('a')//'* `'//TRIM(ADJUSTL(self%switch))//'`'
        ELSE
          usage = '   '//switch_
        END IF
      END IF
    END IF
    prefd = ''; IF (PRESENT(pref)) prefd = pref
    usage = prefd//usage
    IF (self%is_positional) usage = usage//new_LINE('a')//prefd//REPEAT(' ',indent)//TRIM(str(self%position, .TRUE.))//&
                                    '-th argument'
    IF (ALLOCATED(self%envvar)) THEN
      IF (self%envvar /= '') THEN
        usage = usage//new_LINE('a')//prefd//REPEAT(' ',10)//'environment variable name "'//TRIM(ADJUSTL(self%envvar))//'"'
      END IF
    END IF
    IF (.NOT. self%is_required) THEN
      IF (self%def /= '') THEN
        IF (markdownd) THEN
          ! two spaces make a line break in markdown.
          usage = usage//'  '//new_LINE('a')//prefd//REPEAT(' ', 4)//'default value '//TRIM(replace_all(self%def,ARGS_SEP,' '))
        ELSE
          usage = usage//new_LINE('a')//prefd//REPEAT(' ', indent)//'default value '//TRIM(replace_all(self%def,ARGS_SEP,' '))
        END IF
      END IF
    END IF
    IF (self%m_exclude/='') usage = usage//new_LINE('a')//prefd//REPEAT(' ', indent)//'mutually exclude "'//self%m_exclude//'"'
    IF (markdownd) THEN
      usage = usage//'  '//new_LINE('a')//prefd//REPEAT(' ',4)//TRIM(ADJUSTL(self%help))
      IF (self%help_markdown /= '') THEN
        usage = usage//TRIM(ADJUSTL(self%help_markdown))
      END IF
    ELSE
      usage = usage//new_LINE('a')//prefd//REPEAT(' ', indent)//TRIM(ADJUSTL(self%help))
    END IF
  ELSE
    usage = ''
  END IF
END FUNCTION usage

FUNCTION signature(self, bash_completion, plain)
  !< Get signature.
  CLASS(CommandLineArg_), INTENT(in) :: self             !< CLA data.
  LOGICAL, OPTIONAL, INTENT(in) :: bash_completion  !< Return the signature for bash completion.
  LOGICAL, OPTIONAL, INTENT(in) :: plain            !< Return the signature as plain switches list.
  LOGICAL :: plain_           !< Return the signature as plain switches list, local var.
  LOGICAL :: bash_completion_ !< Return the signature for bash completion, local variable.
  CHARACTER(len=:), ALLOCATABLE :: signature        !< Signature.
  INTEGER(I4P) :: nargs            !< Number of arguments consumed by CLA.
  INTEGER(I4P) :: a                !< Counter.

  bash_completion_ = .FALSE.; IF (PRESENT(bash_completion)) bash_completion_ = bash_completion
  plain_ = .FALSE.; IF (PRESENT(plain)) plain_ = plain
  IF (.NOT. self%is_hidden) THEN
    IF (bash_completion_) THEN
      IF (.NOT. self%is_positional) THEN
        IF (plain_) THEN
         IF (TRIM(ADJUSTL(self%switch)) /= TRIM(ADJUSTL(self%switch_ab))) THEN
            signature = ' '//TRIM(ADJUSTL(self%switch))//' '//TRIM(ADJUSTL(self%switch_ab))
          ELSE
            signature = ' '//TRIM(ADJUSTL(self%switch))
          END IF
        ELSE
          signature = new_LINE('a')//'    if [ "$prev" == "'//self%switch//'" ] || [ "$prev" == "'//self%switch_ab//'" ] ; then'
          IF (self%has_choices()) THEN
             signature = signature//new_LINE('a')//'       COMPREPLY=( $( compgen -W "'//choices(self%choices)//'" -- $cur ) )'
 ELSEIF ((self%act == action_store) .OR. (self%act == action_store_star)) THEN
            signature = signature//new_LINE('a')//'       COMPREPLY=( )'
          END IF
          signature = signature//new_LINE('a')//'       return 0'
          signature = signature//new_LINE('a')//'    fi'
        END IF
        ! if (trim(adjustl(self%switch))/=trim(adjustl(self%switch_ab))) then
        ! if (plain_) then
        !   signature = ' "'//trim(adjustl(self%switch))//'" "'//trim(adjustl(self%switch_ab))//'"'
        ! else
        ! signature = ' '//trim(adjustl(self%switch))//' '//trim(adjustl(self%switch_ab))
        ! endif
        ! else
        ! if (plain_) then
        !   signature = ' "'//trim(adjustl(self%switch))//'"'
        ! else
        ! signature = ' '//trim(adjustl(self%switch))
        ! endif
        ! endif
      END IF
    ELSE
      IF (self%act == action_store) THEN
        IF (.NOT. self%is_positional) THEN
          IF (ALLOCATED(self%nargs)) THEN
            SELECT CASE (self%nargs)
            CASE ('+')
              signature = ' value#1 [value#2 value#3...]'
            CASE ('*')
              signature = ' [value#1 value#2 value#3...]'
            CASE default
              nargs = cton(str=TRIM(ADJUSTL(self%nargs)), knd=1_I4P)
              signature = ''
              DO a = 1, nargs
                signature = signature//' value#'//TRIM(str(a, .TRUE.))
              END DO
            END SELECT
          ELSE
            signature = ' value'
          END IF
          IF (self%is_required) THEN
            signature = ' '//TRIM(ADJUSTL(self%switch))//signature
          ELSE
            signature = ' ['//TRIM(ADJUSTL(self%switch))//signature//']'
          END IF
        ELSE
          IF (self%is_required) THEN
            signature = ' value'
          ELSE
            signature = ' [value]'
          END IF
        END IF
      ELSEIF (self%act == action_store_star) THEN
        signature = ' [value]'
      ELSE
        IF (self%is_required) THEN
          signature = ' '//TRIM(ADJUSTL(self%switch))
        ELSE
          signature = ' ['//TRIM(ADJUSTL(self%switch))//']'
        END IF
      END IF
    END IF
  ELSE
    signature = ''
  END IF
CONTAINS
  PURE FUNCTION choices(choices_c)
    !< Return space-separated choices list from a comma-separated one.
    CHARACTER(len=*), INTENT(in) :: choices_c !< Comma-separated list of choices.
    CHARACTER(len=LEN(choices_c)) :: choices   !< Space-separated list of choices.
    INTEGER(I4P) :: c         !< Counter.

    choices = choices_c
    DO c = 1, LEN(choices)
      IF (choices(c:c) == ',') choices(c:c) = ' '
    END DO
  END FUNCTION choices
END FUNCTION signature

PURE FUNCTION has_choices(self)
  !< Return true if CLA has choices.
  CLASS(CommandLineArg_), INTENT(in) :: self        !< CLA data.
  LOGICAL :: has_choices !< Check result.

  has_choices = ALLOCATED(self%choices)
END FUNCTION has_choices

! private methods
SUBROUTINE errored(self, error, pref, switch, val_str, log_value)
  !< Trig error occurence and print meaningful message.
  CLASS(CommandLineArg_), INTENT(inout) :: self      !< CLA data.
  INTEGER(I4P), INTENT(in) :: error     !< Error occurred.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref      !< Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch    !< CLA switch name.
  CHARACTER(*), OPTIONAL, INTENT(in) :: val_str   !< Value string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: log_value !< Logical value to be casted.
  CHARACTER(len=:), ALLOCATABLE :: prefd     !< Prefixing string.

  self%error = error
  IF (self%error /= 0) THEN
    prefd = ''; IF (PRESENT(pref)) prefd = pref
    prefd = prefd//self%progname//': '//colorize('error', color_fg=self%error_color, style=self%error_style)
    SELECT CASE (self%error)
    CASE (ERROR_OPTIONAL_NO_DEF)
      IF (self%is_positional) THEN
        self%error_message = prefd//': "'//TRIM(str(n=self%position))//'-th" positional option has not a default value!'
      ELSE
        self%error_message = prefd//': named option "'//self%switch//'" has not a default value!'
      END IF
    CASE (ERROR_REQUIRED_M_EXCLUDE)
      self%error_message = prefd//': named option "'//self%switch//'" cannot exclude others'//&
                           ', it being required, only optional ones can!'
    CASE (ERROR_POSITIONAL_M_EXCLUDE)
      self%error_message = prefd//': "'//TRIM(str(n=self%position))// &
                           '-th" positional option cannot exclude others, only optional named options can!'
    CASE (ERROR_NAMED_NO_NAME)
      self%error_message = prefd//': a non positional optiona must have a switch name!'
    CASE (ERROR_POSITIONAL_NO_POSITION)
      self%error_message = prefd//': a positional option must have a position number different from 0!'
    CASE (ERROR_POSITIONAL_NO_STORE)
      self%error_message = prefd//': a positional option must have action set to "'//action_store//'"!'
    CASE (ERROR_M_EXCLUDE)
      self%error_message = prefd//': the options "'//self%switch//'" and "'//self%m_exclude//&
                        '" are mutually exclusive, but both have been passed!'
    CASE (ERROR_NOT_IN_CHOICES)
      IF (self%is_positional) THEN
    self%error_message = prefd//': value of "'//TRIM(str(n=self%position))// &
                             '-th" positional option must be chosen in:'
      ELSE
        self%error_message = prefd//': value of named option "'//self%switch//'" must be chosen in: '
      END IF
      self%error_message = self%error_message//'('//self%choices//')'
      self%error_message = self%error_message//' but "'//TRIM(val_str)//'" has been passed!'
    CASE (ERROR_MISSING_REQUIRED)
      IF (.NOT. self%is_positional) THEN
        self%error_message = prefd//': named option "'//TRIM(ADJUSTL(self%switch))//'" is required!'
      ELSE
        self%error_message = prefd//': "'//TRIM(str(self%position, .TRUE.))//'-th" positional option is required!'
      END IF
    CASE (ERROR_CASTING_LOGICAL)
      self%error_message = prefd//': cannot convert "'//log_value//'" of option "'//self%switch//'" to logical type!'
    CASE (ERROR_CHOICES_LOGICAL)
      self%error_message = prefd//': cannot use "choices" value check for option "'//self%switch//&
                           '" it being of logical type! The choices are limited to ".true." or ".false." by definition!'
    CASE (ERROR_NO_LIST)
      IF (.NOT. self%is_positional) THEN
self%error_message = prefd//': named option "'//TRIM(ADJUSTL(self%switch))// &
       '" has not "nargs" value but an array has been passed to "get" method!'
      ELSE
        self%error_message = prefd//': "'//TRIM(str(self%position, .TRUE.))//'-th" positional option '//&
         'has not "nargs" value but an array has been passed to "get" method!'
      END IF
    CASE (ERROR_NARGS_INSUFFICIENT)
      IF (.NOT. self%is_positional) THEN
        IF (self%nargs == '+') THEN
self%error_message = prefd//': named option "'//TRIM(ADJUSTL(self%switch))// &
                          '" requires at least 1 argument but no one remains!'
        ELSE
          self%error_message = prefd//': named option "'//TRIM(ADJUSTL(self%switch))//'" requires '//&
            TRIM(ADJUSTL(self%nargs))//' arguments but no enough ones remain!'
        END IF
      ELSE
        IF (self%nargs == '+') THEN
       self%error_message = prefd//': "'//TRIM(str(self%position, .TRUE.))// &
      '-th" positional option requires at least 1 argument but no one remains'
        ELSE
          self%error_message = prefd//': "'//TRIM(str(self%position, .TRUE.))//'-th" positional option requires '//&
            TRIM(ADJUSTL(self%nargs))//' arguments but no enough ones remain!'
        END IF
      END IF
    CASE (ERROR_VALUE_MISSING)
      self%error_message = prefd//': named option "'//TRIM(ADJUSTL(self%switch))//'" needs a value that is not passed!'
    CASE (ERROR_UNKNOWN)
      self%error_message = prefd//': switch "'//TRIM(ADJUSTL(switch))//'" is unknown!'
    CASE (ERROR_ENVVAR_POSITIONAL)
      self%error_message = prefd//': "'//TRIM(str(self%position, .TRUE.))//'-th" positional option '//&
               'has "envvar" value that is not allowed for positional option!'
    CASE (ERROR_ENVVAR_NOT_STORE)
self%error_message = prefd//': named option "'//TRIM(ADJUSTL(self%switch))// &
                           '" is an envvar with action different from "'//action_store//'" that is not allowed!'
    CASE (ERROR_ENVVAR_NARGS)
self%error_message = prefd//': named option "'//TRIM(ADJUSTL(self%switch))// &
                  '" is an envvar that is not allowed for list valued option!'
    CASE (ERROR_STORE_STAR_POSITIONAL)
      self%error_message = prefd//': "'//TRIM(str(self%position, .TRUE.))//'-th" positional option '//&
                           'has "'//action_store_star//'" action that is not allowed for positional option!'
    CASE (ERROR_STORE_STAR_NARGS)
self%error_message = prefd//': named option "'//TRIM(ADJUSTL(self%switch))// &
                           '" has "'//action_store_star//'" action that is not allowed for list valued option!'
    CASE (ERROR_STORE_STAR_ENVVAR)
self%error_message = prefd//': named option "'//TRIM(ADJUSTL(self%switch))// &
                           '" has "'//action_store_star//'" action that is not allowed for environment variable option!'
    CASE (ERROR_ACTION_UNKNOWN)
      self%error_message = prefd//': named option "'//TRIM(ADJUSTL(self%switch))//'" has unknown "'//self%act//'" action!'
    CASE (ERROR_DUPLICATED_CLAS)
      self%error_message = prefd//': switch "'//TRIM(ADJUSTL(switch))//'" has been passed more than once!'
    END SELECT
    CALL self%print_error_message
  END IF
END SUBROUTINE errored

SUBROUTINE check_envvar_consistency(self, pref)
  !< Check data consistency for envvar CLA.
  CLASS(CommandLineArg_), INTENT(inout) :: self  !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref  !< Prefixing string.

  IF (ALLOCATED(self%envvar)) THEN
    IF (self%is_positional) THEN
      CALL self%errored(pref=pref, error=ERROR_ENVVAR_POSITIONAL)
      RETURN
    END IF
    IF (.NOT. ALLOCATED(self%act)) THEN
      CALL self%errored(pref=pref, error=ERROR_ENVVAR_NOT_STORE)
      RETURN
    ELSE
      IF (self%act /= action_store) THEN
        CALL self%errored(pref=pref, error=ERROR_ENVVAR_NOT_STORE)
        RETURN
      END IF
    END IF
    IF (ALLOCATED(self%nargs)) THEN
      CALL self%errored(pref=pref, error=ERROR_ENVVAR_NARGS)
      RETURN
    END IF
  END IF
END SUBROUTINE check_envvar_consistency

SUBROUTINE check_action_consistency(self, pref)
  !< Check CLA action consistency.
  CLASS(CommandLineArg_), INTENT(inout) :: self  !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref  !< Prefixing string.

  IF (ALLOCATED(self%act)) THEN
    IF (self%act == ACTION_STORE_STAR .AND. self%is_positional) THEN
      CALL self%errored(pref=pref, error=ERROR_STORE_STAR_POSITIONAL)
      RETURN
    END IF
    IF (self%act == ACTION_STORE_STAR .AND. ALLOCATED(self%nargs)) THEN
      CALL self%errored(pref=pref, error=ERROR_STORE_STAR_NARGS)
      RETURN
    END IF
    IF (self%act == ACTION_STORE_STAR .AND. ALLOCATED(self%envvar)) THEN
      CALL self%errored(pref=pref, error=ERROR_STORE_STAR_ENVVAR)
      RETURN
    END IF
    IF (self%act /= ACTION_STORE .AND. &
        self%act /= ACTION_STORE_STAR .AND. &
        self%act /= ACTION_STORE_TRUE .AND. &
        self%act /= ACTION_STORE_FALSE .AND. &
        self%act /= ACTION_PRINT_HELP .AND. &
        self%act /= ACTION_PRINT_VERS) THEN
      CALL self%errored(pref=pref, error=ERROR_ACTION_UNKNOWN)
      RETURN
    END IF
  END IF
END SUBROUTINE check_action_consistency

SUBROUTINE check_optional_consistency(self, pref)
  !< Check optional CLA consistency.
  CLASS(CommandLineArg_), INTENT(inout) :: self  !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref  !< Prefixing string.

  IF ((.NOT.self%is_required).AND.(.NOT.ALLOCATED(self%def))) CALL self%errored(pref=pref, error=ERROR_OPTIONAL_NO_DEF)
END SUBROUTINE check_optional_consistency

SUBROUTINE check_m_exclude_consistency(self, pref)
  !< Check mutually exclusion consistency.
  CLASS(CommandLineArg_), INTENT(inout) :: self  !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref  !< Prefixing string.

  IF ((self%is_required) .AND. (self%m_exclude /= '')) THEN
    CALL self%errored(pref=pref, error=ERROR_REQUIRED_M_EXCLUDE)
    RETURN
  END IF
  IF ((self%is_positional) .AND. (self%m_exclude /= '')) THEN
    CALL self%errored(pref=pref, error=ERROR_POSITIONAL_M_EXCLUDE)
    RETURN
  END IF
END SUBROUTINE check_m_exclude_consistency

SUBROUTINE check_named_consistency(self, pref)
  !< Check named CLA consistency.
  CLASS(CommandLineArg_), INTENT(inout) :: self  !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref  !< Prefixing string.

  IF ((.NOT.self%is_positional).AND.(.NOT.ALLOCATED(self%switch))) CALL self%errored(pref=pref, error=ERROR_NAMED_NO_NAME)
END SUBROUTINE check_named_consistency

SUBROUTINE check_positional_consistency(self, pref)
  !< Check positional CLA consistency.
  CLASS(CommandLineArg_), INTENT(inout) :: self  !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref  !< Prefixing string.

  IF ((self%is_positional) .AND. (self%position == 0_I4P)) THEN
    CALL self%errored(pref=pref, error=ERROR_POSITIONAL_NO_POSITION)
    RETURN
  ELSEIF ((self%is_positional) .AND. (self%act /= action_store)) THEN
    CALL self%errored(pref=pref, error=ERROR_POSITIONAL_NO_STORE)
  END IF
END SUBROUTINE check_positional_consistency

SUBROUTINE check_choices(self, val, pref)
  !< Check if CLA value is in allowed choices.
  !<
  !< @note This procedure can be called if and only if cla%choices has been allocated.
  CLASS(CommandLineArg_), INTENT(inout) :: self    !< CLA data.
  CLASS(*), INTENT(in) :: val     !< CLA value.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref    !< Prefixing string.
  CHARACTER(LEN(self%choices)), ALLOCATABLE :: toks(:) !< Tokens for parsing choices list.
  INTEGER(I4P) :: Nc      !< Number of choices.
  LOGICAL :: val_in  !< Flag for checking if val is in the choosen range.
  CHARACTER(len=:), ALLOCATABLE :: val_str !< Value in string form.
  CHARACTER(len=:), ALLOCATABLE :: tmp     !< Temporary string for avoiding GNU gfrotran bug.
  INTEGER(I4P) :: c       !< Counter.

  val_in = .FALSE.
  val_str = ''
  tmp = self%choices
  CALL tokenize(strin=tmp, delimiter=',', toks=toks, Nt=Nc)
  SELECT TYPE (val)
#if defined USE_Real128
  TYPE is (REAL(R16P))
    val_str = str(n=val)
    DO c = 1, Nc
     IF (val == cton(str=TRIM(ADJUSTL(toks(c))), knd=1._R16P)) val_in = .TRUE.
    END DO
#endif
  TYPE is (REAL(R8P))
    val_str = str(n=val)
    DO c = 1, Nc
      IF (val == cton(str=TRIM(ADJUSTL(toks(c))), knd=1._R8P)) val_in = .TRUE.
    END DO
  TYPE is (REAL(R4P))
    val_str = str(n=val)
    DO c = 1, Nc
      IF (val == cton(str=TRIM(ADJUSTL(toks(c))), knd=1._R4P)) val_in = .TRUE.
    END DO
  TYPE is (INTEGER(I8P))
    val_str = str(n=val)
    DO c = 1, Nc
      IF (val == cton(str=TRIM(ADJUSTL(toks(c))), knd=1_I8P)) val_in = .TRUE.
    END DO
  TYPE is (INTEGER(I4P))
    val_str = str(n=val)
    DO c = 1, Nc
      IF (val == cton(str=TRIM(ADJUSTL(toks(c))), knd=1_I4P)) val_in = .TRUE.
    END DO
  TYPE is (INTEGER(I2P))
    val_str = str(n=val)
    DO c = 1, Nc
      IF (val == cton(str=TRIM(ADJUSTL(toks(c))), knd=1_I2P)) val_in = .TRUE.
    END DO
  TYPE is (INTEGER(I1P))
    val_str = str(n=val)
    DO c = 1, Nc
      IF (val == cton(str=TRIM(ADJUSTL(toks(c))), knd=1_I1P)) val_in = .TRUE.
    END DO
  TYPE is (CHARACTER(*))
    val_str = val
    DO c = 1, Nc
      IF (val == toks(c)) val_in = .TRUE.
    END DO
  TYPE is (LOGICAL)
    CALL self%errored(pref=pref, error=ERROR_CHOICES_LOGICAL)
  END SELECT
  IF (.NOT. val_in .AND. (self%error == 0)) THEN
    CALL self%errored(pref=pref, error=ERROR_NOT_IN_CHOICES, val_str=val_str)
  END IF
END SUBROUTINE check_choices

FUNCTION check_list_size(self, Nv, val, pref) RESULT(is_ok)
  !< Check CLA multiple values list size consistency.
  CLASS(CommandLineArg_), INTENT(inout) :: self  !< CLA data.
  INTEGER(I4P), INTENT(in) :: Nv    !< Number of values.
  CHARACTER(*), INTENT(in) :: val   !< First value.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref  !< Prefixing string.
  LOGICAL :: is_ok !< Check result.

  is_ok = .TRUE.
  IF (Nv == 1) THEN
    IF (TRIM(ADJUSTL(val)) == '') THEN
      ! there is no real value, but only for nargs=+ this is a real error
      is_ok = .FALSE.
      IF (self%nargs == '+') THEN
        CALL self%errored(pref=pref, error=ERROR_NARGS_INSUFFICIENT)
      END IF
    END IF
  END IF
END FUNCTION check_list_size

SUBROUTINE get_cla(self, val, pref)
  !< Get CLA (single) value.
  IMPLICIT NONE
  CLASS(CommandLineArg_), INTENT(inout) :: self  !< CLA data.
  CLASS(*), INTENT(inout) :: val   !< CLA value.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref  !< Prefixing string.

  IF (.NOT. self%is_required_passed(pref=pref)) RETURN
  IF (self%act == action_store .OR. self%act == action_store_star) THEN
    IF (self%is_passed .AND. ALLOCATED(self%val)) THEN
      CALL self%get_cla_from_buffer(buffer=self%val, val=val, pref=pref)
    ELSEIF (ALLOCATED(self%def)) THEN ! using default value
      CALL self%get_cla_from_buffer(buffer=self%def, val=val, pref=pref)
    END IF
    IF (ALLOCATED(self%choices).AND.self%error==0) CALL self%check_choices(val=val, pref=pref)
  ELSEIF (self%act == action_store_true) THEN
    IF (self%is_passed) THEN
      SELECT TYPE (val)
      TYPE is (LOGICAL)
        val = .TRUE.
      END SELECT
    ELSEIF (ALLOCATED(self%def)) THEN
      SELECT TYPE (val)
      TYPE is (LOGICAL)
        READ (self%def, *, iostat=self%error) val
        IF (self%error/=0) CALL self%errored(pref=pref, error=ERROR_CASTING_LOGICAL, log_value=self%def)
      END SELECT
    END IF
  ELSEIF (self%act == action_store_false) THEN
    IF (self%is_passed) THEN
      SELECT TYPE (val)
      TYPE is (LOGICAL)
        val = .FALSE.
      END SELECT
    ELSEIF (ALLOCATED(self%def)) THEN
      SELECT TYPE (val)
      TYPE is (LOGICAL)
        READ (self%def, *, iostat=self%error) val
        IF (self%error/=0) CALL self%errored(pref=pref, error=ERROR_CASTING_LOGICAL, log_value=self%def)
      END SELECT
    END IF
  END IF
END SUBROUTINE get_cla

SUBROUTINE get_cla_from_buffer(self, buffer, val, pref)
  !< Get CLA (single) value from parsed value.
  IMPLICIT NONE
  CLASS(CommandLineArg_), INTENT(inout) :: self   !< CLA data.
  CHARACTER(*), INTENT(in) :: buffer !< Buffer containing values (parsed or default CLA value).
  CLASS(*), INTENT(inout) :: val    !< CLA value.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref   !< Prefixing string.

  SELECT TYPE (val)
#if defined _R16P
  TYPE is (REAL(R16P))
    val = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(buffer)), knd=1._R16P)
#endif
  TYPE is (REAL(R8P))
val = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(buffer)), knd=1._R8P)
  TYPE is (REAL(R4P))
val = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(buffer)), knd=1._R4P)
  TYPE is (INTEGER(I8P))
 val = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(buffer)), knd=1_I8P)
  TYPE is (INTEGER(I4P))
 val = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(buffer)), knd=1_I4P)
  TYPE is (INTEGER(I2P))
 val = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(buffer)), knd=1_I2P)
  TYPE is (INTEGER(I1P))
 val = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(buffer)), knd=1_I1P)
  TYPE is (LOGICAL)
    READ (buffer, *, iostat=self%error) val
    IF (self%error/=0) CALL self%errored(pref=pref, error=ERROR_CASTING_LOGICAL, log_value=buffer)
  TYPE is (CHARACTER(*))
    val = buffer
  END SELECT
END SUBROUTINE get_cla_from_buffer

SUBROUTINE get_cla_list(self, pref, val)
  !< Get CLA multiple values.
  CLASS(CommandLineArg_), INTENT(inout) :: self     !< CLA data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !< Prefixing string.
  CLASS(*), INTENT(inout) :: val(1:)  !< CLA values.
  INTEGER(I4P) :: Nv       !< Number of values.
  CHARACTER(len=LEN(self%def)), ALLOCATABLE :: valsD(:) !< String array of values based on self%def.
  INTEGER(I4P) :: v        !< Values counter.

  IF (.NOT. self%is_required_passed(pref=pref)) RETURN
  IF (.NOT. ALLOCATED(self%nargs)) THEN
    CALL self%errored(pref=pref, error=ERROR_NO_LIST)
    RETURN
  END IF
  IF (self%act == action_store) THEN
    IF (self%is_passed) THEN
      CALL self%get_cla_list_from_buffer(buffer=self%val, val=val, pref=pref)
    ELSE ! using default value
      CALL self%get_cla_list_from_buffer(buffer=self%def, val=val, pref=pref)
    END IF
  ELSEIF (self%act == action_store_true) THEN
    IF (self%is_passed) THEN
      SELECT TYPE (val)
      TYPE is (LOGICAL)
        val = .TRUE.
      END SELECT
    ELSE
      CALL tokenize(strin=self%def, delimiter=' ', toks=valsD, Nt=Nv)
      SELECT TYPE (val)
      TYPE is (LOGICAL)
        DO v = 1, Nv
          READ (valsD(v), *) val(v)
        END DO
      END SELECT
    END IF
  ELSEIF (self%act == action_store_false) THEN
    IF (self%is_passed) THEN
      SELECT TYPE (val)
      TYPE is (LOGICAL)
        val = .FALSE.
      END SELECT
    ELSE
      CALL tokenize(strin=self%def, delimiter=' ', toks=valsD, Nt=Nv)
      SELECT TYPE (val)
      TYPE is (LOGICAL)
        DO v = 1, Nv
          READ (valsD(v), *) val(v)
        END DO
      END SELECT
    END IF
  END IF
END SUBROUTINE get_cla_list

SUBROUTINE get_cla_list_from_buffer(self, buffer, val, pref)
  !< Get CLA multiple values from a buffer.
  IMPLICIT NONE
  CLASS(CommandLineArg_), INTENT(inout) :: self    !< CLA data.
  CHARACTER(*), INTENT(in) :: buffer  !< Buffer containing values (parsed or default CLA value).
  CLASS(*), INTENT(inout) :: val(1:) !< CLA value.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref    !< Prefixing string.
  INTEGER(I4P) :: Nv      !< Number of values.
  CHARACTER(len=LEN(buffer)), ALLOCATABLE :: vals(:) !< String array of values based on buffer value.
  INTEGER(I4P) :: v       !< Values counter.

  CALL tokenize(strin=buffer, delimiter=args_sep, toks=vals, Nt=Nv)
  SELECT TYPE (val)
#if defined _R16P
  TYPE is (REAL(R16P))
    DO v = 1, Nv
      val(v) = cton(pref=pref,error=self%error,str=TRIM(ADJUSTL(vals(v))),knd=1._R16P)
      IF (ALLOCATED(self%choices).AND.self%error==0) CALL self%check_choices(val=val(v),pref=pref)
      IF (self%error /= 0) EXIT
    END DO
#endif
  TYPE is (REAL(R8P))
    DO v = 1, Nv
      val(v) = cton(pref=pref,error=self%error,str=TRIM(ADJUSTL(vals(v))),knd=1._R8P)
      IF (ALLOCATED(self%choices).AND.self%error==0) CALL self%check_choices(val=val(v),pref=pref)
      IF (self%error /= 0) EXIT
    END DO
  TYPE is (REAL(R4P))
    DO v = 1, Nv
      val(v) = cton(pref=pref,error=self%error,str=TRIM(ADJUSTL(vals(v))),knd=1._R4P)
      IF (ALLOCATED(self%choices).AND.self%error==0) CALL self%check_choices(val=val(v),pref=pref)
      IF (self%error /= 0) EXIT
    END DO
  TYPE is (INTEGER(I8P))
    DO v = 1, Nv
val(v) = cton(pref=pref,error=self%error,str=TRIM(ADJUSTL(vals(v))),knd=1_I8P)
      IF (ALLOCATED(self%choices).AND.self%error==0) CALL self%check_choices(val=val(v),pref=pref)
      IF (self%error /= 0) EXIT
    END DO
  TYPE is (INTEGER(I4P))
    DO v = 1, Nv
val(v) = cton(pref=pref,error=self%error,str=TRIM(ADJUSTL(vals(v))),knd=1_I4P)
      IF (ALLOCATED(self%choices).AND.self%error==0) CALL self%check_choices(val=val(v),pref=pref)
      IF (self%error /= 0) EXIT
    END DO
  TYPE is (INTEGER(I2P))
    DO v = 1, Nv
val(v) = cton(pref=pref,error=self%error,str=TRIM(ADJUSTL(vals(v))),knd=1_I2P)
      IF (ALLOCATED(self%choices).AND.self%error==0) CALL self%check_choices(val=val(v),pref=pref)
      IF (self%error /= 0) EXIT
    END DO
  TYPE is (INTEGER(I1P))
    DO v = 1, Nv
val(v) = cton(pref=pref,error=self%error,str=TRIM(ADJUSTL(vals(v))),knd=1_I1P)
      IF (ALLOCATED(self%choices).AND.self%error==0) CALL self%check_choices(val=val(v),pref=pref)
      IF (self%error /= 0) EXIT
    END DO
  TYPE is (LOGICAL)
    DO v = 1, Nv
      READ (vals(v), *, iostat=self%error) val(v)
      IF (self%error /= 0) THEN
  CALL self%errored(pref=pref, error=ERROR_CASTING_LOGICAL, log_value=vals(v))
        EXIT
      END IF
    END DO
  TYPE is (CHARACTER(*))
    DO v = 1, Nv
      val(v) = vals(v)
      IF (ALLOCATED(self%choices).AND.self%error==0) CALL self%check_choices(val=val(v),pref=pref)
      IF (self%error /= 0) EXIT
    END DO
  END SELECT
END SUBROUTINE get_cla_list_from_buffer

SUBROUTINE get_cla_list_varying_R16P(self, val, pref)
  !< Get CLA (multiple) value with varying size, real(R16P).
  CLASS(CommandLineArg_), INTENT(inout) :: self     !< CLA data.
  REAL(R16P), ALLOCATABLE, INTENT(out) :: val(:)   !< CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !< Prefixing string.
  INTEGER(I4P) :: Nv       !< Number of values.
  CHARACTER(len=LEN(self%val)), ALLOCATABLE :: valsV(:) !< String array of values based on self%val.
  CHARACTER(len=LEN(self%def)), ALLOCATABLE :: valsD(:) !< String array of values based on self%def.
  INTEGER(I4P) :: v        !< Values counter.

  IF (.NOT. self%is_required_passed(pref=pref)) RETURN
  IF (.NOT. ALLOCATED(self%nargs)) THEN
    CALL self%errored(pref=pref, error=ERROR_NO_LIST)
    RETURN
  END IF
  IF (self%act == action_store) THEN
    IF (self%is_passed) THEN
      CALL tokenize(strin=self%val, delimiter=ARGS_SEP, toks=valsV, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsV(1), pref=pref)) RETURN
      ALLOCATE (REAL(R16P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsV(v))), knd=1._R16P)
        IF (self%error /= 0) EXIT
      END DO
    ELSE ! using default value
      CALL tokenize(strin=self%def, delimiter=ARGS_SEP, toks=valsD, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsD(1), pref=pref)) RETURN
      IF (Nv == 1) THEN
        IF (TRIM(ADJUSTL(valsD(1))) == '') THEN
          IF (self%nargs == '+') THEN
            CALL self%errored(pref=pref, error=ERROR_NARGS_INSUFFICIENT)
          END IF
          RETURN
        END IF
      END IF
      ALLOCATE (REAL(R16P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsD(v))), knd=1._R16P)
        IF (self%error /= 0) EXIT
      END DO
    END IF
  END IF
END SUBROUTINE get_cla_list_varying_R16P

SUBROUTINE get_cla_list_varying_R8P(self, val, pref)
  !< Get CLA (multiple) value with varying size, real(R8P).
  CLASS(CommandLineArg_), INTENT(inout) :: self     !< CLA data.
  REAL(R8P), ALLOCATABLE, INTENT(out) :: val(:)   !< CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !< Prefixing string.
  INTEGER(I4P) :: Nv       !< Number of values.
  CHARACTER(len=LEN(self%val)), ALLOCATABLE :: valsV(:) !< String array of values based on self%val.
  CHARACTER(len=LEN(self%def)), ALLOCATABLE :: valsD(:) !< String array of values based on self%def.
  INTEGER(I4P) :: v        !< Values counter.

  IF (.NOT. self%is_required_passed(pref=pref)) RETURN
  IF (.NOT. ALLOCATED(self%nargs)) THEN
    CALL self%errored(pref=pref, error=ERROR_NO_LIST)
    RETURN
  END IF
  IF (self%act == action_store) THEN
    IF (self%is_passed) THEN
      CALL tokenize(strin=self%val, delimiter=ARGS_SEP, toks=valsV, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsV(1), pref=pref)) RETURN
      ALLOCATE (REAL(R8P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsV(v))), knd=1._R8P)
        IF (self%error /= 0) EXIT
      END DO
    ELSE ! using default value
      CALL tokenize(strin=self%def, delimiter=ARGS_SEP, toks=valsD, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsD(1), pref=pref)) RETURN
      ALLOCATE (REAL(R8P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsD(v))), knd=1._R8P)
        IF (self%error /= 0) EXIT
      END DO
    END IF
  END IF
END SUBROUTINE get_cla_list_varying_R8P

SUBROUTINE get_cla_list_varying_R4P(self, val, pref)
  !< Get CLA (multiple) value with varying size, real(R4P).
  CLASS(CommandLineArg_), INTENT(inout) :: self     !< CLA data.
  REAL(R4P), ALLOCATABLE, INTENT(out) :: val(:)   !< CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !< Prefixing string.
  INTEGER(I4P) :: Nv       !< Number of values.
  CHARACTER(len=LEN(self%val)), ALLOCATABLE :: valsV(:) !< String array of values based on self%val.
  CHARACTER(len=LEN(self%def)), ALLOCATABLE :: valsD(:) !< String array of values based on self%def.
  INTEGER(I4P) :: v        !< Values counter.

  IF (.NOT. self%is_required_passed(pref=pref)) RETURN
  IF (.NOT. ALLOCATED(self%nargs)) THEN
    CALL self%errored(pref=pref, error=ERROR_NO_LIST)
    RETURN
  END IF
  IF (self%act == action_store) THEN
    IF (self%is_passed) THEN
      CALL tokenize(strin=self%val, delimiter=ARGS_SEP, toks=valsV, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsV(1), pref=pref)) RETURN
      ALLOCATE (REAL(R4P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsV(v))), knd=1._R4P)
        IF (self%error /= 0) EXIT
      END DO
    ELSE ! using default value
      CALL tokenize(strin=self%def, delimiter=ARGS_SEP, toks=valsD, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsD(1), pref=pref)) RETURN
      ALLOCATE (REAL(R4P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsD(v))), knd=1._R4P)
        IF (self%error /= 0) EXIT
      END DO
    END IF
  END IF
END SUBROUTINE get_cla_list_varying_R4P

SUBROUTINE get_cla_list_varying_I8P(self, val, pref)
  !< Get CLA (multiple) value with varying size, integer(I8P).
  CLASS(CommandLineArg_), INTENT(inout) :: self     !< CLA data.
  INTEGER(I8P), ALLOCATABLE, INTENT(out) :: val(:)   !< CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !< Prefixing string.
  INTEGER(I4P) :: Nv       !< Number of values.
  CHARACTER(len=LEN(self%val)), ALLOCATABLE :: valsV(:) !< String array of values based on self%val.
  CHARACTER(len=LEN(self%def)), ALLOCATABLE :: valsD(:) !< String array of values based on self%def.
  INTEGER(I4P) :: v        !< Values counter.

  IF (.NOT. self%is_required_passed(pref=pref)) RETURN
  IF (.NOT. ALLOCATED(self%nargs)) THEN
    CALL self%errored(pref=pref, error=ERROR_NO_LIST)
    RETURN
  END IF
  IF (self%act == action_store) THEN
    IF (self%is_passed) THEN
      CALL tokenize(strin=self%val, delimiter=ARGS_SEP, toks=valsV, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsV(1), pref=pref)) RETURN
      ALLOCATE (INTEGER(I8P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsV(v))), knd=1_I8P)
        IF (self%error /= 0) EXIT
      END DO
    ELSE ! using default value
      CALL tokenize(strin=self%def, delimiter=ARGS_SEP, toks=valsD, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsD(1), pref=pref)) RETURN
      ALLOCATE (INTEGER(I8P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsD(v))), knd=1_I8P)
        IF (self%error /= 0) EXIT
      END DO
    END IF
  END IF
END SUBROUTINE get_cla_list_varying_I8P

SUBROUTINE get_cla_list_varying_I4P(self, val, pref)
  !< Get CLA (multiple) value with varying size, integer(I4P).
  CLASS(CommandLineArg_), INTENT(INOUT) :: self     !< CLA data.
  INTEGER(I4P), ALLOCATABLE, INTENT(OUT) :: val(:)   !< CLA values.
  CHARACTER(*), OPTIONAL, INTENT(IN) :: pref     !< Prefixing string.
  INTEGER(I4P) :: Nv       !< Number of values.
  CHARACTER(len=LEN(self%val)), ALLOCATABLE :: valsV(:) !< String array of values based on self%val.
  CHARACTER(len=LEN(self%def)), ALLOCATABLE :: valsD(:) !< String array of values based on self%def.
  INTEGER(I4P) :: v        !< Values counter.

  IF (.NOT. self%is_required_passed(pref=pref)) RETURN
  IF (.NOT. ALLOCATED(self%nargs)) THEN
    CALL self%errored(pref=pref, error=ERROR_NO_LIST)
    RETURN
  END IF
  IF (self%act == action_store) THEN
    IF (self%is_passed) THEN
      CALL tokenize(strin=self%val, delimiter=ARGS_SEP, toks=valsV, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsV(1), pref=pref)) RETURN
      ALLOCATE (INTEGER(I4P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsV(v))), knd=1_I4P)
        IF (self%error /= 0) EXIT
      END DO
    ELSE ! using default value
      CALL tokenize(strin=self%def, delimiter=ARGS_SEP, toks=valsD, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsD(1), pref=pref)) RETURN
      ALLOCATE (INTEGER(I4P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsD(v))), knd=1_I4P)
        IF (self%error /= 0) EXIT
      END DO
    END IF
  END IF
END SUBROUTINE get_cla_list_varying_I4P

SUBROUTINE get_cla_list_varying_I2P(self, val, pref)
  !< Get CLA (multiple) value with varying size, integer(I2P).
  CLASS(CommandLineArg_), INTENT(inout) :: self     !< CLA data.
  INTEGER(I2P), ALLOCATABLE, INTENT(out) :: val(:)   !< CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !< Prefixing string.
  INTEGER(I4P) :: Nv       !< Number of values.
  CHARACTER(len=LEN(self%val)), ALLOCATABLE :: valsV(:) !< String array of values based on self%val.
  CHARACTER(len=LEN(self%def)), ALLOCATABLE :: valsD(:) !< String array of values based on self%def.
  INTEGER(I4P) :: v        !< Values counter.

  IF (.NOT. self%is_required_passed(pref=pref)) RETURN
  IF (.NOT. ALLOCATED(self%nargs)) THEN
    CALL self%errored(pref=pref, error=ERROR_NO_LIST)
    RETURN
  END IF
  IF (self%act == action_store) THEN
    IF (self%is_passed) THEN
      CALL tokenize(strin=self%val, delimiter=ARGS_SEP, toks=valsV, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsV(1), pref=pref)) RETURN
      ALLOCATE (INTEGER(I2P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsV(v))), knd=1_I2P)
        IF (self%error /= 0) EXIT
      END DO
    ELSE ! using default value
      CALL tokenize(strin=self%def, delimiter=ARGS_SEP, toks=valsD, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsD(1), pref=pref)) RETURN
      ALLOCATE (INTEGER(I2P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsD(v))), knd=1_I2P)
        IF (self%error /= 0) EXIT
      END DO
    END IF
  END IF
END SUBROUTINE get_cla_list_varying_I2P

SUBROUTINE get_cla_list_varying_I1P(self, val, pref)
  !< Get CLA (multiple) value with varying size, integer(I1P).
  CLASS(CommandLineArg_), INTENT(inout) :: self     !< CLA data.
  INTEGER(I1P), ALLOCATABLE, INTENT(out) :: val(:)   !< CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !< Prefixing string.
  INTEGER(I4P) :: Nv       !< Number of values.
  CHARACTER(len=LEN(self%val)), ALLOCATABLE :: valsV(:) !< String array of values based on self%val.
  CHARACTER(len=LEN(self%def)), ALLOCATABLE :: valsD(:) !< String array of values based on self%def.
  INTEGER(I4P) :: v        !< Values counter.

  IF (.NOT. self%is_required_passed(pref=pref)) RETURN
  IF (.NOT. ALLOCATED(self%nargs)) THEN
    CALL self%errored(pref=pref, error=ERROR_NO_LIST)
    RETURN
  END IF
  IF (self%act == action_store) THEN
    IF (self%is_passed) THEN
      CALL tokenize(strin=self%val, delimiter=ARGS_SEP, toks=valsV, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsV(1), pref=pref)) RETURN
      ALLOCATE (INTEGER(I1P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsV(v))), knd=1_I1P)
        IF (self%error /= 0) EXIT
      END DO
    ELSE ! using default value
      CALL tokenize(strin=self%def, delimiter=ARGS_SEP, toks=valsD, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsD(1), pref=pref)) RETURN
      ALLOCATE (INTEGER(I1P) :: val(1:Nv))
      DO v = 1, Nv
        val(v) = cton(pref=pref, error=self%error, str=TRIM(ADJUSTL(valsD(v))), knd=1_I1P)
        IF (self%error /= 0) EXIT
      END DO
    END IF
  END IF
END SUBROUTINE get_cla_list_varying_I1P

SUBROUTINE get_cla_list_varying_logical(self, val, pref)
  !< Get CLA (multiple) value with varying size, logical.
  CLASS(CommandLineArg_), INTENT(inout) :: self     !< CLA data.
  LOGICAL, ALLOCATABLE, INTENT(out) :: val(:)   !< CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !< Prefixing string.
  INTEGER(I4P) :: Nv       !< Number of values.
  CHARACTER(len=LEN(self%val)), ALLOCATABLE :: valsV(:) !< String array of values based on self%val.
  CHARACTER(len=LEN(self%def)), ALLOCATABLE :: valsD(:) !< String array of values based on self%def.
  INTEGER(I4P) :: v        !< Values counter.

  IF (.NOT. self%is_required_passed(pref=pref)) RETURN
  IF (.NOT. ALLOCATED(self%nargs)) THEN
    CALL self%errored(pref=pref, error=ERROR_NO_LIST)
    RETURN
  END IF
  IF (self%act == action_store) THEN
    IF (self%is_passed) THEN
      CALL tokenize(strin=self%val, delimiter=ARGS_SEP, toks=valsV, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsV(1), pref=pref)) RETURN
      ALLOCATE (LOGICAL :: val(1:Nv))
      DO v = 1, Nv
        READ (valsV(v), *, iostat=self%error) val(v)
        IF (self%error /= 0) THEN
 CALL self%errored(pref=pref, error=ERROR_CASTING_LOGICAL, log_value=valsD(v))
          EXIT
        END IF
      END DO
    ELSE ! using default value
      CALL tokenize(strin=self%def, delimiter=ARGS_SEP, toks=valsD, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsD(1), pref=pref)) RETURN
      ALLOCATE (LOGICAL :: val(1:Nv))
      DO v = 1, Nv
        READ (valsD(v), *, iostat=self%error) val(v)
        IF (self%error /= 0) THEN
 CALL self%errored(pref=pref, error=ERROR_CASTING_LOGICAL, log_value=valsD(v))
          EXIT
        END IF
      END DO
    END IF
  END IF
END SUBROUTINE get_cla_list_varying_logical

SUBROUTINE get_cla_list_varying_char(self, val, pref)
  !< Get CLA (multiple) value with varying size, character.
  CLASS(CommandLineArg_), INTENT(inout) :: self     !< CLA data.
  CHARACTER(*), ALLOCATABLE, INTENT(out) :: val(:)   !< CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !< Prefixing string.
  INTEGER(I4P) :: Nv       !< Number of values.
  CHARACTER(len=LEN(self%val)), ALLOCATABLE :: valsV(:) !< String array of values based on self%val.
  CHARACTER(len=LEN(self%def)), ALLOCATABLE :: valsD(:) !< String array of values based on self%def.
  INTEGER(I4P) :: v        !< Values counter.

  IF (.NOT. self%is_required_passed(pref=pref)) RETURN
  IF (.NOT. ALLOCATED(self%nargs)) THEN
    CALL self%errored(pref=pref, error=ERROR_NO_LIST)
    RETURN
  END IF
  IF (self%act == action_store) THEN
    IF (self%is_passed) THEN
      CALL tokenize(strin=self%val, delimiter=ARGS_SEP, toks=valsV, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsV(1), pref=pref)) RETURN
      ALLOCATE (val(1:Nv))
      DO v = 1, Nv
        val(v) = TRIM(ADJUSTL(valsV(v)))
      END DO
    ELSE ! using default value
      CALL tokenize(strin=self%def, delimiter=ARGS_SEP, toks=valsD, Nt=Nv)
      IF (.NOT. self%check_list_size(Nv=Nv, val=valsD(1), pref=pref)) RETURN
      ALLOCATE (val(1:Nv))
      DO v = 1, Nv
        val(v) = TRIM(ADJUSTL(valsD(v)))
      END DO
    END IF
  END IF
END SUBROUTINE get_cla_list_varying_char

ELEMENTAL SUBROUTINE cla_assign_cla(lhs, rhs)
  !< Assignment operator.
  CLASS(CommandLineArg_), INTENT(inout) :: lhs !< Left hand side.
  TYPE(CommandLineArg_), INTENT(in) :: rhs !< Rigth hand side.

  ! object members
  CALL lhs%assign_object(rhs)
  ! CommandLineArg_ members
  IF (ALLOCATED(rhs%switch)) lhs%switch = rhs%switch
  IF (ALLOCATED(rhs%switch_ab)) lhs%switch_ab = rhs%switch_ab
  IF (ALLOCATED(rhs%act)) lhs%act = rhs%act
  IF (ALLOCATED(rhs%def)) lhs%def = rhs%def
  IF (ALLOCATED(rhs%nargs)) lhs%nargs = rhs%nargs
  IF (ALLOCATED(rhs%choices)) lhs%choices = rhs%choices
  IF (ALLOCATED(rhs%val)) lhs%val = rhs%val
  IF (ALLOCATED(rhs%envvar)) lhs%envvar = rhs%envvar
  lhs%is_required = rhs%is_required
  lhs%is_positional = rhs%is_positional
  lhs%position = rhs%position
  lhs%is_passed = rhs%is_passed
  lhs%is_hidden = rhs%is_hidden
END SUBROUTINE cla_assign_cla

ELEMENTAL SUBROUTINE finalize(self)
  !< Free dynamic memory when finalizing.
  TYPE(CommandLineArg_), INTENT(inout) :: self !< CLA data.

  CALL self%free
END SUBROUTINE finalize
endmodule CommandLineArg_Class
