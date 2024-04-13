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
! summary: CommandLineInterface class

MODULE CommandLineInterface_Class
USE FACE, ONLY: colorize
USE CommandLineArg_Class, ONLY: CommandLineArg_, ACTION_STORE, ERROR_UNKNOWN
USE CommandLineGroupArg_Class, ONLY: CommandLineGroupArg_, &
  & STATUS_PRINT_H, STATUS_PRINT_V
USE AbstractCommandLine_Class, ONLY: AbstractCommandLineArg_
USE CommandLineArg_Utils
USE PENF
IMPLICIT NONE
PRIVATE
SAVE

INTEGER(I4P), PARAMETER, PUBLIC :: MAX_VAL_LEN = 1000
!! Maximum number of characters of CLA value.
! errors codes
INTEGER(I4P), PARAMETER, PUBLIC :: ERROR_MISSING_CLA = 1000
!! CLA not found in CLI.
INTEGER(I4P), PARAMETER, PUBLIC :: ERROR_MISSING_GROUP = 1001
!! Group not found in CLI.
INTEGER(I4P), PARAMETER, PUBLIC :: ERROR_MISSING_SELECTION_CLA = 1002
!! CLA selection in CLI failing.
INTEGER(I4P), PARAMETER, PUBLIC :: ERROR_TOO_FEW_CLAS = 1003
!! Insufficient arguments for CLI.
INTEGER(I4P), PARAMETER, PUBLIC :: ERROR_UNKNOWN_CLAS_IGNORED = 1004
!! Unknown CLAs passed, but ignored.

!----------------------------------------------------------------------------
!                                                      CommandLineInterface_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-08
! update: 2021-11-08
! summary: Command line interface class

TYPE, EXTENDS(AbstractCommandLineArg_) :: CommandLineInterface_
  PRIVATE
  TYPE(CommandLineGroupArg_), ALLOCATABLE :: clasg(:)
!! CLA list [1:Na].
#ifdef __GFORTRAN__
  CHARACTER(512), ALLOCATABLE :: args(:)
  !! Actually passed command line arguments.
#else
  CHARACTER(len=:), ALLOCATABLE :: args(:)
  !! Actually passed command line arguments.
#endif
  LOGICAL :: disable_hv = .FALSE.
  !! Disable automatic 'help' and 'version' CLAs.
  LOGICAL :: is_parsed_ = .FALSE.
  !! Parse status.
  LOGICAL :: ignore_unknown_clas = .FALSE.
  !! Disable errors-raising for passed unknown CLAs.
  INTEGER(I4P) :: error_unknown_clas = 0_I4P
  !! Error trapping flag for unknown CLAs.
CONTAINS
  ! public methods
  PROCEDURE, PUBLIC :: Deallocate => free
  !! Free dynamic memory.
  PROCEDURE, PUBLIC :: Initiate => init
  !! Initialize CLI.
  PROCEDURE, PUBLIC :: add_group
  !! Add CLAs group CLI.
  PROCEDURE, PUBLIC :: add
  !! Add CLA to CLI.
  PROCEDURE, PUBLIC :: is_passed
  !! Check if a CLA has been passed.
  PROCEDURE, PUBLIC :: is_defined_group
  !! Check if a CLAs group has been defined.
  PROCEDURE, PUBLIC :: is_defined
  !! Check if a CLA has been defined.
  PROCEDURE, PUBLIC :: is_parsed
  !! Check if CLI has been parsed.
  PROCEDURE, PUBLIC :: set_mutually_exclusive_groups
  !! Set two CLAs group as mutually exclusive.
  PROCEDURE, PUBLIC :: run_command => is_called_group
  !! Check if a CLAs group has been run.
  PROCEDURE, PUBLIC :: parse
  !! Parse Command Line Interfaces.
  GENERIC, PUBLIC :: get => &
    get_cla, &
    get_cla_list
  !! Get CLA value(s) from CLAs list parsed.
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
  !! Get CLA value(s) from CLAs list parsed, varying size list.
  PROCEDURE, PUBLIC :: usage
  !! Get CLI usage.
  PROCEDURE, PUBLIC :: signature
  !! Get CLI signature.
  PROCEDURE, PUBLIC :: print_usage
  !! Print correct usage of CLI.
  PROCEDURE, PUBLIC :: save_bash_completion
  !! Save bash completion script (for named CLAs only).
  PROCEDURE, PUBLIC :: save_man_page
  !! Save CLI usage as man page.
  PROCEDURE, PUBLIC :: save_usage_to_markdown
  !! Save CLI usage as markdown.
  ! private methods
  PROCEDURE, PRIVATE :: errored
  !! Trig error occurence and print meaningful message.
  PROCEDURE, PRIVATE :: check
  !! Check data consistency.
  PROCEDURE, PRIVATE :: check_m_exclusive
  !! Check if two mutually exclusive CLAs group have been called.
  PROCEDURE, PRIVATE :: get_clasg_indexes
  !! Get CLAs groups indexes.
  GENERIC, PRIVATE :: get_args => &
    get_args_from_string, &
    get_args_from_invocation
  !! Get CLAs.
  PROCEDURE, PRIVATE :: get_args_from_string
  !! Get CLAs from string.
  PROCEDURE, PRIVATE :: get_args_from_invocation
  !! Get CLAs from CLI invocation.
  PROCEDURE, PRIVATE :: get_cla
  !! Get CLA (single) value from CLAs list parsed.
  PROCEDURE, PRIVATE :: get_cla_list
  !! Get CLA multiple values from CLAs list parsed.
#if defined USE_Real128
  PROCEDURE, PRIVATE :: get_cla_list_varying_R16P
#endif
  !! Get CLA multiple values from CLAs list parsed, varying size, R16P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_R8P
  !! Get CLA multiple values from CLAs list parsed, varying size, R8P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_R4P
  !! Get CLA multiple values from CLAs list parsed, varying size, R4P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_I8P
  !! Get CLA multiple values from CLAs list parsed, varying size, I8P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_I4P
  !! Get CLA multiple values from CLAs list parsed, varying size, I4P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_I2P
  !! Get CLA multiple values from CLAs list parsed, varying size, I2P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_I1P
  !! Get CLA multiple values from CLAs list parsed, varying size, I1P.
  PROCEDURE, PRIVATE :: get_cla_list_varying_logical
  !! Get CLA multiple values from CLAs list parsed, varying size, bool.
  PROCEDURE, PRIVATE :: get_cla_list_varying_char
  !! Get CLA multiple values from CLAs list parsed, varying size, char.
  PROCEDURE, PRIVATE :: cli_assign_cli
  !! CLI assignment overloading.
  GENERIC, PRIVATE :: ASSIGNMENT(=) => cli_assign_cli
  !! CLI assignment overloading.
  FINAL :: finalize
  !! Free dynamic memory when finalizing.
END TYPE CommandLineInterface_

PUBLIC :: CommandLineInterface_

!----------------------------------------------------------------------------
!                                                                  Contains
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Free
!----------------------------------------------------------------------------

! public methods
ELEMENTAL SUBROUTINE free(self)
  !! Free dynamic memory.
  CLASS(CommandLineInterface_), INTENT(inout) :: self !! CLI data.
  INTEGER(I4P) :: g    !! Counter.

  ! object members
  CALL self%free_object
  ! CommandLineInterface_ members
  IF (ALLOCATED(self%clasg)) THEN
    DO g = 0, SIZE(self%clasg, dim=1) - 1
      CALL self%clasg(g)%free
    END DO
    DEALLOCATE (self%clasg)
  END IF
  IF (ALLOCATED(self%args)) DEALLOCATE (self%args)
  IF (ALLOCATED(self%examples)) DEALLOCATE (self%examples)
  self%disable_hv = .FALSE.
  self%is_parsed_ = .FALSE.
  self%ignore_unknown_clas = .FALSE.
  self%error_unknown_clas = 0_I4P
END SUBROUTINE free

!----------------------------------------------------------------------------
!                                                                  Init
!----------------------------------------------------------------------------

SUBROUTINE init(self, progname, version, help, description, license, &
    & authors, examples, epilog, disable_hv, &
    & usage_lun, error_lun, version_lun, error_color, &
    & error_style, ignore_unknown_clas)
  !! Initialize CLI.
  CLASS(CommandLineInterface_), INTENT(inout) :: self
  !! CLI data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: progname
  !! Program name.
  CHARACTER(*), OPTIONAL, INTENT(in) :: version
  !! Program version.
  CHARACTER(*), OPTIONAL, INTENT(in) :: help
  !! Help message introducing the CLI usage.
  CHARACTER(*), OPTIONAL, INTENT(in) :: description
  !! Detailed description message introducing the program.
  CHARACTER(*), OPTIONAL, INTENT(in) :: license
  !! License description.
  CHARACTER(*), OPTIONAL, INTENT(in) :: authors
  !! Authors list.
  CHARACTER(*), OPTIONAL, INTENT(in) :: examples(1:)
  !! Examples of correct usage.
  CHARACTER(*), OPTIONAL, INTENT(in) :: epilog
  !! Epilog message.
  LOGICAL, OPTIONAL, INTENT(in) :: disable_hv
  !! Disable automatic insert of 'help' and 'version' CLAs.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: usage_lun
  !! Unit number to print usage/help.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: version_lun
  !! Unit number to print version/license info.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: error_lun
  !! Unit number to print error info.
  CHARACTER(*), OPTIONAL, INTENT(in) :: error_color
  !! ANSI color of error messages.
  CHARACTER(*), OPTIONAL, INTENT(in) :: error_style
  !! ANSI style of error messages.
  LOGICAL, OPTIONAL, INTENT(in) :: ignore_unknown_clas
  !! Disable errors-raising for passed unknown CLAs.
  CHARACTER(len=:), ALLOCATABLE :: prog_invocation
  !! Complete program invocation.
  INTEGER(I4P) :: invocation_length
  !! Length of invocation.
  INTEGER(I4P) :: retrieval_status
  !! Retrieval status.
  !> main
  CALL self%Deallocate()
  IF (PRESENT(progname)) THEN
    self%progname = progname
  ELSE
    ! try to set the default progname to the 0th command line entry a-la unix $0
    CALL get_command_ARGUMENT(0, length=invocation_length)
    ALLOCATE (CHARACTER(len=invocation_length) :: prog_invocation)
  CALL get_command_ARGUMENT(0, VALUE=prog_invocation, status=retrieval_status)
    IF (retrieval_status == 0) THEN
      self%progname = prog_invocation
    ELSE
      self%progname = 'program'
    END IF
  END IF
  self%version = 'unknown'
  IF (PRESENT(version)) self%version = version
  self%help = 'usage: '
  IF (PRESENT(help)) self%help = help
  self%description = ''
  IF (PRESENT(description)) self%description = description
  self%license = ''
  IF (PRESENT(license)) self%license = license
  self%authors = ''
  IF (PRESENT(authors)) self%authors = authors
  CALL self%set_examples(examples)
  self%epilog = ''
  IF (PRESENT(epilog)) self%epilog = epilog
  IF (PRESENT(disable_hv)) self%disable_hv = disable_hv
  ! default set by self%free
  IF (PRESENT(usage_lun)) self%usage_lun = usage_lun
  ! default set by self%free
  IF (PRESENT(version_lun)) self%version_lun = version_lun
  ! default set by self%free
  IF (PRESENT(error_lun)) self%error_lun = error_lun
  ! default set by self%free
  self%error_color = ''
  IF (PRESENT(error_color)) self%error_color = error_color
  self%error_style = ''
  IF (PRESENT(error_style)) self%error_style = error_style
  IF (PRESENT(ignore_unknown_clas)) &
       & self%ignore_unknown_clas = ignore_unknown_clas
  ! default set by self%free
  ! initialize only the first default group
  ALLOCATE (self%clasg(0:0))
  CALL self%clasg(0)%assign_object(self)
  self%clasg(0)%group = ''
END SUBROUTINE init

!----------------------------------------------------------------------------
!                                                                  add_group
!----------------------------------------------------------------------------

! Add CLAs group to CLI.

SUBROUTINE add_group(self, help, description, exclude, examples, group)
  CLASS(CommandLineInterface_), INTENT(inout) :: self
  !! CLI data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: help
  !! Help message.
  CHARACTER(*), OPTIONAL, INTENT(in) :: description
  !! Detailed description.
  CHARACTER(*), OPTIONAL, INTENT(in) :: exclude
  !! Group name of the mutually exclusive group.
  CHARACTER(*), OPTIONAL, INTENT(in) :: examples(1:)
  !! Examples of correct usage of the group.
  CHARACTER(*), INTENT(in) :: group
  !! Name of the grouped CLAs.
  TYPE(CommandLineGroupArg_), ALLOCATABLE :: clasg_list_new(:)
  !! New (extended) CLAs group list.
  CHARACTER(len=:), ALLOCATABLE :: helpd
  !! Help message.
  CHARACTER(len=:), ALLOCATABLE :: descriptiond
  !! Detailed description.
  CHARACTER(len=:), ALLOCATABLE :: excluded
  !! Group name of the mutually exclusive group.
  INTEGER(I4P) :: Ng
  !! Number of groups.
  INTEGER(I4P) :: gi
  !! Group index
  !> main
  IF (.NOT. self%is_defined_group(group=group)) THEN
    helpd = 'usage: '
    IF (PRESENT(help)) helpd = help
    descriptiond = ''
    IF (PRESENT(description)) descriptiond = description
    excluded = ''
    IF (PRESENT(exclude)) excluded = exclude
    Ng = SIZE(self%clasg, dim=1)
    ALLOCATE (clasg_list_new(0:Ng))
    !! clasg_list_new(0:Ng-1) = self%clasg(0:Ng-1)
    !! Not working on Intel Fortran 15.0.2
    DO gi = 0, Ng - 1
      clasg_list_new(gi) = self%clasg(gi)
    END DO
    CALL clasg_list_new(Ng)%assign_object(self)
    clasg_list_new(Ng)%help = helpd
    clasg_list_new(Ng)%description = descriptiond
    clasg_list_new(Ng)%group = group
    clasg_list_new(Ng)%m_exclude = excluded
    CALL clasg_list_new(Ng)%set_examples(examples)
    DEALLOCATE (self%clasg)
    ALLOCATE (self%clasg(0:Ng))
    self%clasg = clasg_list_new
    DEALLOCATE (clasg_list_new)
  END IF
END SUBROUTINE add_group

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

! Set two CLAs group ad mutually exclusive.
SUBROUTINE set_mutually_exclusive_groups(self, group1, group2)
  CLASS(CommandLineInterface_), INTENT(inout) :: self
  !! CLI data.
  CHARACTER(*), INTENT(in) :: group1
  !! Name of the first grouped CLAs.
  CHARACTER(*), INTENT(in) :: group2
  !! Name of the second grouped CLAs.
  INTEGER(I4P) :: g1
  !! Counter.
  INTEGER(I4P) :: g2
  !! Counter.
  IF (self%is_defined_group(group=group1, g=g1) &
       & .AND. self%is_defined_group(group=group2, g=g2)) THEN
    self%clasg(g1)%m_exclude = group2
    self%clasg(g2)%m_exclude = group1
  END IF
END SUBROUTINE set_mutually_exclusive_groups

!----------------------------------------------------------------------------
!                                                                  add
!----------------------------------------------------------------------------

! Add CLA to CLI.
!
! @note If not otherwise declared the action on CLA value is set to
! "store" a value that must be passed after the switch name
! or directly passed in case of positional CLA.
!
!@note
! If not otherwise speficied the CLA belongs to the default group
! "zero" that is the group of non-grouped CLAs.
!@endnote
!
!@note
!If CLA belongs to a not yet present group it is created on the fly.
!@endnote

SUBROUTINE add(self, pref, group, group_index, switch, &
     & switch_ab, help, help_markdown, help_color, help_style, &
     & required, positional, position, hidden, act, def, nargs, &
     & choices, exclude, envvar, error)
  CLASS(CommandLineInterface_), INTENT(inout) :: self
  !! CLI data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref
  !! Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group
  !! Name of the grouped CLAs.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: group_index
  !! Index of the grouped CLAs.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch
  !! Switch name.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch_ab
  !! Abbreviated switch name.
  CHARACTER(*), OPTIONAL, INTENT(in) :: help
  !! Help message describing the CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: help_color
  !! ANSI color of help messages.
  CHARACTER(*), OPTIONAL, INTENT(in) :: help_style
  !! ANSI style of help messages.
  CHARACTER(*), OPTIONAL, INTENT(in) :: help_markdown
  !! Longer help message, markdown formatted.
  LOGICAL, OPTIONAL, INTENT(in) :: required
  !! Flag for set required argument.
  LOGICAL, OPTIONAL, INTENT(in) :: positional
  !! Flag for checking if CLA is a positional or a named CLA.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position
  !! Position of positional CLA.
  LOGICAL, OPTIONAL, INTENT(in) :: hidden
  !! Flag for hiding CLA, thus it does not compare into help.
  CHARACTER(*), OPTIONAL, INTENT(in) :: act
  !! CLA value action.
  CHARACTER(*), OPTIONAL, INTENT(in) :: def
  !! Default value.
  CHARACTER(*), OPTIONAL, INTENT(in) :: nargs
  !! Number of arguments consumed by CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: choices
  !! List of allowable values for the argument.
  CHARACTER(*), OPTIONAL, INTENT(in) :: exclude
  !! Switch name of the mutually exclusive CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: envvar
  !! Environment variable from which take value.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error
  !! Error trapping flag.
  TYPE(CommandLineArg_) :: cla
  !! CLA data.
  INTEGER(I4P) :: g
  !! Counter.
  !> main
  ! initialize CLA
  CALL cla%assign_object(self)
  IF (PRESENT(switch)) THEN
    cla%switch = switch
    cla%switch_ab = switch
  ELSE
    IF (PRESENT(switch_ab)) THEN
      cla%switch = switch_ab
      cla%switch_ab = switch_ab
    END IF
  END IF
  IF (PRESENT(switch_ab)) cla%switch_ab = switch_ab
  cla%help = 'Undocumented argument'
  IF (PRESENT(help)) cla%help = help
  cla%help_color = ''
  IF (PRESENT(help_color)) cla%help_color = help_color
  cla%help_style = ''
  IF (PRESENT(help_style)) cla%help_style = help_style
  cla%help_markdown = ''
  IF (PRESENT(help_markdown)) cla%help_markdown = help_markdown
  cla%is_required = .FALSE.
  IF (PRESENT(required)) cla%is_required = required
  cla%is_positional = .FALSE.
  IF (PRESENT(positional)) cla%is_positional = positional
  cla%position = 0_I4P
  IF (PRESENT(position)) cla%position = position
  cla%is_hidden = .FALSE.
  IF (PRESENT(hidden)) cla%is_hidden = hidden
  cla%act = action_store
  IF (PRESENT(act)) cla%act = TRIM(ADJUSTL(Upper_Case(act)))
  IF (PRESENT(def)) cla%def = def
  IF (PRESENT(def)) cla%val = def
  IF (PRESENT(nargs)) cla%nargs = nargs
  IF (PRESENT(choices)) cla%choices = choices
  cla%m_exclude = ''
  IF (PRESENT(exclude)) cla%m_exclude = exclude
  IF (PRESENT(envvar)) cla%envvar = envvar
  CALL cla%check(pref=pref)
  self%error = cla%error
  IF (self%error /= 0) THEN
    IF (PRESENT(error)) error = self%error
    RETURN
  END IF
  ! add CLA to CLI
  IF ((.NOT. PRESENT(group)) .AND. (.NOT. PRESENT(group_index))) THEN
    CALL self%clasg(0)%add(pref=pref, cla=cla)
    self%error = self%clasg(0)%error
  ELSEIF (PRESENT(group)) THEN
    IF (self%is_defined_group(group=group, g=g)) THEN
      CALL self%clasg(g)%add(pref=pref, cla=cla)
      self%error = self%clasg(g)%error
    ELSE
      CALL self%add_group(group=group)
      CALL self%clasg(SIZE(self%clasg, dim=1) - 1)%add(pref=pref, cla=cla)
      self%error = self%clasg(SIZE(self%clasg, dim=1) - 1)%error
    END IF
  ELSEIF (PRESENT(group_index)) THEN
    IF (group_index <= SIZE(self%clasg, dim=1) - 1) THEN
      CALL self%clasg(group_index)%add(pref=pref, cla=cla)
      self%error = self%clasg(group_index)%error
    END IF
  END IF
  IF (PRESENT(error)) error = self%error
END SUBROUTINE add

!----------------------------------------------------------------------------
!                                                                  check
!----------------------------------------------------------------------------

SUBROUTINE check(self, pref, error)
  !! Check data consistency.
  CLASS(CommandLineInterface_), INTENT(INOUT) :: self  !! CLI data.
  CHARACTER(*), OPTIONAL, INTENT(IN) :: pref  !! Prefixing string.
  INTEGER(I4P), OPTIONAL, INTENT(OUT) :: error !! Error trapping flag.
  INTEGER(I4P) :: g     !! Counter.
  INTEGER(I4P) :: gg    !! Counter.

  DO g = 0, SIZE(self%clasg, dim=1) - 1
    ! check group consistency
    CALL self%clasg(g)%check(pref=pref)
    self%error = self%clasg(g)%error
    IF (PRESENT(error)) error = self%error
    IF (self%error /= 0) EXIT
    ! check mutually exclusive interaction
    IF (g > 0) THEN
      IF (self%clasg(g)%m_exclude /= '') THEN
        IF (self%is_defined_group( &
             & group=self%clasg(g)%m_exclude, g=gg)) &
             & self%clasg(gg)%m_exclude = self%clasg(g)%group
      END IF
    END IF
  END DO
END SUBROUTINE check

!----------------------------------------------------------------------------
!                                                                  check
!----------------------------------------------------------------------------

! Check if two mutually exclusive CLAs group have been called.
SUBROUTINE check_m_exclusive(self, pref)
  CLASS(CommandLineInterface_), INTENT(inout) :: self
  !! CLI data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref
  !! Prefixing string.
  INTEGER(I4P) :: g
  !! Counter.
  INTEGER(I4P) :: gg
  !! Counter.
  !> main
  DO g = 1, SIZE(self%clasg, dim=1) - 1
    IF (self%clasg(g)%is_called &
         & .AND. (self%clasg(g)%m_exclude /= '')) THEN
      IF (self%is_defined_group(group=self%clasg(g)%m_exclude, g=gg)) THEN
        IF (self%clasg(gg)%is_called) THEN
          CALL self%clasg(g)%raise_error_m_exclude(pref=pref)
          self%error = self%clasg(g)%error
          EXIT
        END IF
      END IF
    END IF
  END DO
END SUBROUTINE check_m_exclusive

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

FUNCTION is_passed(self, group, switch, position)
  !! Check if a CLA has been passed.
  CLASS(CommandLineInterface_), INTENT(in) :: self
  !! CLI data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group
  !! Name of group (command) of CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch
  !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position
  !! Position of positional CLA.
  LOGICAL :: is_passed
  !! Check if a CLA has been passed.
  INTEGER(I4P) :: g
  !! Counter.
  is_passed = .FALSE.
  IF (.NOT. PRESENT(group)) THEN
    IF (PRESENT(switch)) THEN
      is_passed = self%clasg(0)%is_passed(switch=switch)
    ELSEIF (PRESENT(position)) THEN
      is_passed = self%clasg(0)%is_passed(position=position)
    END IF
  ELSE
    IF (self%is_defined_group(group=group, g=g)) THEN
      IF (PRESENT(switch)) THEN
        is_passed = self%clasg(g)%is_passed(switch=switch)
      ELSEIF (PRESENT(position)) THEN
        is_passed = self%clasg(g)%is_passed(position=position)
      END IF
    END IF
  END IF
END FUNCTION is_passed

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

FUNCTION is_defined_group(self, group, g) RESULT(defined)
  !! Check if a CLAs group has been defined.
  CLASS(CommandLineInterface_), INTENT(in) :: self
  !! CLI data.
  CHARACTER(*), INTENT(in) :: group
  !! Name of group (command) of CLAs.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: g
  !! Index of group.
  LOGICAL :: defined
  !! Check if a CLAs group has been defined.
  INTEGER(I4P) :: gg
  !! Counter.
  INTEGER(I4P) :: ggg
  !! Counter.
  defined = .FALSE.
  DO gg = 0, SIZE(self%clasg, dim=1) - 1
    ggg = gg
    IF (ALLOCATED(self%clasg(gg)%group)) &
         & defined = (self%clasg(gg)%group == group)
    IF (defined) EXIT
  END DO
  IF (PRESENT(g)) g = ggg
END FUNCTION is_defined_group

FUNCTION is_called_group(self, group) RESULT(called)
  !! Check if a CLAs group has been run.
  CLASS(CommandLineInterface_), INTENT(in) :: self
  !! CLI data.
  CHARACTER(*), INTENT(in) :: group
  !! Name of group (command) of CLAs.
  LOGICAL :: called
  !! Check if a CLAs group has been runned.
  INTEGER(I4P) :: g
  !! Counter.
  called = .FALSE.
  IF (self%is_defined_group(group=group, g=g)) &
       & called = self%clasg(g)%is_called
END FUNCTION is_called_group

FUNCTION is_defined(self, switch, group)
  !! Check if a CLA has been defined.
  CLASS(CommandLineInterface_), INTENT(in) :: self       !! CLI data.
  CHARACTER(*), INTENT(in) :: switch     !! Switch name.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group      !! Name of group (command) of CLAs.
  LOGICAL :: is_defined !! Check if a CLA has been defined.
  INTEGER(I4P) :: g          !! Counter.

  is_defined = .FALSE.
  IF (.NOT. PRESENT(group)) THEN
    is_defined = self%clasg(0)%is_defined(switch=switch)
  ELSE
    IF (self%is_defined_group(group=group, g=g)) is_defined = self%clasg(g)%is_defined(switch=switch)
  END IF
END FUNCTION is_defined

ELEMENTAL FUNCTION is_parsed(self)
  !! Check if CLI has been parsed.
  CLASS(CommandLineInterface_), INTENT(in) :: self      !! CLI data.
  LOGICAL :: is_parsed !! Parsed status.

  is_parsed = self%is_parsed_
END FUNCTION is_parsed

SUBROUTINE parse(self, pref, args, error)
  !! Parse Command Line Interfaces by means of a previously initialized CLAs groups list.
  !!
  !! @note The leading and trailing white spaces are removed from CLA values.
  !!
  !! @note If the *args* argument is passed the command line arguments are taken from it and not from the actual program CLI
  !! invocations.
  CLASS(CommandLineInterface_), INTENT(inout) :: self    !! CLI data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref    !! Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: args    !! String containing command line arguments.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error   !! Error trapping flag.
  INTEGER(I4P) :: g       !! Counter for CLAs group.
  INTEGER(I4P), ALLOCATABLE :: ai(:, :) !! Counter for CLAs grouped.

  IF (PRESENT(error)) error = 0
  IF (self%is_parsed_) RETURN

  ! add help and version switches if not done by user
  IF (.NOT. self%disable_hv) THEN
    DO g = 0, SIZE(self%clasg, dim=1) - 1
      IF (.NOT. (self%is_defined( &
           & group=self%clasg(g)%group, switch='--help') .AND. &
           & self%is_defined(group=self%clasg(g)%group, switch='-h'))) &
           & CALL self%add(pref=pref, &
                            group_index=g, &
                            switch='--help', &
                            switch_ab='-h', &
                            help='Print this help message', &
                            required=.FALSE., &
                            def='', &
                            act='print_help')
      IF (.NOT. (self%is_defined(group=self%clasg(g)%group, &
           & switch='--version') .AND. &
           & self%is_defined(group=self%clasg(g)%group, switch='-v'))) &
           CALL self%add(pref=pref, &
           & group_index=g, &
                      switch='--version', &
                      switch_ab='-v', &
                      help='Print version', &
                      required=.FALSE., &
                      def='', &
                      act='print_version')
    END DO
  END IF
  ! add hidden CLA '--' for getting the rid of eventual trailing CLAs garbage
  DO g = 0, SIZE(self%clasg, dim=1) - 1
    IF (.NOT. self%is_defined(group=self%clasg(g)%group, switch='--')) &
      CALL self%add(pref=pref, group_index=g, switch='--', required=.FALSE., &
      & hidden=.TRUE., nargs='*', def='', act='store')
  END DO
  ! parse passed CLAs grouping in indexes
  IF (PRESENT(args)) THEN
    CALL self%get_args(args=args, ai=ai)
  ELSE
    CALL self%get_args(ai=ai)
  END IF
  ! check CLI consistency
  CALL self%check(pref=pref)
  IF (self%error > 0) THEN
    IF (((self%error == ERROR_UNKNOWN) &
         & .AND. (.NOT. self%ignore_unknown_clas)) &
         & .OR. (self%error /= ERROR_UNKNOWN)) THEN
      IF (PRESENT(error)) error = self%error
      RETURN
    ELSE
      self%error_unknown_clas = ERROR_UNKNOWN_CLAS_IGNORED
    END IF
  END IF
  ! parse CLI
  DO g = 0, SIZE(ai, dim=1) - 1
    IF (ai(g, 1) > 0) THEN
      CALL self%clasg(g)%parse( &
           & args=self%args(ai(g, 1):ai(g, 2)), &
           & ignore_unknown_clas=self%ignore_unknown_clas, &
           & pref=pref, error_unknown_clas=self%error_unknown_clas)
    ELSE
      CALL self%clasg(g)%sanitize_defaults
    END IF
    self%error = self%clasg(g)%error
    IF (self%error < 0) EXIT
    IF (self%error > 0) THEN
      IF (((self%error == ERROR_UNKNOWN) &
           & .AND. (.NOT. self%ignore_unknown_clas)) &
           & .OR. (self%error /= ERROR_UNKNOWN)) THEN
        IF (PRESENT(error)) error = self%error
        EXIT
      ELSE
        self%error_unknown_clas = ERROR_UNKNOWN_CLAS_IGNORED
      END IF
    END IF
  END DO
  IF (self%error > 0) THEN
    IF (((self%error == ERROR_UNKNOWN) &
         & .AND. (.NOT. self%ignore_unknown_clas)) &
         & .OR. (self%error /= ERROR_UNKNOWN)) THEN
      IF (PRESENT(error)) error = self%error
      RETURN
    ELSE
      self%error_unknown_clas = ERROR_UNKNOWN_CLAS_IGNORED
    END IF
  END IF

  ! trap the special cases of version/help printing
  IF (self%error == STATUS_PRINT_V) THEN
    CALL self%print_version(pref=pref)
    STOP
  ELSEIF (self%error == STATUS_PRINT_H) THEN
    DO g = 0, SIZE(ai, dim=1) - 1
      IF (self%clasg(g)%error == STATUS_PRINT_H) THEN
        WRITE (self%usage_lun, '(A)') self%usage(pref=pref, g=g)
        STOP
      END IF
    END DO
  END IF

  ! check if all required CLAs have been passed
  DO g = 0, SIZE(ai, dim=1) - 1
    CALL self%clasg(g)%is_required_passed(pref=pref)
    self%error = self%clasg(g)%error
    IF (self%error > 0) THEN
      IF (((self%error == ERROR_UNKNOWN) &
           & .AND. (.NOT. self%ignore_unknown_clas)) &
           & .OR. (self%error /= ERROR_UNKNOWN)) THEN
        IF (PRESENT(error)) error = self%error
        EXIT
      ELSE
        self%error_unknown_clas = ERROR_UNKNOWN_CLAS_IGNORED
      END IF
    END IF
  END DO
  IF (self%error > 0) THEN
    IF (((self%error == ERROR_UNKNOWN) &
         & .AND. (.NOT. self%ignore_unknown_clas)) &
         & .OR. (self%error /= ERROR_UNKNOWN)) THEN
      IF (PRESENT(error)) error = self%error
      RETURN
    ELSE
      self%error_unknown_clas = ERROR_UNKNOWN_CLAS_IGNORED
    END IF
  END IF
  ! check mutually exclusive interaction
  CALL self%check_m_exclusive(pref=pref)
  self%is_parsed_ = .TRUE.
  ! check if the only error found is for unknown passed CLAs
  ! and if it is ignored by the user
  IF (self%error == ERROR_UNKNOWN &
       & .AND. self%error_unknown_clas == ERROR_UNKNOWN_CLAS_IGNORED) &
       & self%error = ERROR_UNKNOWN_CLAS_IGNORED
  IF (PRESENT(error)) error = self%error
END SUBROUTINE parse

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

  !! Get the argument indexes of CLAs groups defined parsing
  !! the actual passed CLAs.
SUBROUTINE get_clasg_indexes(self, ai)
  CLASS(CommandLineInterface_), INTENT(inout) :: self   !! CLI data.
  INTEGER(I4P), ALLOCATABLE, INTENT(out) :: ai(:, :)!! CLAs grouped indexes.
  INTEGER(I4P) :: Na     !! Number of command line arguments passed.
  INTEGER(I4P) :: a      !! Counter for CLAs.
  INTEGER(I4P) :: aa     !! Counter for CLAs.
  INTEGER(I4P) :: g      !! Counter for CLAs group.
  LOGICAL :: found  !! Flag for inquiring if a named group is found.

  ALLOCATE (ai(0:SIZE(self%clasg, dim=1) - 1, 1:2))
  ai = 0
  IF (ALLOCATED(self%args)) THEN
    Na = SIZE(self%args, dim=1)
    a = 0
    found = .FALSE.
    search_named: DO WHILE (a < Na)
      a = a + 1
      IF (self%is_defined_group(group=TRIM(self%args(a)), g=g)) THEN
        found = .TRUE.
        self%clasg(g)%is_called = .TRUE.
        ai(g, 1) = a + 1
        aa = a
        DO WHILE (aa < Na)
          aa = aa + 1
          IF (self%is_defined_group(group=TRIM(self%args(aa)))) THEN
            a = aa - 1
            ai(g, 2) = a
            EXIT
          ELSE
            ai(g, 2) = aa
          END IF
        END DO
      ELSEIF (.NOT. found) THEN
        ai(0, 2) = a
      END IF
    END DO search_named
    IF (ai(0, 2) > 0) THEN
      ai(0, 1) = 1
      self%clasg(0)%is_called = .TRUE.
    ELSEIF (ALL(ai == 0)) THEN
      self%clasg(0)%is_called = .TRUE.
    END IF
  ELSE
    self%clasg(0)%is_called = .TRUE.
  END IF
END SUBROUTINE get_clasg_indexes

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

SUBROUTINE get_args_from_string(self, args, ai)
  !! Get CLAs from string.
  CLASS(CommandLineInterface_), INTENT(inout) :: self   !! CLI data.
  CHARACTER(*), INTENT(in) :: args   !! String containing command line arguments.
  INTEGER(I4P), ALLOCATABLE, INTENT(out) :: ai(:, :)!! CLAs grouped indexes.
  CHARACTER(len=len_TRIM(args)) :: argsd  !! Dummy string containing command line arguments.
  CHARACTER(len=len_TRIM(args)), ALLOCATABLE :: toks(:)!! CLAs tokenized.
  INTEGER(I4P) :: Nt     !! Number of tokens.
  INTEGER(I4P) :: Na     !! Number of command line arguments passed.
  INTEGER(I4P) :: a      !! Counter for CLAs.
  INTEGER(I4P) :: t      !! Counter for tokens.
  INTEGER(I4P) :: c      !! Counter for characters inside tokens.
#ifndef __GFORTRAN__
  INTEGER(I4P) :: length !! Maxium lenght of arguments string.
#endif

  ! prepare CLI arguments list
  IF (ALLOCATED(self%args)) DEALLOCATE (self%args)

  ! sanitize arguments string
  argsd = TRIM(args)
  IF (INDEX(args, "'") > 0) THEN
    argsd = sanitize_args(argsin=argsd, delimiter="'")
  ELSEIF (INDEX(args, '"') > 0) THEN
    argsd = sanitize_args(argsin=argsd, delimiter='"')
  END IF

  ! tokenize arguments string; the previously sanitized white spaces inside tokens are restored
  CALL tokenize(strin=argsd, delimiter=' ', toks=toks, Nt=Nt)
  Na = 0
  find_number_of_valid_arguments: DO t = 1, Nt
    IF (TRIM(ADJUSTL(toks(t))) /= '') THEN
      Na = Na + 1
      DO c = 1, LEN(toks(t))
        IF (toks(t) (c:c) == "'") toks(t) (c:c) = " "
      END DO
    END IF
  END DO find_number_of_valid_arguments

  IF (Na > 0) THEN
    ! allocate CLI arguments list
#ifdef __GFORTRAN__
    ALLOCATE (self%args(1:Na))
#else
    length = 0
    find_longest_arg: DO t = 1, Nt
      IF (TRIM(ADJUSTL(toks(t)))/='') length = MAX(length,len_TRIM(ADJUSTL(toks(t))))
    END DO find_longest_arg
    ALLOCATE (CHARACTER(length) :: self%args(1:Na))
#endif

    ! construct arguments list
    a = 0
    get_args: DO t = 1, Nt
      IF (TRIM(ADJUSTL(toks(t))) /= '') THEN
        a = a + 1
        self%args(a) = TRIM(ADJUSTL(toks(t)))
      END IF
    END DO get_args
  END IF

  CALL self%get_clasg_indexes(ai=ai)
CONTAINS
  FUNCTION sanitize_args(argsin, delimiter) RESULT(sanitized)
    !! Sanitize arguments string.
    !!
    !! Substitute white spaces enclosed into string-arguments, i.e. 'string argument with spaces...' or
    !! "string argument with spaces..." with a safe equivalent for tokenization against white spaces, i.e. the finally tokenized
    !! string is string'argument'with'spaces...
    !!
    !! @note The white spaces are reintroduce later.
    CHARACTER(*), INTENT(in) :: argsin    !! Arguments string.
    CHARACTER(*), INTENT(in) :: delimiter !! Delimiter enclosing string argument.
    CHARACTER(len=len_TRIM(argsin)) :: sanitized !! Arguments string sanitized.
    CHARACTER(len=len_TRIM(argsin)), ALLOCATABLE :: tok(:)    !! Arguments string tokens.
    INTEGER(I4P) :: Nt        !! Number of command line arguments passed.
    INTEGER(I4P) :: t         !! Counter.
    INTEGER(I4P) :: tt        !! Counter.

    CALL tokenize(strin=TRIM(argsin), delimiter=delimiter, toks=tok, Nt=Nt)
    DO t = 2, Nt, 2
      DO tt = 1, len_TRIM(ADJUSTL(tok(t)))
        IF (tok(t) (tt:tt) == ' ') tok(t) (tt:tt) = "'"
      END DO
    END DO
    sanitized = ''
    DO t = 1, Nt
      sanitized = TRIM(sanitized)//" "//TRIM(ADJUSTL(tok(t)))
    END DO
    sanitized = TRIM(ADJUSTL(sanitized))
  END FUNCTION sanitize_args
END SUBROUTINE get_args_from_string

SUBROUTINE get_args_from_invocation(self, ai)
  !! Get CLAs from CLI invocation.
  CLASS(CommandLineInterface_), INTENT(inout) :: self    !! CLI data.
  INTEGER(I4P), ALLOCATABLE, INTENT(out) :: ai(:, :) !! CLAs grouped indexes.
  INTEGER(I4P) :: Na      !! Number of command line arguments passed.
  CHARACTER(max_val_len) :: switch  !! Switch name.
  INTEGER(I4P) :: a       !! Counter for CLAs.
  INTEGER(I4P) :: aa      !! Counter for CLAs.

  IF (ALLOCATED(self%args)) DEALLOCATE (self%args)
  Na = command_argument_COUNT()
  IF (Na > 0) THEN
#ifdef __GFORTRAN__
    ALLOCATE (self%args(1:Na))
#else
    aa = 0
    find_longest_arg: DO a = 1, Na
      CALL get_command_ARGUMENT(a, switch)
      aa = MAX(aa, len_TRIM(switch))
    END DO find_longest_arg
    ALLOCATE (CHARACTER(aa) :: self%args(1:Na))
#endif
    get_args: DO a = 1, Na
      CALL get_command_ARGUMENT(a, switch)
      self%args(a) = TRIM(ADJUSTL(switch))
    END DO get_args
  END IF

  CALL self%get_clasg_indexes(ai=ai)
END SUBROUTINE get_args_from_invocation

SUBROUTINE get_cla(self, val, pref, args, group, switch, position, error)
  !! Get CLA (single) value from CLAs list parsed.
  !!
  !! @note For logical type CLA the value is directly read without any robust error trapping.
  CLASS(CommandLineInterface_), INTENT(inout) :: self     !! CLI data.
  CLASS(*), INTENT(inout) :: val      !! CLA value.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !! Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: args     !! String containing command line arguments.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group    !! Name of group (command) of CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch   !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position !! Position of positional CLA.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error    !! Error trapping flag.
  LOGICAL :: found    !! Flag for checking if CLA containing switch has been found.
  INTEGER(I4P) :: g        !! Group counter.
  INTEGER(I4P) :: a        !! Argument counter.

  IF (.NOT. self%is_parsed_) THEN
    CALL self%parse(pref=pref, args=args, error=error)
    IF (self%error>0.AND.self%error_unknown_clas/=ERROR_UNKNOWN_CLAS_IGNORED) RETURN
  END IF
  IF (PRESENT(group)) THEN
    IF (.NOT. self%is_defined_group(group=group, g=g)) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_GROUP, group=group)
    END IF
  ELSE
    g = 0
  END IF
IF (self%error==0.OR.self%error_unknown_clas==ERROR_UNKNOWN_CLAS_IGNORED) THEN
    IF (PRESENT(switch)) THEN
      ! search for the CLA corresponding to switch
      found = .FALSE.
      DO a = 1, self%clasg(g)%Na
        IF (.NOT. self%clasg(g)%cla(a)%is_positional) THEN
          IF ((self%clasg(g)%cla(a)%switch==switch).OR.(self%clasg(g)%cla(a)%switch_ab==switch)) THEN
            found = .TRUE.
            EXIT
          END IF
        END IF
      END DO
      IF (.NOT. found) THEN
        CALL self%errored(pref=pref, error=ERROR_MISSING_CLA, switch=switch)
      ELSE
        CALL self%clasg(g)%cla(a)%get(pref=pref, val=val); self%error = self%clasg(g)%cla(a)%error
      END IF
    ELSEIF (PRESENT(position)) THEN
      CALL self%clasg(g)%cla(position)%get(pref=pref, val=val); self%error = self%clasg(g)%cla(position)%error
    ELSE
      CALL self%errored(pref=pref, error=ERROR_MISSING_SELECTION_CLA)
    END IF
  END IF
  ! check if the only error found is for unknown passed CLAs and if it is ignored by the user
  IF (self%error==ERROR_UNKNOWN.AND.self%error_unknown_clas==ERROR_UNKNOWN_CLAS_IGNORED) self%error = ERROR_UNKNOWN_CLAS_IGNORED
  IF (self%error == 0 .AND. (.NOT. self%clasg(g)%is_called)) THEN
    ! TODO warn (if liked) for non invoked group querying
  END IF
  IF (PRESENT(error)) error = self%error
END SUBROUTINE get_cla

SUBROUTINE get_cla_list(self, val, pref, args, group, switch, position, error)
  !! Get CLA multiple values from CLAs list parsed.
  !!
  !! @note For logical type CLA the value is directly read without any robust error trapping.
  CLASS(CommandLineInterface_), INTENT(inout) :: self     !! CLI data.
  CLASS(*), INTENT(inout) :: val(1:)  !! CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !! Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: args     !! String containing command line arguments.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group    !! Name of group (command) of CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch   !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position !! Position of positional CLA.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error    !! Error trapping flag.
  LOGICAL :: found    !! Flag for checking if CLA containing switch has been found.
  INTEGER(I4P) :: g        !! Group counter.
  INTEGER(I4P) :: a        !! Argument counter.

  IF (.NOT. self%is_parsed_) THEN
    CALL self%parse(pref=pref, args=args, error=error)
    IF (self%error>0.AND.self%error_unknown_clas/=ERROR_UNKNOWN_CLAS_IGNORED) RETURN
  END IF
  IF (PRESENT(group)) THEN
    IF (.NOT. self%is_defined_group(group=group, g=g)) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_GROUP, group=group)
    END IF
  ELSE
    g = 0
  END IF
  IF (PRESENT(switch)) THEN
    ! search for the CLA corresponding to switch
    found = .FALSE.
    DO a = 1, self%clasg(g)%Na
      IF (.NOT. self%clasg(g)%cla(a)%is_positional) THEN
        IF ((self%clasg(g)%cla(a)%switch==switch).OR.(self%clasg(g)%cla(a)%switch_ab==switch)) THEN
          found = .TRUE.
          EXIT
        END IF
      END IF
    END DO
    IF (.NOT. found) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_CLA, switch=switch)
    ELSE
      CALL self%clasg(g)%cla(a)%get(pref=pref, val=val); self%error = self%clasg(g)%cla(a)%error
    END IF
  ELSEIF (PRESENT(position)) THEN
    CALL self%clasg(g)%cla(position)%get(pref=pref, val=val); self%error = error
  ELSE
    CALL self%errored(pref=pref, error=ERROR_MISSING_SELECTION_CLA)
  END IF
  ! check if the only error found is for unknown passed CLAs and if it is ignored by the user
  IF (self%error==ERROR_UNKNOWN.AND.self%error_unknown_clas==ERROR_UNKNOWN_CLAS_IGNORED) self%error = ERROR_UNKNOWN_CLAS_IGNORED
  IF (PRESENT(error)) error = self%error
END SUBROUTINE get_cla_list

#if defined USE_Real128
SUBROUTINE get_cla_list_varying_R16P(self, val, pref, args, group, &
     & switch, position, error)
  !! Get CLA multiple values from CLAs list parsed with varying size list, real(R16P).
  !!
  !! @note The CLA list is returned deallocated if values are not correctly gotten.
  !!
  !! @note For logical type CLA the value is directly read without any robust error trapping.
  CLASS(CommandLineInterface_), INTENT(inout) :: self     !! CLI data.
  REAL(R16P), ALLOCATABLE, INTENT(out) :: val(:)   !! CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !! Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: args     !! String containing command line arguments.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group    !! Name of group (command) of CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch   !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position !! Position of positional CLA.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error    !! Error trapping flag.
  LOGICAL :: found    !! Flag for checking if CLA containing switch has been found.
  INTEGER(I4P) :: g        !! Group counter.
  INTEGER(I4P) :: a        !! Argument counter.

  IF (.NOT. self%is_parsed_) THEN
    CALL self%parse(pref=pref, args=args, error=error)
    IF (self%error > 0 &
         & .AND. self%error_unknown_clas /= ERROR_UNKNOWN_CLAS_IGNORED) &
         & RETURN
  END IF
  IF (PRESENT(group)) THEN
    IF (.NOT. self%is_defined_group(group=group, g=g)) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_GROUP, group=group)
    END IF
  ELSE
    g = 0
  END IF
  IF (PRESENT(switch)) THEN
    ! search for the CLA corresponding to switch
    found = .FALSE.
    DO a = 1, self%clasg(g)%Na
      IF (.NOT. self%clasg(g)%cla(a)%is_positional) THEN
        IF ((self%clasg(g)%cla(a)%switch == switch) &
             & .OR. (self%clasg(g)%cla(a)%switch_ab == switch)) THEN
          found = .TRUE.
          EXIT
        END IF
      END IF
    END DO
    IF (.NOT. found) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_CLA, switch=switch)
    ELSE
      CALL self%clasg(g)%cla(a)%get_varying(pref=pref, val=val)
      self%error = self%clasg(g)%cla(a)%error
    END IF
  ELSEIF (PRESENT(position)) THEN
    CALL self%clasg(g)%cla(position)%get_varying(pref=pref, val=val); self%error = error
  ELSE
    CALL self%errored(pref=pref, error=ERROR_MISSING_SELECTION_CLA)
  END IF
  ! check if the only error found is for unknown passed CLAs and if it is ignored by the user
  IF (self%error == ERROR_UNKNOWN &
       & .AND. self%error_unknown_clas == ERROR_UNKNOWN_CLAS_IGNORED) &
       & self%error = ERROR_UNKNOWN_CLAS_IGNORED
  IF (PRESENT(error)) error = self%error
END SUBROUTINE get_cla_list_varying_R16P
#endif

!----------------------------------------------------------------------------
!                                                                  name
!----------------------------------------------------------------------------

! Get CLA multiple values from CLAs list parsed with varying size list, real(R8P).
!
!@note
! The CLA list is returned deallocated if values are not correctly gotten.
!@endnote
!
!@note
!For logical type CLA the value is directly read without any robust error trapping.
!@endnote

SUBROUTINE get_cla_list_varying_R8P(self, val, pref, args, group, &
     & switch, position, error)
  CLASS(CommandLineInterface_), INTENT(inout) :: self
  !! CLI data.
  REAL(R8P), ALLOCATABLE, INTENT(out) :: val(:)
  !! CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref
  !! Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: args
  !! String containing command line arguments.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group
  !! Name of group (command) of CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch
  !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position
  !! Position of positional CLA.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error
  !! Error trapping flag.
  LOGICAL :: found
  !! Flag for checking if CLA containing switch has been found.
  INTEGER(I4P) :: g
  !! Group counter.
  INTEGER(I4P) :: a
  !! Argument counter.
  !> main
  IF (.NOT. self%is_parsed_) THEN
    CALL self%parse(pref=pref, args=args, error=error)
    IF (self%error > 0 .AND. &
         & self%error_unknown_clas /= ERROR_UNKNOWN_CLAS_IGNORED) RETURN
  END IF
  IF (PRESENT(group)) THEN
    IF (.NOT. self%is_defined_group(group=group, g=g)) THEN
      CALL self%errored(pref=pref, &
           & error=ERROR_MISSING_GROUP, group=group)
    END IF
  ELSE
    g = 0
  END IF
  IF (PRESENT(switch)) THEN
    ! search for the CLA corresponding to switch
    found = .FALSE.
    DO a = 1, self%clasg(g)%Na
      IF (.NOT. self%clasg(g)%cla(a)%is_positional) THEN
        IF ((self%clasg(g)%cla(a)%switch == switch) &
             & .OR. (self%clasg(g)%cla(a)%switch_ab == switch)) THEN
          found = .TRUE.
          EXIT
        END IF
      END IF
    END DO
    IF (.NOT. found) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_CLA, &
           & switch=switch)
    ELSE
      CALL self%clasg(g)%cla(a)%get_varying(pref=pref, val=val)
      self%error = self%clasg(g)%cla(a)%error
    END IF
  ELSEIF (PRESENT(position)) THEN
    CALL self%clasg(g)%cla(position)%get_varying(pref=pref, val=val)
    self%error = error
  ELSE
    CALL self%errored(pref=pref, error=ERROR_MISSING_SELECTION_CLA)
  END IF
  !! check if the only error found is for unknown passed
  !! CLAs and if it is ignored by the user
  IF (self%error == ERROR_UNKNOWN .AND. &
       & self%error_unknown_clas == ERROR_UNKNOWN_CLAS_IGNORED) &
       & self%error = ERROR_UNKNOWN_CLAS_IGNORED
  IF (PRESENT(error)) error = self%error
END SUBROUTINE get_cla_list_varying_R8P

! Get CLA multiple values from CLAs list parsed with
! varying size list, real(R4P).
!
! @note The CLA list is returned deallocated if values are
! not correctly gotten.
!
! @note For logical type CLA the value is directly read without
! any robust error trapping.

SUBROUTINE get_cla_list_varying_R4P(self, val, pref, args, group, &
     & switch, position, error)
  CLASS(CommandLineInterface_), INTENT(inout) :: self
  !! CLI data.
  REAL(R4P), ALLOCATABLE, INTENT(out) :: val(:)
  !! CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref
  !! Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: args
  !! String containing command line arguments.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group
  !! Name of group (command) of CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch
  !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position
  !! Position of positional CLA.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error
  !! Error trapping flag.
  LOGICAL :: found
  !! Flag for checking if CLA containing switch has been found.
  INTEGER(I4P) :: g
  !! Group counter.
  INTEGER(I4P) :: a
  !! Argument counter.

  IF (.NOT. self%is_parsed_) THEN
    CALL self%parse(pref=pref, args=args, error=error)
    IF (self%error > 0 .AND. &
         & self%error_unknown_clas /= ERROR_UNKNOWN_CLAS_IGNORED) RETURN
  END IF
  IF (PRESENT(group)) THEN
    IF (.NOT. self%is_defined_group(group=group, g=g)) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_GROUP, group=group)
    END IF
  ELSE
    g = 0
  END IF
  IF (PRESENT(switch)) THEN
    ! search for the CLA corresponding to switch
    found = .FALSE.
    DO a = 1, self%clasg(g)%Na
      IF (.NOT. self%clasg(g)%cla(a)%is_positional) THEN
        IF ((self%clasg(g)%cla(a)%switch == switch) &
             & .OR. (self%clasg(g)%cla(a)%switch_ab == switch)) THEN
          found = .TRUE.
          EXIT
        END IF
      END IF
    END DO
    IF (.NOT. found) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_CLA, switch=switch)
    ELSE
      CALL self%clasg(g)%cla(a)%get_varying(pref=pref, val=val)
      self%error = self%clasg(g)%cla(a)%error
    END IF
  ELSEIF (PRESENT(position)) THEN
    CALL self%clasg(g)%cla(position)%get_varying(pref=pref, val=val)
    self%error = error
  ELSE
    CALL self%errored(pref=pref, error=ERROR_MISSING_SELECTION_CLA)
  END IF
  ! check if the only error found is for unknown passed CLAs and if it is ignored by the user
  IF (self%error == ERROR_UNKNOWN .AND. &
       & self%error_unknown_clas == ERROR_UNKNOWN_CLAS_IGNORED) &
       & self%error = ERROR_UNKNOWN_CLAS_IGNORED
  IF (PRESENT(error)) error = self%error
END SUBROUTINE get_cla_list_varying_R4P

! Get CLA multiple values from CLAs list parsed with varying size list, integer(I8P).
!
! @note The CLA list is returned deallocated if values are not correctly gotten.
!
! @note For logical type CLA the value is directly read without any robust error trapping.

SUBROUTINE get_cla_list_varying_I8P(self, val, pref, args, &
     & group, switch, position, error)
  CLASS(CommandLineInterface_), INTENT(inout) :: self
  !! CLI data.
  INTEGER(I8P), ALLOCATABLE, INTENT(out) :: val(:)
  !! CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref
  !! Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: args
  !! String containing command line arguments.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group
  !! Name of group (command) of CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch
  !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position
  !! Position of positional CLA.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error
  !! Error trapping flag.
  LOGICAL :: found
  !! Flag for checking if CLA containing switch has been found.
  INTEGER(I4P) :: g
  !! Group counter.
  INTEGER(I4P) :: a
  !! Argument counter.
  IF (.NOT. self%is_parsed_) THEN
    CALL self%parse(pref=pref, args=args, error=error)
    IF (self%error > 0 &
         & .AND. self%error_unknown_clas /= ERROR_UNKNOWN_CLAS_IGNORED) RETURN
  END IF
  IF (PRESENT(group)) THEN
    IF (.NOT. self%is_defined_group(group=group, g=g)) THEN
      CALL self%errored(pref=pref, &
           & error=ERROR_MISSING_GROUP, group=group)
    END IF
  ELSE
    g = 0
  END IF
  IF (PRESENT(switch)) THEN
    ! search for the CLA corresponding to switch
    found = .FALSE.
    DO a = 1, self%clasg(g)%Na
      IF (.NOT. self%clasg(g)%cla(a)%is_positional) THEN
        IF ((self%clasg(g)%cla(a)%switch == switch) &
             & .OR. (self%clasg(g)%cla(a)%switch_ab == switch)) THEN
          found = .TRUE.
          EXIT
        END IF
      END IF
    END DO
    IF (.NOT. found) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_CLA, switch=switch)
    ELSE
      CALL self%clasg(g)%cla(a)%get_varying(pref=pref, val=val)
      self%error = self%clasg(g)%cla(a)%error
    END IF
  ELSEIF (PRESENT(position)) THEN
    CALL self%clasg(g)%cla(position)%get_varying(pref=pref, val=val)
    self%error = error
  ELSE
    CALL self%errored(pref=pref, error=ERROR_MISSING_SELECTION_CLA)
  END IF
  ! check if the only error found is for unknown passed CLAs and if it is ignored by the user
  IF (self%error == ERROR_UNKNOWN &
       & .AND. self%error_unknown_clas == ERROR_UNKNOWN_CLAS_IGNORED) &
       & self%error = ERROR_UNKNOWN_CLAS_IGNORED
  IF (PRESENT(error)) error = self%error
END SUBROUTINE get_cla_list_varying_I8P

! Get CLA multiple values from CLAs list parsed with varying size
! list, integer(I4P).
!
! @note The CLA list is returned deallocated if values are
! not correctly gotten.
!
! @note For logical type CLA the value is directly read without
! any robust error trapping.

SUBROUTINE get_cla_list_varying_I4P(self, val, pref, args, group, &
     & switch, position, error)
  CLASS(CommandLineInterface_), INTENT(inout) :: self
  !! CLI data.
  INTEGER(I4P), ALLOCATABLE, INTENT(out) :: val(:)
  !! CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref
  !! Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: args
  !! String containing command line arguments.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group
  !! Name of group (command) of CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch
  !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position
  !! Position of positional CLA.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error
  !! Error trapping flag.
  LOGICAL :: found
  !! Flag for checking if CLA containing switch has been found.
  INTEGER(I4P) :: g
  !! Group counter.
  INTEGER(I4P) :: a
  !! Argument counter.
  !> main
  IF (.NOT. self%is_parsed_) THEN
    CALL self%parse(pref=pref, args=args, error=error)
    IF (self%error > 0 .AND. &
         & self%error_unknown_clas /= ERROR_UNKNOWN_CLAS_IGNORED) RETURN
  END IF
  IF (PRESENT(group)) THEN
    IF (.NOT. self%is_defined_group(group=group, g=g)) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_GROUP, group=group)
    END IF
  ELSE
    g = 0
  END IF
  IF (PRESENT(switch)) THEN
    ! search for the CLA corresponding to switch
    found = .FALSE.
    DO a = 1, self%clasg(g)%Na
      IF (.NOT. self%clasg(g)%cla(a)%is_positional) THEN
        IF ((self%clasg(g)%cla(a)%switch == switch) &
             & .OR. (self%clasg(g)%cla(a)%switch_ab == switch)) THEN
          found = .TRUE.
          EXIT
        END IF
      END IF
    END DO
    IF (.NOT. found) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_CLA, switch=switch)
    ELSE
      CALL self%clasg(g)%cla(a)%get_varying(pref=pref, val=val)
      self%error = self%clasg(g)%cla(a)%error
    END IF
  ELSEIF (PRESENT(position)) THEN
    CALL self%clasg(g)%cla(position)%get_varying(pref=pref, val=val)
    self%error = error
  ELSE
    CALL self%errored(pref=pref, error=ERROR_MISSING_SELECTION_CLA)
  END IF
  ! check if the only error found is for unknown passed CLAs and if it is ignored by the user
  IF (self%error == ERROR_UNKNOWN .AND. &
       & self%error_unknown_clas == ERROR_UNKNOWN_CLAS_IGNORED) &
       & self%error = ERROR_UNKNOWN_CLAS_IGNORED
  IF (PRESENT(error)) error = self%error
END SUBROUTINE get_cla_list_varying_I4P

  !! Get CLA multiple values from CLAs list parsed with varying size list, integer(I2P).
  !!
  !! @note The CLA list is returned deallocated if values are not correctly gotten.
  !!
  !! @note For logical type CLA the value is directly read without any robust error trapping.

SUBROUTINE get_cla_list_varying_I2P(self, val, pref, args, group, &
     & switch, position, error)
  CLASS(CommandLineInterface_), INTENT(inout) :: self     !! CLI data.
  INTEGER(I2P), ALLOCATABLE, INTENT(out) :: val(:)   !! CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !! Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: args     !! String containing command line arguments.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group    !! Name of group (command) of CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch   !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position !! Position of positional CLA.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error    !! Error trapping flag.
  LOGICAL :: found    !! Flag for checking if CLA containing switch has been found.
  INTEGER(I4P) :: g        !! Group counter.
  INTEGER(I4P) :: a        !! Argument counter.

  IF (.NOT. self%is_parsed_) THEN
    CALL self%parse(pref=pref, args=args, error=error)
    IF (self%error>0.AND.self%error_unknown_clas/=ERROR_UNKNOWN_CLAS_IGNORED) RETURN
  END IF
  IF (PRESENT(group)) THEN
    IF (.NOT. self%is_defined_group(group=group, g=g)) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_GROUP, group=group)
    END IF
  ELSE
    g = 0
  END IF
  IF (PRESENT(switch)) THEN
    ! search for the CLA corresponding to switch
    found = .FALSE.
    DO a = 1, self%clasg(g)%Na
      IF (.NOT. self%clasg(g)%cla(a)%is_positional) THEN
        IF ((self%clasg(g)%cla(a)%switch==switch).OR.(self%clasg(g)%cla(a)%switch_ab==switch)) THEN
          found = .TRUE.
          EXIT
        END IF
      END IF
    END DO
    IF (.NOT. found) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_CLA, switch=switch)
    ELSE
      CALL self%clasg(g)%cla(a)%get_varying(pref=pref, val=val); self%error = self%clasg(g)%cla(a)%error
    END IF
  ELSEIF (PRESENT(position)) THEN
    CALL self%clasg(g)%cla(position)%get_varying(pref=pref, val=val); self%error = error
  ELSE
    CALL self%errored(pref=pref, error=ERROR_MISSING_SELECTION_CLA)
  END IF
  ! check if the only error found is for unknown passed CLAs and if it is ignored by the user
  IF (self%error==ERROR_UNKNOWN.AND.self%error_unknown_clas==ERROR_UNKNOWN_CLAS_IGNORED) self%error = ERROR_UNKNOWN_CLAS_IGNORED
  IF (PRESENT(error)) error = self%error
END SUBROUTINE get_cla_list_varying_I2P

  SUBROUTINE get_cla_list_varying_I1P(self, val, pref, args, group, switch, position, error)
  !! Get CLA multiple values from CLAs list parsed with varying size list, integer(I1P).
  !!
  !! @note The CLA list is returned deallocated if values are not correctly gotten.
  !!
  !! @note For logical type CLA the value is directly read without any robust error trapping.
  CLASS(CommandLineInterface_), INTENT(inout) :: self     !! CLI data.
  INTEGER(I1P), ALLOCATABLE, INTENT(out) :: val(:)   !! CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !! Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: args     !! String containing command line arguments.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group    !! Name of group (command) of CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch   !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position !! Position of positional CLA.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error    !! Error trapping flag.
  LOGICAL :: found    !! Flag for checking if CLA containing switch has been found.
  INTEGER(I4P) :: g        !! Group counter.
  INTEGER(I4P) :: a        !! Argument counter.

  IF (.NOT. self%is_parsed_) THEN
    CALL self%parse(pref=pref, args=args, error=error)
    IF (self%error>0.AND.self%error_unknown_clas/=ERROR_UNKNOWN_CLAS_IGNORED) RETURN
  END IF
  IF (PRESENT(group)) THEN
    IF (.NOT. self%is_defined_group(group=group, g=g)) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_GROUP, group=group)
    END IF
  ELSE
    g = 0
  END IF
  IF (PRESENT(switch)) THEN
    ! search for the CLA corresponding to switch
    found = .FALSE.
    DO a = 1, self%clasg(g)%Na
      IF (.NOT. self%clasg(g)%cla(a)%is_positional) THEN
        IF ((self%clasg(g)%cla(a)%switch==switch).OR.(self%clasg(g)%cla(a)%switch_ab==switch)) THEN
          found = .TRUE.
          EXIT
        END IF
      END IF
    END DO
    IF (.NOT. found) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_CLA, switch=switch)
    ELSE
      CALL self%clasg(g)%cla(a)%get_varying(pref=pref, val=val); self%error = self%clasg(g)%cla(a)%error
    END IF
  ELSEIF (PRESENT(position)) THEN
    CALL self%clasg(g)%cla(position)%get_varying(pref=pref, val=val); self%error = error
  ELSE
    CALL self%errored(pref=pref, error=ERROR_MISSING_SELECTION_CLA)
  END IF
  ! check if the only error found is for unknown passed CLAs and if it is ignored by the user
  IF (self%error==ERROR_UNKNOWN.AND.self%error_unknown_clas==ERROR_UNKNOWN_CLAS_IGNORED) self%error = ERROR_UNKNOWN_CLAS_IGNORED
  IF (PRESENT(error)) error = self%error
END SUBROUTINE get_cla_list_varying_I1P

  SUBROUTINE get_cla_list_varying_logical(self, val, pref, args, group, switch, position, error)
  !! Get CLA multiple values from CLAs list parsed with varying size list, logical.
  !!
  !! @note The CLA list is returned deallocated if values are not correctly gotten.
  !!
  !! @note For logical type CLA the value is directly read without any robust error trapping.
  CLASS(CommandLineInterface_), INTENT(inout) :: self     !! CLI data.
  LOGICAL, ALLOCATABLE, INTENT(out) :: val(:)   !! CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !! Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: args     !! String containing command line arguments.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group    !! Name of group (command) of CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch   !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position !! Position of positional CLA.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error    !! Error trapping flag.
  LOGICAL :: found    !! Flag for checking if CLA containing switch has been found.
  INTEGER(I4P) :: g        !! Group counter.
  INTEGER(I4P) :: a        !! Argument counter.

  IF (.NOT. self%is_parsed_) THEN
    CALL self%parse(pref=pref, args=args, error=error)
    IF (self%error>0.AND.self%error_unknown_clas/=ERROR_UNKNOWN_CLAS_IGNORED) RETURN
  END IF
  IF (PRESENT(group)) THEN
    IF (.NOT. self%is_defined_group(group=group, g=g)) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_GROUP, group=group)
    END IF
  ELSE
    g = 0
  END IF
  IF (PRESENT(switch)) THEN
    ! search for the CLA corresponding to switch
    found = .FALSE.
    DO a = 1, self%clasg(g)%Na
      IF (.NOT. self%clasg(g)%cla(a)%is_positional) THEN
        IF ((self%clasg(g)%cla(a)%switch==switch).OR.(self%clasg(g)%cla(a)%switch_ab==switch)) THEN
          found = .TRUE.
          EXIT
        END IF
      END IF
    END DO
    IF (.NOT. found) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_CLA, switch=switch)
    ELSE
      CALL self%clasg(g)%cla(a)%get_varying(pref=pref, val=val); self%error = self%clasg(g)%cla(a)%error
    END IF
  ELSEIF (PRESENT(position)) THEN
    CALL self%clasg(g)%cla(position)%get_varying(pref=pref, val=val); self%error = error
  ELSE
    CALL self%errored(pref=pref, error=ERROR_MISSING_SELECTION_CLA)
  END IF
  ! check if the only error found is for unknown passed CLAs and if it is ignored by the user
  IF (self%error==ERROR_UNKNOWN.AND.self%error_unknown_clas==ERROR_UNKNOWN_CLAS_IGNORED) self%error = ERROR_UNKNOWN_CLAS_IGNORED
  IF (PRESENT(error)) error = self%error
END SUBROUTINE get_cla_list_varying_logical

  SUBROUTINE get_cla_list_varying_char(self, val, pref, args, group, switch, position, error)
  !! Get CLA multiple values from CLAs list parsed with varying size list, character.
  !!
  !! @note The CLA list is returned deallocated if values are not correctly gotten.
  !!
  !! @note For logical type CLA the value is directly read without any robust error trapping.
  CLASS(CommandLineInterface_), INTENT(inout) :: self     !! CLI data.
  CHARACTER(*), ALLOCATABLE, INTENT(out) :: val(:)   !! CLA values.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref     !! Prefixing string.
  CHARACTER(*), OPTIONAL, INTENT(in) :: args     !! String containing command line arguments.
  CHARACTER(*), OPTIONAL, INTENT(in) :: group    !! Name of group (command) of CLA.
  CHARACTER(*), OPTIONAL, INTENT(in) :: switch   !! Switch name.
  INTEGER(I4P), OPTIONAL, INTENT(in) :: position !! Position of positional CLA.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error    !! Error trapping flag.
  LOGICAL :: found    !! Flag for checking if CLA containing switch has been found.
  INTEGER(I4P) :: g        !! Group counter.
  INTEGER(I4P) :: a        !! Argument counter.

  IF (.NOT. self%is_parsed_) THEN
    CALL self%parse(pref=pref, args=args, error=error)
    IF (self%error>0.AND.self%error_unknown_clas/=ERROR_UNKNOWN_CLAS_IGNORED) RETURN
  END IF
  IF (PRESENT(group)) THEN
    IF (.NOT. self%is_defined_group(group=group, g=g)) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_GROUP, group=group)
    END IF
  ELSE
    g = 0
  END IF
  IF (PRESENT(switch)) THEN
    ! search for the CLA corresponding to switch
    found = .FALSE.
    DO a = 1, self%clasg(g)%Na
      IF (.NOT. self%clasg(g)%cla(a)%is_positional) THEN
        IF ((self%clasg(g)%cla(a)%switch==switch).OR.(self%clasg(g)%cla(a)%switch_ab==switch)) THEN
          found = .TRUE.
          EXIT
        END IF
      END IF
    END DO
    IF (.NOT. found) THEN
      CALL self%errored(pref=pref, error=ERROR_MISSING_CLA, switch=switch)
    ELSE
      CALL self%clasg(g)%cla(a)%get_varying(pref=pref, val=val); self%error = self%clasg(g)%cla(a)%error
    END IF
  ELSEIF (PRESENT(position)) THEN
    CALL self%clasg(g)%cla(position)%get_varying(pref=pref, val=val); self%error = error
  ELSE
    CALL self%errored(pref=pref, error=ERROR_MISSING_SELECTION_CLA)
  END IF
  ! check if the only error found is for unknown passed CLAs and if it is ignored by the user
  IF (self%error==ERROR_UNKNOWN.AND.self%error_unknown_clas==ERROR_UNKNOWN_CLAS_IGNORED) self%error = ERROR_UNKNOWN_CLAS_IGNORED
  IF (PRESENT(error)) error = self%error
END SUBROUTINE get_cla_list_varying_char

  FUNCTION usage(self, g, pref, no_header, no_examples, no_epilog, markdown) RESULT(usaged)
  !! Print correct usage of CLI.
  CLASS(CommandLineInterface_), INTENT(in) :: self             !! CLI data.
  INTEGER(I4P), INTENT(in) :: g                !! Group index.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref             !! Prefixing string.
  LOGICAL, OPTIONAL, INTENT(in) :: no_header        !! Avoid insert header to usage.
  LOGICAL, OPTIONAL, INTENT(in) :: no_examples      !! Avoid insert examples to usage.
  LOGICAL, OPTIONAL, INTENT(in) :: no_epilog        !! Avoid insert epilogue to usage.
  LOGICAL, OPTIONAL, INTENT(in) :: markdown         !! Format things with markdown
  CHARACTER(len=:), ALLOCATABLE :: prefd            !! Prefixing string.
  CHARACTER(len=:), ALLOCATABLE :: usaged           !! Usage string.
  LOGICAL :: no_headerd       !! Avoid insert header to usage.
  LOGICAL :: no_examplesd     !! Avoid insert examples to usage.
  LOGICAL :: no_epilogd       !! Avoid insert epilogue to usage.
  LOGICAL :: markdownd        !! Format for markdown.
  LOGICAL :: grouped_examples !! Will show examples of group usage.
  INTEGER(I4P) :: gi               !! Counter.

  no_headerd = .FALSE.; IF (PRESENT(no_header)) no_headerd = no_header
  no_examplesd = .FALSE.; IF (PRESENT(no_examples)) no_examplesd = no_examples
  no_epilogd = .FALSE.; IF (PRESENT(no_epilog)) no_epilogd = no_epilog
  markdownd = .FALSE.; IF (PRESENT(markdown)) markdownd = markdown
  prefd = ''; IF (PRESENT(pref)) prefd = pref
  grouped_examples = .FALSE.
  IF (g > 0) THEN ! usage of a specific command
    usaged = self%clasg(g)%usage(pref=prefd,no_header=no_headerd,markdown=markdownd)
    IF (ALLOCATED(self%clasg(g)%examples) .AND. (.NOT. no_examplesd)) THEN
      usaged = usaged//print_examples(prefd, self%clasg(g)%examples)
      grouped_examples = .TRUE.
    END IF
  ELSE ! usage of whole CLI
    IF (no_headerd) THEN
      usaged = ''
    ELSE
      usaged = prefd//self%help//self%progname//' '//self%signature()
      IF (self%description/='') usaged = usaged//new_LINE('a')//new_LINE('a')//prefd//self%description
    END IF
    IF (self%clasg(0)%Na>0) usaged = usaged//new_LINE('a')//self%clasg(0)%usage(pref=prefd,no_header=.TRUE.,markdown=markdownd)
    IF (SIZE(self%clasg, dim=1) > 1) THEN
      usaged = usaged//new_LINE('a')//new_LINE('a')//prefd//'Commands:'
      DO gi = 1, SIZE(self%clasg, dim=1) - 1
        usaged = usaged//new_LINE('a')//prefd//'  '//self%clasg(gi)%group
        usaged = usaged//new_LINE('a')//prefd//REPEAT(' ',10)//self%clasg(gi)%description
      END DO
      usaged = usaged//new_LINE('a')//new_LINE('a')//prefd//'For more detailed commands help try:'
      DO gi = 1, SIZE(self%clasg, dim=1) - 1
        usaged = usaged//new_LINE('a')//prefd//'  '//self%progname//' '//self%clasg(gi)%group//' -h,--help'
      END DO
    END IF
  END IF
  IF (ALLOCATED(self%examples).AND.(.NOT.no_examplesd).AND.(.NOT.grouped_examples)) THEN
    usaged = usaged//print_examples(prefd, self%examples)
  END IF
  IF (self%epilog/=''.AND.(.NOT.no_epilogd)) usaged = usaged//new_LINE('a')//prefd//self%epilog

CONTAINS
  FUNCTION print_examples(prefd, examples) RESULT(exampled)
    !! Print examples of the correct usage.
    CHARACTER(*), INTENT(in) :: prefd          !! Prefixing string.
    CHARACTER(*), INTENT(in) :: examples(1:)   !! Examples to be printed.
    CHARACTER(len=:), ALLOCATABLE :: exampled       !! Examples string.
    INTEGER(I4P) :: e              !! Counter.

    exampled = new_LINE('a')//new_LINE('a')//prefd//'Examples:'
    DO e = 1, SIZE(examples, dim=1)
      exampled = exampled//new_LINE('a')//prefd//'   '//TRIM(examples(e))
    END DO
  END FUNCTION print_examples
END FUNCTION usage

FUNCTION signature(self, bash_completion)
  !! Get signature.
  CLASS(CommandLineInterface_), INTENT(in) :: self             !! CLI data.
  LOGICAL, OPTIONAL, INTENT(in) :: bash_completion  !! Return the signature for bash completion.
  LOGICAL :: bash_completion_ !! Return the signature for bash completion, local variable.
  CHARACTER(len=:), ALLOCATABLE :: signature        !< Signature.
  INTEGER(I4P) :: g                !< Counter.

  bash_completion_ = .FALSE.; IF (PRESENT(bash_completion)) bash_completion_ = bash_completion

  IF (bash_completion_) THEN
    signature = signature//new_LINE('a')//'    COMPREPLY=( )'
  signature = signature//new_LINE('a')//'    COMPREPLY+=( $( compgen -W "'// &
                self%clasg(0)%signature(bash_completion=bash_completion, plain=.TRUE.)//'" -- $cur ) )'
    IF (SIZE(self%clasg, dim=1) > 1) THEN
      DO g = 1, SIZE(self%clasg, dim=1) - 1
        signature = signature//new_LINE('a')//'    COMPREPLY+=( $( compgen -W "'//self%clasg(g)%group//'" -- $cur ) )'
      END DO
    END IF
  ELSE
    signature = self%clasg(0)%signature()
    IF (SIZE(self%clasg, dim=1) > 1) THEN
      signature = signature//' {'//self%clasg(1)%group
      DO g = 2, SIZE(self%clasg, dim=1) - 1
        signature = signature//','//self%clasg(g)%group
      END DO
      signature = signature//'} ...'
    END IF
  END IF
END FUNCTION signature

SUBROUTINE print_usage(self, pref)
  !< Print correct usage.
  CLASS(CommandLineInterface_), INTENT(in) :: self  !< CLI data.
  CHARACTER(*), OPTIONAL, INTENT(in) :: pref  !< Prefixing string.

  WRITE (self%usage_lun, '(A)') self%usage(pref=pref, g=0)
END SUBROUTINE print_usage

SUBROUTINE save_bash_completion(self, bash_file, error)
  !< Save bash completion script (for named CLAs only).
  CLASS(CommandLineInterface_), INTENT(in) :: self      !< CLI data.
  CHARACTER(*), INTENT(in) :: bash_file !< Output file name of bash completion script.
  INTEGER(I4P), OPTIONAL, INTENT(out) :: error     !< Error trapping flag.
  CHARACTER(len=:), ALLOCATABLE :: script    !< Script text.
  INTEGER(I4P) :: g         !< CLAs groups counter.
  INTEGER(I4P) :: u         !< Unit file handler.

  script = '#/usr/bin/env bash'
  IF (SIZE(self%clasg, dim=1) > 1) THEN
    script = script//new_LINE('a')//'_completion()'
    script = script//new_LINE('a')//'{'
    script = script//new_LINE('a')//'  cur=${COMP_WORDS[COMP_CWORD]}'
    script = script//new_LINE('a')//'  prev=${COMP_WORDS[COMP_CWORD - 1]}'
    ! script = script//new_line('a')//'  if [[ $prev == "--help" || $prev == "-h" || $prev == "--version" || $prev == "-v" ]] ; then'
    ! script = script//new_line('a')//'    COMPREPLY=()'
    ! script = script//new_line('a')//'  else'
    script = script//new_LINE('a')//'  groups=('
    DO g = 1, SIZE(self%clasg, dim=1) - 1
      script = script//' "'//self%clasg(g)%group//'"'
    END DO
    script = script//' )'
    ! script = script//new_line('a')//'    base_clas=('//&
    !          self%clasg(0)%signature(bash_completion=.true., plain=.true.)//' )'
    ! do g=1,size(self%clasg,dim=1)-1
    !   script = script//new_line('a')//'    '//self%clasg(g)%group//'_clas=('//&
    !            self%clasg(g)%signature(bash_completion=.true., plain=.true.)//' )'
    ! enddo
    script = script//new_LINE('a')//'  for g in ${groups[@]}; do'
    script = script//new_LINE('a')//'    if [ "$prev" == "$g" ] ; then'
    script = script//new_LINE('a')//'      group=$prev '
    script = script//new_LINE('a')//'    fi'
    script = script//new_LINE('a')//'  done'
    ! script = script//new_line('a')//'  fi'
    script = script//new_LINE('a')//'  if [ "$group" == "'//self%clasg(1)%group//'" ] ; then'
    script = script//self%clasg(1)%signature(bash_completion=.TRUE.)
    DO g = 2, SIZE(self%clasg, dim=1) - 1
      script = script//new_LINE('a')//'  elif [ "$group" == "'//self%clasg(g)%group//'" ] ; then'
      script = script//self%clasg(g)%signature(bash_completion=.TRUE.)
    END DO
    script = script//new_LINE('a')//'  else'
    script = script//'    '//self%signature(bash_completion=.TRUE.)
    script = script//new_LINE('a')//'  fi'
    script = script//new_LINE('a')//'  return 0'
    script = script//new_LINE('a')//'}'
      script = script//new_LINE('a')//'complete -F _completion '//basename(self%progname)
  ELSE
    script = script//new_LINE('a')//'complete -W "'//self%signature(bash_completion=.TRUE.)//'" '//basename(self%progname)
  END IF
  OPEN (newunit=u, file=TRIM(ADJUSTL(bash_file)))
  IF (PRESENT(error)) THEN
    WRITE (u, "(A)", iostat=error) script
  ELSE
    WRITE (u, "(A)") script
  END IF
  CLOSE (u)
CONTAINS
  PURE FUNCTION basename(progname)
    CHARACTER(len=*), INTENT(in) :: progname !< Program name.
    CHARACTER(len=:), ALLOCATABLE :: basename !< Program name without full PATH.
    INTEGER(I4P) :: pos      !< Counter.

    basename = progname
    pos = INDEX(basename, '/', back=.TRUE.)
    IF (pos > 0) THEN
      basename = basename(pos + 1:)
    ELSE
      pos = INDEX(basename, '\', back=.true.)
      if (pos > 0) basename = basename(pos + 1:)
    end if
  end function basename
end subroutine save_bash_completion

subroutine save_man_page(self, man_file, error)
  !< Save CLI usage as man page.
  class(CommandLineInterface_), intent(in) :: self               !< CLI data.
  character(*), intent(in) :: man_file           !< Output file name for saving man page.
  integer(I4P), optional, intent(out) :: error              !< Error trapping flag.
  character(len=:), allocatable :: man                !< Man page.
  integer(I4P) :: idate(1:8)         !< Integer array for handling the date.
  integer(I4P) :: e                  !< Counter.
  integer(I4P) :: u                  !< Unit file handler.
  character(*), parameter :: month(12) = ["Jan", &
                                          "Feb", &
                                          "Mar", &
                                          "Apr", &
                                          "May", &
                                          "Jun", &
                                          "Jul", &
                                          "Aug", &
                                          "Sep", &
                                          "Oct", &
                                          "Nov", &
                                          "Dec"]  !< Months list.

  call date_and_time(values=idate)
  man = '.TH '//self%progname//' "1" "'//month(idate(2))//' '//trim(adjustl(strz(idate(1),4)))//'" "version '//self%version//&
        '" "'//self%progname//' Manual"'
  man = man//new_line('a')//'.SH NAME'
  man = man//new_line('a')//self%progname//' - manual page for '//self%progname//' version '//self%version
  man = man//new_line('a')//'.SH SYNOPSIS'
  man = man//new_line('a')//'.B '//self%progname//new_line('a')//trim(adjustl(self%signature()))
  if (self%description /= '') man = man//new_line('a')//'.SH DESCRIPTION'//new_line('a')//self%description
  if (self%clasg(0)%Na > 0) then
    man = man//new_line('a')//'.SH OPTIONS'
    man = man//new_line('a')//self%usage(no_header=.true.,no_examples=.true.,no_epilog=.true.,g=0)
  end if
  if (allocated(self%examples)) then
    man = man//new_line('a')//'.SH EXAMPLES'
    man = man//new_line('a')//'.PP'
    man = man//new_line('a')//'.nf'
    man = man//new_line('a')//'.RS'
    do e = 1, size(self%examples, dim=1)
      man = man//new_line('a')//trim(self%examples(e))
    end do
    man = man//new_line('a')//'.RE'
    man = man//new_line('a')//'.fi'
    man = man//new_line('a')//'.PP'
  end if
  if (self%authors /= '') man = man//new_line('a')//'.SH AUTHOR'//new_line('a')//self%authors
  if (self%license /= '') man = man//new_line('a')//'.SH COPYRIGHT'//new_line('a')//self%license
  open (newunit=u, file=trim(adjustl(man_file)))
  if (present(error)) then
    write (u, "(A)", iostat=error) man
  else
    write (u, "(A)") man
  end if
  close (u)
end subroutine save_man_page

subroutine save_usage_to_markdown(self, markdown_file, error)
  !< Save CLI usage as markdown.
  class(CommandLineInterface_), intent(in) :: self               !< CLI data.
  character(*), intent(in) :: markdown_file      !< Output file name for saving man page.
  integer(I4P), optional, intent(out) :: error              !< Error trapping flag.
  character(len=:), allocatable :: man                !< Man page.
  integer(I4P) :: idate(1:8)         !< Integer array for handling the date.
  integer(I4P) :: e                  !< Counter.
  integer(I4P) :: u                  !< Unit file handler.
  character(*), parameter :: month(12) = ["Jan", &
                                          "Feb", &
                                          "Mar", &
                                          "Apr", &
                                          "May", &
                                          "Jun", &
                                          "Jul", &
                                          "Aug", &
                                          "Sep", &
                                          "Oct", &
                                          "Nov", &
                                          "Dec"]  !< Months list.

  call date_and_time(values=idate)
  man = '# '//self%progname//new_line('a')
  man = man//new_line('a')//'Manual page for `'//self%progname//'` version '//self%version//new_line('a')
  man = man//new_line('a')//'`'//self%progname//' '//trim(adjustl(self%signature()))//'`'//new_line('a')
  man = man//new_line('a')//month(idate(2))//' '//trim(adjustl(strz(idate(1),4)))//new_line('a')
  if (self%description /= '') man = man//new_line('a')//'### Short description'//new_line('a')//new_line('a')//self%description
  if (self%clasg(0)%Na > 0) then
    man = man//new_line('a')//new_line('a')//'### Command line options:'
    man = man//self%usage(no_header=.true.,no_examples=.true.,no_epilog=.true.,g=0,markdown=.true.)
  end if
  if (allocated(self%examples)) then
    man = man//new_line('a')//new_line('a')//'### Examples'
    do e = 1, size(self%examples, dim=1)
      man = man//new_line('a')
      man = man//new_line('a')//'`'//trim(self%examples(e))//'` '
    end do
  end if
  open (newunit=u, file=trim(adjustl(markdown_file)))
  if (present(error)) then
    write (u, "(A)", iostat=error) man
  else
    write (u, "(A)") man
  end if
  close (u)
end subroutine save_usage_to_markdown

! private methods
subroutine errored(self, error, pref, group, switch)
  !< Trig error occurrence and print meaningful message.
  class(CommandLineInterface_), intent(inout) :: self   !< Object data.
  integer(I4P), intent(in) :: error  !< Error occurred.
  character(*), optional, intent(in) :: pref   !< Prefixing string.
  character(*), optional, intent(in) :: group  !< Group name.
  character(*), optional, intent(in) :: switch !< CLA switch name.
  character(len=:), allocatable :: prefd  !< Prefixing string.

  self%error = error
  if (self%error /= 0) then
    prefd = ''; if (present(pref)) prefd = pref
    prefd = prefd//self%progname//': '//colorize('error', color_fg=self%error_color, style=self%error_style)
    select case (self%error)
    case (ERROR_MISSING_CLA)
      self%error_message = prefd//': there is no option "'//trim(adjustl(switch))//'"!'
    CASE (ERROR_MISSING_SELECTION_CLA)
      self%error_message = prefd//': to get an option value one of switch "name" or "position" must be provided!'
    CASE (ERROR_MISSING_GROUP)
      self%error_message = prefd//': ther is no group (command) named "'//TRIM(ADJUSTL(group))//'"!'
    CASE (ERROR_TOO_FEW_CLAS)
      ! self%error_message = prefd//': too few arguments ('//trim(str(.true.,Na))//')'//&
      ! ' respect the required ('//trim(str(.true.,self%Na_required))//')'
    END SELECT
    WRITE (self%error_lun, '(A)')
    CALL self%print_error_message
  END IF
END SUBROUTINE errored

ELEMENTAL SUBROUTINE cli_assign_cli(lhs, rhs)
  !< Assignment operator.
  CLASS(CommandLineInterface_), INTENT(inout) :: lhs !< Left hand side.
  TYPE(CommandLineInterface_), INTENT(in) :: rhs !< Right hand side.

  ! object members
  CALL lhs%assign_object(rhs)
  ! CommandLineInterface_ members
  IF (ALLOCATED(rhs%clasg)) lhs%clasg = rhs%clasg
  IF (ALLOCATED(rhs%examples)) lhs%examples = rhs%examples
  lhs%disable_hv = rhs%disable_hv
END SUBROUTINE cli_assign_cli

ELEMENTAL SUBROUTINE finalize(self)
  !< Free dynamic memory when finalizing.
  TYPE(CommandLineInterface_), INTENT(inout) :: self !< CLI data.
  CALL self%Deallocate()
END SUBROUTINE finalize
ENDMODULE CommandLineInterface_Class
