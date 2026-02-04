! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

! > authors: Vikas Sharma, Ph. D.
! date: 2021-11-07
! summary: This code reads a markdown file and extracts the fortran code

PROGRAM main
USE CommandLineInterface_Class, ONLY: CommandLineInterface_
USE TxtFile_Class, ONLY: TxtFile_
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE String_Class, ONLY: String

#define PARAM_MAX_LEN 1024

IMPLICIT NONE
TYPE(TxtFile_) :: srcfile, mdfile
INTEGER(I4B), PARAMETER :: maxStrLen = 1024
CHARACTER(LEN=PARAM_MAX_LEN) :: mdfilename
CHARACTER(LEN=PARAM_MAX_LEN) :: srcfilename
CHARACTER(LEN=*), PARAMETER :: modName = "src2md"
CHARACTER(LEN=*), PARAMETER :: myName = "main"
CHARACTER(*), PARAMETER :: description = "Read source code and generate &
&markdown files for hugo site."

TYPE(CommandLineInterface_) :: cli
INTEGER(I4B) :: error
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

!> main
! initializing Command Line Interface
CALL cli%Initiate( &
  progname='md2src', &
  version='v25.10.0', &
  authors='Vikas Sharma, Ph.D.', &
  license='MIT', &
  description=description, &
  examples=[ &
  'md2src                                           ', &
  'md2src -h                                        ', &
  'md2src --input inputFile.md --output outFile.F90 ', &
  'md2src -i inputFile.md -o outFile.F90            ', &
  'md2src --version                                 ', &
  'md2src -v                                        '])

CALL cli%Add( &
  switch='--input', switch_ab='-i', help='name of input markdown file', &
  required=math%yes, act='store', error=error)

problem = error .NE. 0
IF (problem) &
  CALL e%RaiseError(modName//"::"//myName//" - "// &
                    'cannot add value of --input from CLI')

!> handling output
CALL cli%Add( &
  switch='--output', switch_ab='-o', help='name of output source file', &
  required=math%no, act='store', def='default', error=error)

problem = error .NE. 0
IF (problem) &
  CALL e%RaiseError(modName//"::"//myName//" - "// &
                    'cannot add value of --output from CLI')

CALL cli%Get(switch='-i', val=mdfilename, error=error)

IF (error .NE. 0) &
  CALL e%RaiseError(modName//"::"//myName//" - "// &
                    'cannot get value of --input from CLI')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
                        'Parsing markdown file : '//TRIM(mdfilename))
#endif

CALL mdfile%Initiate(filename=mdfilename, STATUS="OLD", ACTION="READ")

CALL mdfile%OPEN()

CALL cli%Get(switch='-o', val=srcfilename, error=error)

problem = error .NE. 0

IF (problem) &
  CALL e%RaiseError(modName//"::"//myName//" - "// &
                    'cannot get value of --output from CLI')

IF (TRIM(srcfilename) .EQ. 'default') THEN
  srcfilename = mdfile%GetFilePath()//mdfile%GetFileName()//".F90"
END IF

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//"::"//myName//" - "// &
                  'Results will be written to file : '//TRIM(srcfilename))
#endif

CALL srcfile%Initiate(filename=srcfilename, status="REPLACE", &
                      ACTION="WRITE")
CALL srcfile%OPEN()

CALL mdfile%ConvertMarkdownToSource(outfile=srcfile)

CALL mdfile%DEALLOCATE()
CALL srcfile%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROGRAM main

