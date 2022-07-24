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

MODULE PLPlot_Class
USE GlobalData
USE BaseType
USE BaseMethod
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE ParameterList, ONLY: ParameterList_
USE AbstractPlot_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="PLPlot_Class"
TYPE( ExceptionHandler_ ) :: e

!----------------------------------------------------------------------------
!                                                                 PLPlot_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 11 July 2022
! summary: Interface to PLPLOT library.

TYPE, EXTENDS( AbstractPlot_ ) :: PLPlot_
  CONTAINS
  PRIVATE
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => Plot_Initiate
  PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => Plot_Deallocate
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => Plot_Display
  !!
  !! @LinePlotMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: line_plot_x1y1
  PROCEDURE, PUBLIC, PASS( obj ) :: line_plot_x1y2
  GENERIC, PUBLIC :: LinePlot => &
    & line_plot_x1y1, &
    & line_plot_x1y2

END TYPE PLPlot_

PUBLIC :: PLPlot_

!----------------------------------------------------------------------------
!                                                            PLPlotPointer
!----------------------------------------------------------------------------

TYPE :: PLPlotPointer_
  CLASS( PLPlot_ ), POINTER :: ptr => NULL()
END TYPE PLPlotPointer_

PUBLIC :: PLPlotPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 July 2022
! summary: Initiate the plotting engine
!
!# Introduction
!
! This routine initiate the kernel.

INTERFACE
MODULE SUBROUTINE Plot_Initiate( obj, param )
  CLASS( PLPlot_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), OPTIONAL, INTENT( IN ) :: param
END SUBROUTINE Plot_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 July 2022
! summary: Deallocate the data stored inside the plot

INTERFACE
MODULE SUBROUTINE Plot_Deallocate( obj )
  CLASS( PLPlot_ ), INTENT( INOUT ) :: obj
END SUBROUTINE Plot_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 July 2022
! summary: Display the content of Plot

INTERFACE
MODULE SUBROUTINE Plot_Display( obj, msg, unitno )
  CLASS( PLPlot_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE Plot_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                LinePlot@LinePlotMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE line_plot_x1y1( obj, x, y, filename, &
  & xlabel, ylabel, title, lineWidth, xmin, ymin, xmax, ymax, &
  & isPoint, pointSize, pointType )
  CLASS( PLPlot_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), INTENT( IN ) :: y( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: xlabel
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: ylabel
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: title
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: lineWidth
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xmin, xmax
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: ymin, ymax
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isPoint
    !! If true we print points also
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: pointSize
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: pointType
END SUBROUTINE line_plot_x1y1
END INTERFACE

!----------------------------------------------------------------------------
!                                                LinePlot@LinePlotMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE line_plot_x1y2( obj, x, y, filename, &
  & xlabel, ylabel, legendTexts, title, lineWidth, xmin, ymin, xmax, ymax, &
  & isPoint, pointSize, pointType )
  CLASS( PLPlot_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
  REAL( DFP ), INTENT( IN ) :: y( :, : )
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: xlabel
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: ylabel
  TYPE(String), OPTIONAL, INTENT( IN ) :: legendTexts( : )
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: title
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: lineWidth
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xmin, xmax
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: ymin, ymax
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isPoint
    !! If true we print points also
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: pointSize
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: pointType
END SUBROUTINE line_plot_x1y2
END INTERFACE

END MODULE PLPlot_Class