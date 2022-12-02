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

MODULE AbstractPlot_Class
USE GlobalData
USE ExceptionHandler_Class, ONLY: e
USE ParameterList, ONLY: ParameterList_
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "AbstractPlot_Class"
INTEGER(I4B), PUBLIC, PARAMETER :: PLOT_ENGINE_PLPLOT = 1
INTEGER(I4B), PUBLIC, PARAMETER :: PLOT_ENGINE_GNUPLOT = 2
INTEGER(I4B), PUBLIC, PARAMETER :: PLOT_ENGINE_MATPLOTLIB = 3
INTEGER(I4B), PUBLIC, PARAMETER :: PLOT_ENGINE_VTK = 4
INTEGER(I4B), PUBLIC, PARAMETER :: PLOT_ENGINE_GMSH = 5
INTEGER(I4B), PUBLIC, PARAMETER :: PLOT_ENGINE_PYPLOT = 6
CHARACTER(LEN=*), PUBLIC, PARAMETER, DIMENSION(6) :: PLOT_ENGINE_NAME = &
  & [ &
  & "PLPLOT    ", &
  & "GNUPLOT   ", &
  & "MATPLOTLIB", &
  & "VTK       ", &
  & "GMSH      ", &
  & "PYPLOT    " &
  & ]

!----------------------------------------------------------------------------
!                                                              AbstractPlot_
!----------------------------------------------------------------------------

TYPE, ABSTRACT :: AbstractPlot_
  INTEGER(I4B) :: plotEngine = PLOT_ENGINE_VTK
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => plot_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => plot_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => plot_Display
END TYPE AbstractPlot_

PUBLIC :: AbstractPlot_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractPlotPointer_
  CLASS(AbstractPlot_), POINTER :: ptr => NULL()
END TYPE AbstractPlotPointer_

PUBLIC :: AbstractPlotPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 July 2022
! summary: Initiate the plotting engine

INTERFACE
  MODULE SUBROUTINE plot_Initiate(obj, param)
    CLASS(AbstractPlot_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), OPTIONAL, INTENT(IN) :: param
  END SUBROUTINE plot_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 July 2022
! summary: Deallocate the data stored inside the plot

INTERFACE
  MODULE SUBROUTINE plot_Deallocate(obj)
    CLASS(AbstractPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE plot_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 July 2022
! summary: Display the content of Plot

INTERFACE
  MODULE SUBROUTINE plot_Display(obj, msg, unitno)
    CLASS(AbstractPlot_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE plot_Display
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractPlot_Class
