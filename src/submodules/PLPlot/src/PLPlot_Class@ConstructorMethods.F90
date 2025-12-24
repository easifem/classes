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

#ifdef USE_PLPLOT
SUBMODULE(PLPlot_Class) ConstructorMethods
USE AbstractPlot_Class, ONLY: PLOT_ENGINE_PLPLOT
USE Display_Method, ONLY: Display
USE StringUtility, ONLY: GetExtension, LowerCase

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Initiate
obj%plotEngine = PLOT_ENGINE_PLPLOT
END PROCEDURE plot_Initiate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Deallocate
obj%plotEngine = PLOT_ENGINE_PLPLOT
END PROCEDURE plot_Deallocate

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Display
CALL Display("# PLOT ENGINE : PLPLOT", msg, unitno)
END PROCEDURE plot_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetDeviceName
CHARACTER(:), ALLOCATABLE :: extn

extn = TRIM(LowerCase(GetExtension(filename)))

#ifdef Darwin_SYSTEM

SELECT CASE (extn)
CASE ("pdf"); ans = "pdfcairo"
CASE ("png"); ans = "pngcairo"
CASE ("ps"); ans = "ps"
CASE ("eps"); ans = "epscairo"
CASE ("svg"); ans = "svg"
CASE ("jpeg", "jpg"); ans = "pngcairo"
END SELECT

#else

SELECT CASE (extn)
CASE ("pdf"); ans = "pdf"
CASE ("png"); ans = "pngqt"
CASE ("ps"); ans = "ps"
CASE ("eps"); ans = "epscairo"
CASE ("svg"); ans = "svg"
CASE ("jpeg", "jpg"); ans = "jpgqt"
END SELECT

#endif

extn = ""

END PROCEDURE GetDeviceName

END SUBMODULE ConstructorMethods
#endif
