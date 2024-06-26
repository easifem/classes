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

#define FTL_TEMPLATE_TYPE FaceData_
#define FTL_TEMPLATE_TYPE_IS_DERIVED
#define FTL_TEMPLATE_TYPE_IS_CLASS
#define FTL_TEMPLATE_TYPE_NAME FaceData
#define FTL_INSTANTIATE_TEMPLATE

MODULE FaceDataList_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE Display_Method
USE FaceData_Class
#include "../../ftlMacros/ftlList.inc"
END MODULE FaceDataList_Class
