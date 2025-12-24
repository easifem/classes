! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

MODULE AbstractMeshParam
USE GlobalData, ONLY: I4B
PRIVATE

#ifdef MAX_NODE_TO_NODE
INTEGER(I4B), PUBLIC, PARAMETER :: PARAM_MAX_NODE_TO_NODE = MAX_NODE_TO_NODE
#else
INTEGER(I4B), PUBLIC, PARAMETER :: PARAM_MAX_NODE_TO_NODE = 256
#endif

#ifdef MAX_NODE_TO_ELEM
INTEGER(I4B), PUBLIC, PARAMETER :: PARAM_MAX_NODE_TO_ELEM = MAX_NODE_TO_ELEM
#else
INTEGER(I4B), PUBLIC, PARAMETER :: PARAM_MAX_NODE_TO_ELEM = 128
#endif

#ifdef MAX_CONNECTIVITY_SIZE
INTEGER(I4B), PUBLIC, PARAMETER :: PARAM_MAX_CONNECTIVITY_SIZE = MAX_CONNECTIVITY_SIZE
#else
INTEGER(I4B), PUBLIC, PARAMETER :: PARAM_MAX_CONNECTIVITY_SIZE = 256
#endif

#ifdef MAX_NNE
INTEGER(I4B), PUBLIC, PARAMETER :: PARAM_MAX_NNE = MAX_NNE
#else
INTEGER(I4B), PUBLIC, PARAMETER :: PARAM_MAX_NNE = 128
#endif

END MODULE AbstractMeshParam
