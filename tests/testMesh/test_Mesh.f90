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

module test_m
use easifemBase
use easifemClasses
implicit none
contains

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! subroutine test0
!   type( mesh_ ) :: obj
!   integer( I4B ) :: ierr, iel
!   type( HDF5File_ ) :: meshfile

!   call display( "testing Generate MeshData")
!   call meshfile%initiate( filename="./mesh.h5", mode="READ" )
!   call meshfile%open()
!   call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
!   obj%refelem => ReferenceTriangle_Pointer( nsd=2 )
!   call obj%GenerateMeshData()
!   call meshfile%close()
!   call meshfile%deallocateData()
! end subroutine

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! subroutine test8
!   type( mesh_ ) :: obj
!   integer( I4B ) :: ierr, iel
!   type( HDF5File_ ) :: meshfile

!   call display( "testing Boundary Data")
!   call meshfile%initiate( filename="./mesh.h5", mode="READ" )
!   call meshfile%open()
!   call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
!   call obj%GenerateMeshData()
!   obj%refelem => ReferenceTriangle_Pointer( nsd=2 )

!   call obj%InitiateBoundaryData()

!   call display( obj%isBoundaryDataInitiated(), "isBoundaryDataInitiated=")

!   iel = obj%getLocalElemNumber( 207 )
!   call display( iel, "local element number = " )
!   call display( obj%getConnectivity(iel), "connectivity = ")
!   call display( obj%isBoundaryElement( iel ), "T=" )
!   call display( obj%getBoundaryElementData( iel ), "boundary data = " )

!   iel = obj%getLocalElemNumber( 65 )
!   call display( iel, "local element number = " )
!   call display( obj%getConnectivity(iel), "connectivity = ")
!   call display( obj%isBoundaryElement( iel ), "T=" )
!   call display( obj%getBoundaryElementData( iel ), "boundary data = " )

!   iel = obj%getLocalElemNumber( 151 )
!   call display( obj%isBoundaryElement( iel ), "F=" )


!   call meshfile%close()
!   call meshfile%deallocateData()
! end subroutine

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! subroutine test7
!   type( mesh_ ) :: obj
!   integer( I4B ) :: ierr
!   type( HDF5File_ ) :: meshfile

!   call display( "testing Initiate Element to Elements Mapping")
!   call meshfile%initiate( filename="./mesh.h5", mode="READ" )
!   call meshfile%open()
!   call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
!   call obj%GenerateMeshData()
!   obj%refelem => ReferenceTriangle_Pointer( nsd=2 )

!   call obj%initiateElementToElements()

!   call display( obj%isElementToElementsInitiated(), "isElementToElementsInitiated=")

!   call display( obj%getElementToElements( obj%getLocalElemNumber( 33 ) ), "elements around 33 = " )

!   call display( obj%getElementToElements( &
!     & obj%getLocalElemNumber( 33 ), onlyElements=.TRUE. ), "elements around 33 = " )

!   call meshfile%close()
!   call meshfile%deallocateData()
! end subroutine

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! subroutine test6
!   type( mesh_ ) :: obj
!   integer( I4B ) :: ierr
!   type( HDF5File_ ) :: meshfile

!   call display( "testing Initiate Node to Nodes Mapping")
!   call meshfile%initiate( filename="./mesh.h5", mode="READ" )
!   call meshfile%open()
!   call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
!   call obj%GenerateMeshData()

!   call obj%initiateNodeToNodes()
!   call display( obj%isNodeToNodesInitiated(), "isNodeToNodesInitiated=")
!   call display( obj%getNodeToNodes( globalNode=1, IncludeSelf=.TRUE. ), "Node 1 = " )
!   call display( obj%getNodeToNodes( globalNode=33, IncludeSelf=.FALSE. ), "Node 33 = " )
!   call meshfile%close()
!   call meshfile%deallocateData()
! end subroutine

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! subroutine test5
!   type( mesh_ ) :: obj
!   integer( I4B ) :: ierr
!   type( HDF5File_ ) :: meshfile

!   call display( "testing Initiate Node Element Mapping")
!   call meshfile%initiate( filename="./mesh.h5", mode="READ" )
!   call meshfile%open()
!   call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
!   call obj%GenerateMeshData()

!   call obj%initiateNodeToElements()
!   call display( obj%isNodeToElementsInitiated(), "isNodeToElementsInitiated=")
!   call display( obj%getGlobalElemNumber(obj%getNodeToElements(1)),"elem for node 1=" )
!   call display( obj%getGlobalElemNumber(obj%getNodeToElements(96)),"elem for node 96=" )
!   call meshfile%close()
!   call meshfile%deallocateData()
! end subroutine

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! subroutine test4
!   type( mesh_ ) :: obj
!   integer( I4B ) :: ierr
!   type( HDF5File_ ) :: meshfile

!   call display( "testing query methods of meshdata")
!   call meshfile%initiate( filename="./mesh.h5", mode="READ" )
!   call meshfile%open()
!   call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
!   call obj%GenerateMeshData()
!   call display( obj%isNodeToNodesInitiated(), "isNodeToNodesInitiated")

!   call display( obj%isNodeToElementsInitiated(), "isNodeToElementsInitiated=")

!   call display( obj%isElementToElementsInitiated(), "isElementToElementsInitiated=")

!   call display( obj%isConnectivityInitiated(), "isConnectivityInitiated=")

!   call display( obj%isBoundaryDataInitiated(), "isBoundaryDataInitiated=")

!   call display( obj%isInternalNptrsInitiated(), "isInternalNptrsInitiated=")

!   call display( obj%isBoundaryNptrsInitiated(), "isBoundaryNptrsInitiated=")

!   call display( obj%isLocalNptrsInitiated(), "isLocalNptrsInitiated=")

!   call display( obj%isInternalBoundaryDataInitiated(), "isInternalBoundaryDataInitiated=")

!   call meshfile%close()
!   call meshfile%deallocateData()
! end subroutine

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! subroutine test3
!   type( mesh_ ) :: obj
!   integer( I4B ) :: ierr
!   type( HDF5File_ ) :: meshfile

!   call display( "testing scalars of meshdata")
!   call meshfile%initiate( filename="./mesh.h5", mode="READ" )
!   call meshfile%open()
!   call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
!   call obj%GenerateMeshData()
!   call display( obj%uid, "uid = ")
!   call display( obj%xidim, "xidim = ")
!   call display( obj%elemType, "elemType = ")
!   call display( obj%nsd, "nsd = ")
!   call display( obj%maxNptrs, "maxNptrs = ")
!   call display( obj%minNptrs, "minNptrs = ")
!   call display( obj%tNodes, "tNodes = ")
!   call display( obj%tIntNodes, "tIntNodes = ")
!   call display( obj%tElements, "tElements = ")
!   call display( obj%minX, "minX = ")
!   call display( obj%minY, "minY = ")
!   call display( obj%minZ, "minZ = ")
!   call display( obj%maxX, "maxX = ")
!   call display( obj%maxY, "maxY = ")
!   call display( obj%maxZ, "maxZ = ")
!   call display( obj%x, "x = ")
!   call display( obj%y, "y = ")
!   call display( obj%z, "z = ")
!   call display( obj%isInitiated, "isInitiated = ")
!   call meshfile%close()
!   call meshfile%deallocateData()
! end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test0
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr
  type( HDF5File_ ) :: meshfile

  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
  call obj%deallocateData()
  call obj%initiate(meshfile=meshfile, xidim=1, id=1 )
  call obj%deallocateData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

! subroutine test1
!   type( mesh_ ) :: obj
!   class( ReferenceElement_ ), pointer :: refelem
!   class( element_ ), pointer :: elem
!   type( ParameterList_ ) :: param
!   integer( I4B ), ALLOCATABLE :: nptrs(:)
!   integer( I4B ) :: ierr

!   refelem => ReferenceLine_Pointer( NSD = 1 )
!   call FPL_INIT()
!   call param%init()

!   nptrs = [1,2]
!   ierr = param%set(key='nptrs', value=nptrs)
!   ierr = param%set(key="mat_type", value=1)
!   elem => Element_Pointer( param=param, refelem = refelem )
!   ierr = param%set(key="nsd", value=2)
!   ierr = param%set(key="size", value=10)
!   call obj%initiate(param)
!   call obj%pushback(Elem=elem)

!   nptrs = [2,3]
!   ierr = param%set(key='nptrs', value=nptrs)
!   ierr = param%set(key="mat_type", value=2)
!   elem => Element_Pointer( param=param, refelem = refelem )
!   call obj%pushback(Elem=elem)

!   nptrs = [3,4]
!   ierr = param%set(key='nptrs', value=nptrs)
!   ierr = param%set(key="mat_type", value=3)
!   elem => Element_Pointer( param=param, refelem = refelem )
!   call obj%pushback(Elem=elem)

!   nptrs = [4,5]
!   ierr = param%set(key='nptrs', value=nptrs)
!   ierr = param%set(key="mat_type", value=4)
!   elem => Element_Pointer( param=param, refelem = refelem )
!   call obj%pushback(Elem=elem)

!   ierr = param%set(key="mat_type", value=4)
!   elem => Element_Pointer( param=param, refelem = refelem )
!   call obj%setElement( iel = 4, elem = elem )
!   call obj%display( "mesh=" )

!   elem => obj%getElementPointer( iel=2 )
!   call elem%display( "getElementPointer()=" )
!   call display( obj%size(), "size()=" )

!   call obj%RemoveElement( iel=3, freeMem = .TRUE. )
!   call display( obj%size(), "size()=" )
!   call obj%display( "Removing iel=3 " )
!   ! call display( obj%size(), "size()=" )
!   ! call obj%Prune()

!   call FPL_Finalize()
! end subroutine


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module test_m

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_m
implicit none
call test0
end program main