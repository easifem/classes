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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test0
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr, iel
  type( HDF5File_ ) :: meshfile

  call display( "testing Generate MeshData")
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
  obj%meshData%refelem => ReferenceTriangle_Pointer( nsd=2 )
  call obj%GenerateMeshData()
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test8
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr, iel
  type( HDF5File_ ) :: meshfile

  call display( "testing Boundary Data")
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
  call obj%GenerateMeshData()
  obj%meshData%refelem => ReferenceTriangle_Pointer( nsd=2 )

  call obj%meshData%InitiateBoundaryData()

  call display( obj%meshData%isBoundaryDataInitiated(), "isBoundaryDataInitiated=")

  iel = obj%meshData%getLocalElemNumber( 207 )
  call display( iel, "local element number = " )
  call display( obj%meshData%getConnectivity(iel), "connectivity = ")
  call display( obj%meshData%isBoundaryElement( iel ), "T=" )
  call display( obj%meshData%getBoundaryElementData( iel ), "boundary data = " )

  iel = obj%meshData%getLocalElemNumber( 65 )
  call display( iel, "local element number = " )
  call display( obj%meshData%getConnectivity(iel), "connectivity = ")
  call display( obj%meshData%isBoundaryElement( iel ), "T=" )
  call display( obj%meshData%getBoundaryElementData( iel ), "boundary data = " )

  iel = obj%meshData%getLocalElemNumber( 151 )
  call display( obj%meshData%isBoundaryElement( iel ), "F=" )


  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test7
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr
  type( HDF5File_ ) :: meshfile

  call display( "testing Initiate Element to Elements Mapping")
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
  call obj%GenerateMeshData()
  obj%meshData%refelem => ReferenceTriangle_Pointer( nsd=2 )

  call obj%meshData%initiateElementToElements()

  call display( obj%meshData%isElementToElementsInitiated(), "isElementToElementsInitiated=")

  call display( obj%meshData%getElementToElements( obj%meshData%getLocalElemNumber( 33 ) ), "elements around 33 = " )

  call display( obj%meshData%getElementToElements( &
    & obj%meshData%getLocalElemNumber( 33 ), onlyElements=.TRUE. ), "elements around 33 = " )

  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test6
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr
  type( HDF5File_ ) :: meshfile

  call display( "testing Initiate Node to Nodes Mapping")
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
  call obj%GenerateMeshData()

  call obj%meshData%initiateNodeToNodes()
  call display( obj%meshData%isNodeToNodesInitiated(), "isNodeToNodesInitiated=")
  call display( obj%meshData%getNodeToNodes( globalNode=1, IncludeSelf=.TRUE. ), "Node 1 = " )
  call display( obj%meshData%getNodeToNodes( globalNode=33, IncludeSelf=.FALSE. ), "Node 33 = " )
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr
  type( HDF5File_ ) :: meshfile

  call display( "testing Initiate Node Element Mapping")
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
  call obj%GenerateMeshData()

  call obj%meshData%initiateNodeToElements()
  call display( obj%meshData%isNodeToElementsInitiated(), "isNodeToElementsInitiated=")
  call display( obj%meshData%getGlobalElemNumber(obj%meshData%getNodeToElements(1)),"elem for node 1=" )
  call display( obj%meshData%getGlobalElemNumber(obj%meshData%getNodeToElements(96)),"elem for node 96=" )
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr
  type( HDF5File_ ) :: meshfile

  call display( "testing query methods of meshdata")
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
  call obj%GenerateMeshData()
  call display( obj%meshData%isNodeToNodesInitiated(), "isNodeToNodesInitiated")

  call display( obj%meshData%isNodeToElementsInitiated(), "isNodeToElementsInitiated=")

  call display( obj%meshData%isElementToElementsInitiated(), "isElementToElementsInitiated=")

  call display( obj%meshData%isConnectivityInitiated(), "isConnectivityInitiated=")

  call display( obj%meshData%isBoundaryDataInitiated(), "isBoundaryDataInitiated=")

  call display( obj%meshData%isInternalNptrsInitiated(), "isInternalNptrsInitiated=")

  call display( obj%meshData%isBoundaryNptrsInitiated(), "isBoundaryNptrsInitiated=")

  call display( obj%meshData%isLocalNptrsInitiated(), "isLocalNptrsInitiated=")

  call display( obj%meshData%isInternalBoundaryDataInitiated(), "isInternalBoundaryDataInitiated=")

  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr
  type( HDF5File_ ) :: meshfile

  call display( "testing scalars of meshdata")
  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
  call obj%GenerateMeshData()
  call display( obj%meshData%uid, "uid = ")
  call display( obj%meshData%xidim, "xidim = ")
  call display( obj%meshData%elemType, "elemType = ")
  call display( obj%meshData%nsd, "nsd = ")
  call display( obj%meshData%maxNptrs, "maxNptrs = ")
  call display( obj%meshData%minNptrs, "minNptrs = ")
  call display( obj%meshData%tNodes, "tNodes = ")
  call display( obj%meshData%tIntNodes, "tIntNodes = ")
  call display( obj%meshData%tElements, "tElements = ")
  call display( obj%meshData%minX, "minX = ")
  call display( obj%meshData%minY, "minY = ")
  call display( obj%meshData%minZ, "minZ = ")
  call display( obj%meshData%maxX, "maxX = ")
  call display( obj%meshData%maxY, "maxY = ")
  call display( obj%meshData%maxZ, "maxZ = ")
  call display( obj%meshData%x, "x = ")
  call display( obj%meshData%y, "y = ")
  call display( obj%meshData%z, "z = ")
  call display( obj%meshData%isInitiated, "isInitiated = ")
  call meshfile%close()
  call meshfile%deallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
  type( mesh_ ) :: obj
  integer( I4B ) :: ierr
  type( HDF5File_ ) :: meshfile

  call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  call meshfile%open()
  call obj%initiate(meshfile=meshfile, xidim=2, id=1 )
  call obj%GenerateMeshData()
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