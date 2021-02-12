program main
    use basetype
    use basemethod
    use penf_stringify
    use mshType

    type( msh4_ ) :: obj
    logical( LGT ) :: ierr
    type( file_ ) :: afile
    integer( I4B ) :: i, j, k, entityDim, entityTag, numNodesInBlock
    real( dfp ), allocatable :: nodecoord( :, : )

    call obj % initiate( "./", "mesh", ".msh" , 2 )

    ! call display( obj, "obj" )
    ! call obj % getNodes( nodecoord )
    ! call display( nodecoord, "nodecoord = " )
    ! call obj % getNodes( str = "$Nodes", endstr = "$EndNodes")
end program main
