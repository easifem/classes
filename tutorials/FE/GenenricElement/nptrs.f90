program main
  use basetype
  use basemethod
  use GenericElement_Class

  type( GenericElement_ ) :: Obj
  type( ElementData_ ) :: ElemData
  integer( i4b ), allocatable :: nptrs( : )

  ElemData % NSD = 2
  ElemData % XiDimension = 2
  ElemData % ElemTopology = Triangle
  ElemData % Mat_Type = 1

  call initiate( obj, [1, 2, 3], ElemData)
  call display( obj, "obj")
  call obj % getNptrs( nptrs )
  call display( nptrs, "getnptrs()" )

  nptrs = .nptrs. obj
  call display( nptrs, ".nptrs." )

  call display( (.nne. obj), ".nne." )
  call display( (.nns. obj), ".nns." )

  call obj % setNptrs( [100, 102, 111])
  call display( obj, "after setNptrs")

  call display( obj .localnode. 102, "local node")

  call display( obj % isNodePresent( 1 ), "isNodePresent 1" )
  call display( obj % isNodePresent( 111 ), "isNodePresent 111" )
end program main