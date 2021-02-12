#define  INT int
#define BITS 8
#define AX( a, x ) AX1( a, x )
#define AX1( a, x ) a ## x


#define AXB( a, x, b ) AXB1( a, x, b)
#define AXB1( a, x, b ) a ## x ## b

INTEGER( AX( INT, BITS ) )  :: a

#define _X_ x
FUNCTION AXB( is, _X_, Defined )

#undef _X_
#define _X_ y
FUNCTION AXB( is, _X_, Defined )
