#define _TYPE_ INTEGER

#define _TOSTRING_( x ) _TS_( x )
#define _TS_( x ) #x

#define _TYPESTR_ _TOSTRING_( _TYPE_ )

#if _TYPESTR_ == "REAL"

#define TYPE REAL

#elif _TYPESTR_ == "INTEGER"

#define TYPE INTEGER

#endif

_TOSTRING_(_TYPE_)

TYPE