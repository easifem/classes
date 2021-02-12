!#define X y
!#define X1() X
!#define AxB(x) A(x) ## B
!#define A(x) B(x) 
!#define B(x) A ## x

#define X y
#define AxB(a, x, b) A(a, x) ## b
#define A(a,x) B(a,x)
#define B(a,x) a ## x

#define Ax(a, x) Ax1(a,x)
#define Ax1(a,x) a ## x

AxB(Hello,X,World)

Ax( Hello, World )

