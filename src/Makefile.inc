
### Machine dependent ###


FC      = gfortran
FCold   = $(FC)
FC90    = $(FC)
LDR     = $(FC)

OPT     = -O2 
DBG     = -fbacktrace -fbounds-check -g -Wall -ffpe-trap=invalid,zero,overflow -Wno-unused-function -DDEBUG -Wno-unused-dummy-argument 
PRF     = -O0 -g -pg -p

FFLAGS  = ${OPT} -ffree-line-length-256  
CFLAGS  = -O2
JFLAGS  = -O

LFLAGS  =
LIBS    =   
