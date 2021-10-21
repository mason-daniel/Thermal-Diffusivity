.SUFFIXES: .o .F .f .f90 .c .java .class .F90

#	all the source in dependency order

 
ASIDE_DEF = \
NBAX_StringTokenizers.o         \
Lib_CommandLineArguments.o      \
thermalConductivity.o    

################################

include Makefile.inc

  

ASIDE = thermalConductivity.exe

all:
	make aside

	
aside:  $(ASIDE)


$(ASIDE) : $(ASIDE_DEF)
	$(LDR) $(LFLAGS) $(FFLAGS) -o $(ASIDE) $(ASIDE_DEF) $(LIBS)
	cp -u $(ASIDE) ../bin/.



#	rules for compiling source


.F90.o:
	$(FC90) -c $(FFLAGS) $*.F90

.f90.o:
	$(FC90) -c $(FFLAGS) $*.f90

.f.o:
	$(FCold) -c $(FFLAGS) $*.f	-ffree-form

.F.o:
	$(FC) -c $(FFLAGS) $*.F -FR

.c.o:
	$(CC) -c $(CFLAGS) $*.c

.java.class:
	$(JAVAC) $(JFLAGS) $*.java

#	rules for tidying up


clean:
	rm -f *.o *.run *.mod *.il ifc* *.a *.so *.mpi *.class


#################
