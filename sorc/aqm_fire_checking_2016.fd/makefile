SHELL=/bin/sh
#
SRCS    = aqm_fire_checking.f

OBJS    = aqm_fire_checking.o

FC      = ifort 
LDFLAGS =	    
LIBS    =   
        
CMD     = aqm_fire_checking_2016	

FFLAGS  = -O2 

all:	$(CMD)

$(CMD):	$(OBJS)
	$(FC) $(LDFLAGS) -openmp -o ../../exec/$(@) $(OBJS) $(LIBS)
clean:
	-rm -f $(OBJS) $(CMD)
