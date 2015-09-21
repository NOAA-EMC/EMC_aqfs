SHELL=/bin/sh
#
SRCS    = aqm_fire_checking.f

OBJS    = aqm_fire_checking.o

FC      = ${F_COMP} 
LDFLAGS =	    
LIBS    =   
        
CMD     = aqm_fire_checking	

FFLAGS  = -O2 

all:	$(CMD)

$(CMD):	$(OBJS)
	$(FC) $(LDFLAGS) -o ../../exec/$(@) $(OBJS) $(LIBS)
clean:
	-rm -f $(OBJS) ../../exec/$(CMD)
