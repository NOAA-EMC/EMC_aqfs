#!/bin/ksh
SRC=date2day_of_year.f
OBJ=date2day_of_year.o
CMD=aqm_date2day

all: 		$(CMD)

$(CMD):		$(OBJ)
	ncepxlf -o $(@) $(OBJ)

$(OBJ)	: $(SRC)
	ncepxlf -c $<
