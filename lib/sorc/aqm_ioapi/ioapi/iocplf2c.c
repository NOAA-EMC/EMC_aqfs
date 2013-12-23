
/**********************************************************************
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "iocplf2c.c" version "@(#)$Header$"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Fortran bindings for I/O API Coupling Mode.

CALLS:  PVM

REVISION HISTORY:
        Prototype  6/1998 by Atanas Trayanov, MCNC EMC.

**********************************************************************/

#include <string.h>
#include "fdesc3.h"

#define MAX_STR_LEN 512
#define FALSE 0

typedef int  Integer;
typedef long Logical;

int desc3net(char *name, IOAPI_Bdesc3 *bdesc3p, IOAPI_Cdesc3 *cdesc3p) ;
int close3net(char *name) ;
int open3net(char *fname, 
	     IOAPI_Bdesc3 *bdesc3p, IOAPI_Cdesc3 *cdesc3p,
	     int status, char *pgm) ;
int read3net(char *fname, char *vname, int skip, 
	     int jdate, int jtime, void *buffer, int n, int buftype) ;
int shut3net() ;
int write3net(char *fname, char *vname, 
	      int jdate, int jtime, void *buffer, int n, int buftype) ;

/* =========================================================== */
/* =========================================================== */

IOAPI_Bdesc3 *get_bdesc3f()
{

  extern IOAPI_Bdesc3 bdesc3_;
  IOAPI_Bdesc3 *bdesc3p = &bdesc3_;

  return (bdesc3p);

}
/* =========================================================== */
IOAPI_Cdesc3 *get_cdesc3f()
{

  extern IOAPI_Cdesc3 cdesc3_;
  IOAPI_Cdesc3 *cdesc3p = &cdesc3_;

  return (cdesc3p);

}
/* =========================================================== */
/* =========================================================== */
/*
static int f2cstring(char *cstr, char *fstr, int len)
{
  strncpy(cstr, fstr, len);
  cstr[len]='\0';

  return 1;
}
*/
#ifndef min
#define min(i,j) ((i)<(j)?(i):(j))
#endif


static int f2cstring(ds, ss, len)
    char *ds, *ss;      /* dst, src ptrs */
    int len;             /* src len */
{
    char *p;
    int dl = MAX_STR_LEN;

    for (p = ss + len; --p >= ss && *p == ' '; ) ;
    len = p - ss + 1;
    dl--;
    ds[0] = 0;
    if (len > dl)
        return 1;
    strncat(ds, ss, min(len, dl));
    return 0;
}

/* =========================================================== */

Logical open3v_
     (fname_ptr, mode, pgmname_ptr, fname_len, pgmname_len)
  char * fname_ptr; int fname_len;  
  char * pgmname_ptr; int pgmname_len;  
  Integer *mode;
{

  int filemode=*mode;
  char name[MAX_STR_LEN];
  char pgm[MAX_STR_LEN];
  IOAPI_Bdesc3 *get_bdesc3f();
  IOAPI_Cdesc3 *get_cdesc3f();
  IOAPI_Bdesc3 *bdesc3p;
  IOAPI_Cdesc3 *cdesc3p;
  char *trim;


  f2cstring(name, fname_ptr, fname_len);
  f2cstring(pgm, pgmname_ptr, pgmname_len);
  trim = strrchr(pgm,'/');
  
  bdesc3p=get_bdesc3f();
  cdesc3p=get_cdesc3f();

  return open3net(name, bdesc3p, cdesc3p, filemode, (trim ? trim+1:pgm));
}

/* =========================================================== */

Logical desc3v_
     (fname_ptr, fname_len)
  char * fname_ptr; int fname_len;  
{

  char name[MAX_STR_LEN];
  IOAPI_Bdesc3 *get_bdesc3f();
  IOAPI_Cdesc3 *get_cdesc3f();
  IOAPI_Bdesc3 *bdesc3p;
  IOAPI_Cdesc3 *cdesc3p;


  f2cstring(name, fname_ptr, fname_len);
  
  bdesc3p=get_bdesc3f();
  cdesc3p=get_cdesc3f();

  return desc3net(name, bdesc3p, cdesc3p);
}

/* =========================================================== */
Logical shut3v_st_
     (void)
{
  return shut3net();
}

/* =========================================================== */

Logical read3v_st_
     (fname_ptr,vname_ptr,
	  iskip,jdate,jtime,buffer,icount,ftype, fname_len, vname_len)
  char * fname_ptr; int fname_len;  
  char * vname_ptr; int vname_len;
  Integer *iskip;
  Integer *jdate;
  Integer *jtime;
  Integer *icount;
  Integer *ftype;
  void *buffer;
{
  int skip=*iskip;
  int date=*jdate;
  int time=*jtime;
  int count=*icount;
  int type=*ftype;
  char fnm[MAX_STR_LEN], vnm[MAX_STR_LEN];

  f2cstring(fnm, fname_ptr, fname_len);
  f2cstring(vnm, vname_ptr, vname_len);

  return read3net(fnm,vnm,skip,date,time,buffer,count,type);
}

/* =========================================================== */

Logical write3v_st_
     (fname_ptr,vname_ptr,
	  jdate,jtime,buffer,icount,ftype, fname_len, vname_len)
  char * fname_ptr; int fname_len;  
  char * vname_ptr; int vname_len;
  Integer *jdate;
  Integer *jtime;
  Integer *icount;
  Integer *ftype;
  void *buffer;
{

  int date=*jdate;
  int time=*jtime;
  int count=*icount;
  int type=*ftype;
  char fnm[MAX_STR_LEN], vnm[MAX_STR_LEN];

  f2cstring(fnm, fname_ptr, fname_len);
  f2cstring(vnm, vname_ptr, vname_len);

  return write3net(fnm,vnm,date,time,buffer,count,type);
}

/* =========================================================== */

void get_iocpl_version_
     (vers_ptr, vers_len)
  char * vers_ptr; int vers_len;  
{
  extern char *iocpl_version();
  char *version, *Ftrn_str;
  int i, len;

  Ftrn_str = vers_ptr; 
  len = vers_len;
  /* fill the Fortran string with blanks */
  for (i=0; i< len; i++) {
    Ftrn_str[i] = ' ';
  }

  version = iocpl_version();
  strncpy(Ftrn_str, version, min(len,STRLEN(version)));
}

/* =========================================================== */

void get_pvm_version_
     (vers_ptr, vers_len)
  char * vers_ptr; int vers_len;  
{
  extern char *pvm_version();
  char *version, *Ftrn_str;
  int i, len;

  Ftrn_str = vers_ptr; 
  len = vers_len;
  /* fill the Fortran string with blanks */
  for (i=0; i< len; i++) {
    Ftrn_str[i] = ' ';
  }

  version = pvm_version();
  strncpy(Ftrn_str, version, min(len,STRLEN(version)));
}

/* =========================================================== */

void setsynchro3v_
     (newvalue)
   Integer *newvalue;
{
  int new_value=*newvalue;
  void setWaitForSynchro(int);


  setWaitForSynchro(new_value);
}


/* =========================================================== */
/* =========================================================== */
/* =========================================================== */
/* =========================================================== */
/* =========================================================== */
/* =========================================================== */
