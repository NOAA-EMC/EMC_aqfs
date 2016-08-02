#include <stdlib.h>
#include <string.h>

#define  BUFLEN  1024

#if FLDMN

#define RETRIEVE_ENV  retrieve_env_

#elif defined(__hpux) || defined(_AIX)

#define RETRIEVE_ENV  retrieve_env

#endif

void RETRIEVE_ENV ( const char *env_name,
                          char *env_value,
                    int        env_name_len,
                    int        env_value_len)

    { char  *env_str;
      char  loc_env_name[ BUFLEN ] ;
      int   i;

      for (i = 0; i < env_name_len; i++, env_name++)
          loc_env_name[i] = *env_name;
    
      loc_env_name[i] = '\0';

      if (env_str = getenv( loc_env_name ))
         strcpy (env_value, env_str);
      else   /* environment variable does not exist */
         strcpy (env_value, " ");

    }
