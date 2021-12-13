/* run-time functions for running Tiger programs in a POSIX environment.
 * 
 * $Id: sysposix.c,v 1.1 2002/08/25 05:06:41 shivers Exp $
 *
 * Almost all needed functions are in the standard C library...
 *
 */


#include <stdio.h>


void tig_flush()
{
   fflush(stdout);
}

