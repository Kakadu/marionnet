/* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009  Luca Saiu

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. */


#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <assert.h>

#include <memory.h>
#include <mlvalues.h>
/* If I don't #include caml/alloc.h then this module compiles fine, but then crashes
   at runtime. Funny, isn't it? */
#include <alloc.h>

/* Return true iff the process identified by the given pid exists. The idea is just to
   call kill with 0 as a signal, which is easy in C but impossible in OCaml: */
CAMLprim value does_process_exist_c(value pid_as_an_ocaml_value){
  /* Just to be pedantic: it's not a pointer, so the GC doesn't really care: */
  CAMLparam1(pid_as_an_ocaml_value);
  
  /* Convert the PID from the OCaml encoding to the C encoding: */
  int pid_as_c_value = Int_val(pid_as_an_ocaml_value);
  
  /* Check whether the process exists, by killing it with a 0 signal: */
  const int kill_result = kill(pid_as_c_value, 0);
  if(kill_result == 0)
    CAMLreturn(Val_true); // the signal could be sent
  
  /* Ok, if we're here then the signal could *not* be sent; let's see why,
     by looking at errno: */
  switch(errno){
  case EPERM:
    /* The EPERM case is interesting: if we don't have the permission to kill
       a process, it does exist. */
    CAMLreturn(Val_true);
  case ESRCH:
    CAMLreturn(Val_false);
  case EINVAL: // wrong signal number
  default:
    assert(false);
  } // switch
}
