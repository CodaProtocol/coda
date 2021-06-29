/* caml_syslimits.c */

#include <limits.h>
#include <caml/mlvalues.h>

/* maximum length of a file path in the OCaml compiler */
CAMLprim value caml_syslimits_path_max() {
  return Val_int(PATH_MAX);
}
