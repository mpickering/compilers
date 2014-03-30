#include "obx.h"
#include <stdio.h>

unsigned prim_check = 0;

#define args (bp + HEAD + 1)

static void Lib_Print(value *sp) {
     value *bp = sp;
     printf(" %d", args[0].i);
}

static void Lib_Newline(value *sp) {
     printf("\n");
}

void dltrap(value *sp) {
     fprintf(stderr, "Oops: dltrap called!\n");
     exit(2);
}

primitive *primtab[] = {
     interp, dltrap, Lib_Print, Lib_Newline,
     NULL
};

char *primname[] = {
     "INTERP", "DLTRAP", "Lib_Print", "Lib_Newline"
};

