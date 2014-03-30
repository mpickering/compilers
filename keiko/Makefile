#
# runtime/Makefile.  Generated from Makefile.in by configure.
#
# runtime/Makefile.in
# 
# This file is part of the Oxford Oberon-2 compiler
# Copyright (c) 2006 J. M. Spivey
# All rights reserved
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
# 3. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
# IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# Config stuff
AR = ar
RANLIB = ranlib
CFLAGS = -g -O2
RTFLAGS = $(CFLAGS) -fno-strict-aliasing
TCLSH = tclsh
GC_PAGESIZE = 4096
# End

include config.mk

all :: ppx pplink

ALL_CFLAGS = $(CFLAGS) -Wall $(INCLUDE)
ALL_RTFLAGS = $(RTFLAGS) -Wall $(INCLUDE)

PPLINK = oblink.o linker.o symtab.o template.o util.o
pplink : $(PPLINK) 
	$(CC) $(CFLAGS) -o $@ $^

$(PPLINK) : %.o : %.c
	$(CC) $(ALL_CFLAGS) -c $< -o $@

COMMON = support.o loader.o gc0.o
OBX = interp.o xmain.o $(COMMON)
PACK = $(AR) cr $@ $^ && $(RANLIB) $@

ppx: obx-t.a lib.o 
	$(CC) $(ALL_RTFLAGS) $^ -o $@

lib.o: lib.c
	$(CC) -c $(ALL_RTFLAGS) $< -o $@

obx-t.a : interp-t.o xmain-t.o trace.o $(COMMON);		$(PACK)

# Cancel default rule
%.o: %.c

$(OBX) trace.o: %.o : %.c
	$(CC) $(ALL_RTFLAGS) -c $< -o $@

%-t.o : %.c
	$(CC) $(ALL_RTFLAGS) -DTRACE -c $< -o $@

GEN = keiko.h template.c interp.c

generate : $(GEN)

keiko.h template.c interp.c : iset.tcl keiko.iset iskel.c config.tcl
	$(TCLSH) iset.tcl keiko.iset iskel.c keiko.h template.c interp.c


## Cleanup

# clean: remove all object files
clean:
	rm -f pplink ppx *.[ao]

# realclean: remove generated source too
realclean: clean
	rm -f $(GEN)

.DELETE_ON_ERROR:

###

support.o interp.o interp-t.o interp-p.o interp-d.o profile.o \
xmain.o xmain-t.o xmain-p.o xmain-d.o xmain-j.o debug.o \
trace.o loader.o jit.o jitlab.o gc0.o: \
	obx.h obcommon.h config.h exec.h

interp.o interp-t.o xmain.o xmain-t.o trace.o: \
	keiko.h

linker.o oblink.o symtab.o template.o util.o: \
	oblink.h obcommon.h config.h exec.h util.h keiko.h
