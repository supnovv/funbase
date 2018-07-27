# gcc - GNU project C and C++ compiler
#
# gcc [-c|-S|-E] [-std=standard]
#     [-g] [-pg] [-Olevel]
#     [-Wwarn...] [-Wpedantic]
#     [-Idir...] [-Ldir...]
#     [-Dmacro[=defn]...] [-Umacro]
#     [-foption...] [-mmachine-option...]
#     [-o outfile] [@file] infile...
#
# -E output code after preprocess, -S output code after assemble, -c output object code, if none of these flags specified then output executable code
# in all above cases, -o flag can be used to specify a output file name, otherwise a default file name is used
#
# default variables in make
# $@ - the target file name
# $< - the 1st prerequisite file name
# $^ - all prerequisite file names, separated by space, and remove duplicated names
# $+ - all prerequisite file names, separated by space, duplicated names are not removed
# $? - all prerequisite file names that the timestamp is later than the target file
# $* - the target file name with suffix removed, empty if the target file doesn't have suffix
#
# where the make variables come from?
# (priority level "command line variable" > "makefile variable" > "envrionment variable",
# use "override" keyword ahead the variable define in the makefile can avoid it overrided
# by command line variables)
# 1. the make command line
# 2. include from other makefile
# 3. defined in the current makefile
# 4. the envirnment variables
#

PLAT = none
PLATS = linux macosx

CC = gcc
CC89 = gcc -std=c89
CFLAGS = -g -O2
CWARNS = -Wall -Wextra -Werror -Wno-error=unused-function -Wno-unused-function
CINCPATH = -I./
CMACRO =

POSINDEPCODE = -fPIC # position independent code

LDSTATIC = -Wl,-Bstatic
LDSHARED = -Wl,-Bdynamic

LDFLAGS =
LDPATH = -L./lib
LDLIBS = -lm -ldl -lpthread  # left library is more basic
LDLIBLUA = -llua

ifeq ($(PLAT), linux)
SHARED = $(POSINDEPCODE) -shared -Wl,-E -ldl
CMACRO += -DL_PLAT_LINUX
endif

ifeq ($(PLAT), macosx)
SHARED = $(POSINDEPCODE) -dynamiclib -Wl,-undefined,dynamic_lookup -ldl
CMACRO += -DL_PLAT_MACOSX
endif

CMPL_OPTIONS = $(CFLAGS) $(CWARNS) $(CINCPATH) $(CMACRO)
CMPL = $(CC89) $(CMPL_OPTIONS) -c -o$@

LINK_OPTIONS = $(LDFLAGS) $(LDPATH) $(LDLIBS)
LINK = $(CC89) $(LINK_OPTIONS) -o$@

RM = rm -rf
MKDIR = mkdir -p

OBJ = .o
EXE =
LIB = .a
DLL = .so

OUTDIR = ./build

AUTOCONF = $(OUTDIR)/autoconf$(EXE)
AUTOOBJS = $(OUTDIR)/autoconf$(OBJ)
AUTOINCS = core/prefix.h

COREOBJS = $(OUTDIR)/core/base$(OBJ) \
           $(OUTDIR)/core/lapi$(OBJ) \
           $(OUTDIR)/core/beat$(OBJ)

COREINCS = autoconf.h \
           core/prefix.h \
           core/base.h \
           core/lapi.h \
           core/beat.h

OSIOBJS = $(OUTDIR)/osi/lnxbase$(OBJ)

OSIINCS = $(COREINCS) \
          osi/base.h \
          osi/lnxpref.h

ifeq ($(PLAT), none)
default: none
else
default: echo MAKEDIR $(AUTOCONF) LNLYLIB
endif

none:
	@echo "make <platfrom> # $(PLATS)"

echo:
	@echo "PLAT= $(PLAT)"

clean:
	$(RM) $(OUTDIR) autoconf.h

$(OUTDIR)/%$(OBJ): %.c
	@echo "$@ <- $? | $(CMPL)"
	@$(CMPL) $<

$(AUTOCONF): $(AUTOOBJS) $(AUTOINCS)
	$(RM) autoconf.h
	@echo "$@ <- $(AUTOOBJS) | $(LINK)"
	@$(LINK) $(AUTOOBJS)
	./$@

MAKEDIR:
	$(MKDIR) $(OUTDIR)/core
	$(MKDIR) $(OUTDIR)/osi

LNLYLIB: $(COREOBJS) $(COREINCS) $(OSIOBJS) $(OSIINCS)

.PHONY: default none echo clean MAKEDIR LNLYLIB

