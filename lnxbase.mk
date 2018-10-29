CC = gcc -std=c99
CFLAGS = -g -O2
CWARNS = -Wall -Wextra -Werror -Wno-error=unused-function -Wno-unused-function
CCPATH = -I./
CMACRO = -DL_PLAT_LINUX

CMPL_OPTIONS = $(CFLAGS) $(CWARNS) $(CCPATH) $(CMACRO)
CMPL = $(CC) $(CMPL_OPTIONS) -c -o$@

LDFLAGS =
LDPATH = -L./lib/
LDLIBS =

LINK_OPTIONS = $(LDFLAGS) $(LDPATH)
LINK = $(CC) $(LINK_OPTIONS) -o$@

AR = ar rcv $@
RM = rm -rf
MKDIR = mkdir -p

OBJ = .o
EXE =
LIB = .a
DLL = .so

