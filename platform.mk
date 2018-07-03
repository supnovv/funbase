PLAT = none
PLATS = linux macosx

MACRO =
POSINDEPCODE = -fPIC # position independent code

ifeq ($(PLAT), linux)
SHARED = $(POSINDEPCODE) -shared -Wl,-E -ldl
LDLIBS += -lpthread
MACRO += -DL_PLAT_LINUX
endif

ifeq ($(PLAT), macosx)
SHARED = $(POSINDEPCODE) -dynamiclib -Wl,-undefined,dynamic_lookup -ldl
MACRO += -DL_PLAT_MACOSX
endif

CC = gcc
CC89 = gcc -std=c89
CWARNS = -Wall -Wextra -Werror -Wno-error=unused-function -Wno-unused-function
CMACRO = $(MACRO)
CFLAGS = -g -O2
CINCPATH =
