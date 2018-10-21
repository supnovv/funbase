CC = gcc -std=c99
CFLAGS = -g -O2
CWARNS = -Wall -Wextra -Werror -Wno-error=unused-function -Wno-unused-function
CINCPATH = -I./../../
CMACRO = -DL_PLAT_LINUX

CMPL_OPTIONS = $(CFLAGS) $(CWARNS) $(CINCPATH) $(CMACRO)
CMPL = $(CC) $(CMPL_OPTIONS) -c -o$@

LDFLAGS =
LDPATH = -L./../../
LDLIBS = -llnly -llua -lpthread -ldl -lm

LINK_OPTIONS = $(LDFLAGS) $(LDPATH)
LINK = $(CC) $(LINK_OPTIONS) -o$@

RM = rm -rf

OBJ = .o
EXE =

OUTDIR = .

ETFTRADE_EXE = $(OUTDIR)/etf-trade$(EXE)
ETFTRADE_OBJ = $(OUTDIR)/etf-trade$(OBJ)
ETFTRADE_INC =

default: $(ETFTRADE_EXE)

clean:
	$(RM) $(ETFTRADE_OBJ) $(ETFTRADE_EXE)

.PHONY: default clean

$(OUTDIR)/%$(OBJ): %.c
	@echo "$@ <- $? | $(CMPL)"
	@$(CMPL) $<

$(ETFTRADE_EXE) : $(ETFTRADE_OBJ) $(ETFTRADE_INC)
	@echo "$@ <- $(ETFTRADE_OBJ) | $(LINK) $(LDLIBS)"
	@$(LINK) $(ETFTRADE_OBJ) $(LDLIBS)

