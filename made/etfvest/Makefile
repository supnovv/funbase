LNLY_ROOT := ./../..

include $(LNLY_ROOT)/lnxbase.mk

CCPATH += -I$(LNLY_ROOT)/
LDPATH += -L$(LNLY_ROOT)/
LDLIBS += -llnly -llua -lpthread -ldl -lm
OUTDIR = .

ETFTRADE_EXE = $(OUTDIR)/etftrade$(EXE)
ETFTRADE_OBJ = $(OUTDIR)/etftrade$(OBJ)
ETFTRADE_INC =

default: echo $(ETFTRADE_EXE)

echo:
	@echo "CCPATH = $(CCPATH)"
	@echo "LDPATH = $(LDPATH)"
	@echo "LDLIBS = $(LDLIBS)"

clean:
	$(RM) $(ETFTRADE_OBJ) $(ETFTRADE_EXE)

.PHONY: default echo clean

$(OUTDIR)/%$(OBJ): %.c
	@echo "$@ <- $? | $(CMPL)"
	@$(CMPL) $<

$(ETFTRADE_EXE) : $(ETFTRADE_OBJ) $(ETFTRADE_INC)
	@echo "$@ <- $(ETFTRADE_OBJ) | $(LINK) $(LDLIBS)"
	@$(LINK) $(ETFTRADE_OBJ) $(LDLIBS)

