LNLY_ROOT := ./../..

include $(LNLY_ROOT)/lnxbase.mk

CCPATH += -I$(LNLY_ROOT)/
LDPATH += -L$(LNLY_ROOT)/
LDLIBS += -llnly -llua -lpthread -ldl -lm
OUTDIR = .

RSSIDBM_EXE = $(OUTDIR)/rssidbm$(EXE)
RSSIDBM_OBJ = $(OUTDIR)/rssidbm$(OBJ)
RSSIDBM_INC =

default: echo $(RSSIDBM_EXE)

echo:
	@echo "CCPATH = $(CCPATH)"
	@echo "LDPATH = $(LDPATH)"
	@echo "LDLIBS = $(LDLIBS)"

clean:
	$(RM) $(RSSIDBM_OBJ) $(RSSIDBM_EXE)

.PHONY: default echo clean

$(OUTDIR)/%$(OBJ): %.c
	@echo "$@ <- $? | $(CMPL)"
	@$(CMPL) $<

$(RSSIDBM_EXE) : $(RSSIDBM_OBJ) $(RSSIDBM_INC)
	@echo "$@ <- $(RSSIDBM_OBJ) | $(LINK) $(LDLIBS)"
	@$(LINK) $(RSSIDBM_OBJ) $(LDLIBS)

