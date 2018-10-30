LNLY_ROOT := ./../..

include $(LNLY_ROOT)/lnxbase.mk

CCPATH += -I$(LNLY_ROOT)/
OUTDIR = .

OPTUNIT_EXE = $(OUTDIR)/optunit$(EXE)
OPTUNIT_OBJ = $(OUTDIR)/optunit$(OBJ)
OPTUNIT_INC =

default: $(OPTUNIT_EXE) do_optunit

clean:
	$(RM) $(OPTUNIT_OBJ) $(OPTUNIT_EXE)

do_optunit:
	$(OPTUNIT_EXE) a b c                  # -> [a] [b] [c]
	$(OPTUNIT_EXE) -a100 -b200            # -> [-a100] [-b200]
	$(OPTUNIT_EXE) -a  100  200           # -> [-a] [100] [200]
	$(OPTUNIT_EXE) -a' 100 200 '          # -> [-a 100 200 ]
	$(OPTUNIT_EXE) -b" 200 300 "          # -> [-b 200 300 ]
	$(OPTUNIT_EXE) --a=10 --b = 20        # -> [--a=10] [--b] [=] [20]
	$(OPTUNIT_EXE) ' a"b ' c              # -> [ a"b ] [c]
	$(OPTUNIT_EXE) " a'b " c              # -> [ a'b ] [c]

.PHONY: default clean do_optunit

$(OUTDIR)/%$(OBJ): %.c
	@echo "$@ <- $? | $(CMPL)"
	@$(CMPL) $<

$(OPTUNIT_EXE) : $(OPTUNIT_OBJ) $(OPTUNIT_INC)
	@echo "$@ <- $(OPTUNIT_OBJ) | $(LINK) $(LDLIBS)"
	@$(LINK) $(OPTUNIT_OBJ) $(LDLIBS)

