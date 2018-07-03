default: ms.PHONY
	@echo "make <platform> # linux macosx"
	@echo "nmake <platform> # windows"

linux: ms.PHONY
	$(MAKE) PLAT="linux" -f platform.mk

macosx: ms.PHONY
	$(MAKE) PLAT="macosx" -f platform.mk

windows: ms.PHONY
	$(MAKE) PLAT="windows" -f msplatform.mk

clean: ms.PHONY
	$(MAKE) clean -f $(?B)platform.mk

test: ms.PHONY
	$(MAKE) test -f $(?B)platform.mk

ms.PHONY:

