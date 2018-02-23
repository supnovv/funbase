#!/bin/bash -v
ctags -R --languages=c --langmap=c:+.h -h +.h
