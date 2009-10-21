#!/bin/sh
#ASM=$(dirname $0)/asm.ss
ASM=$(dirname $0)/test-gauche.scm

$ASM $1 && avmshell $1.abc
