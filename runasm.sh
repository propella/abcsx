#!/bin/sh
ASM=$(dirname $0)/asm.ss

$ASM $1 && avmshell $1.abc
