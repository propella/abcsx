#!/bin/sh

./asm.ss $1 && avmshell $1.abc
