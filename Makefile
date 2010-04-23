#DUMP=./dump.ss
#DUMP=./abcsx-gauche.scm -dump
#ASM=./abcsx-gauche.scm -asm
DUMP=./abcsx.ss -dump
ASM=./abcsx.ss -asm
RUNASM := ./runasm.sh

REGRESSION = examples/textField

# asm test
run :
	$(RUNASM) examples/hello.sx
	$(RUNASM) examples/type.sx
	$(RUNASM) examples/parseInt.sx
	$(RUNASM) examples/arithmetic.sx
	$(RUNASM) examples/ifte.sx
	$(RUNASM) examples/callLocal.sx
	$(RUNASM) examples/call.sx
	$(RUNASM) examples/send.sx
	$(RUNASM) examples/closure.sx
	$(RUNASM) examples/class.sx
	$(RUNASM) examples/namespace.sx
	$(RUNASM) examples/array.sx
	$(RUNASM) examples/with.sx
#	$(RUNASM) examples/activation.sx -- works only before tamarin 711

run-gosh :
	$(MAKE) ASM="./abcsx-gauche.scm -asm" run

partial : examples/partial1.sx.abc examples/partial2.sx.abc
	avmshell examples/partial1.sx.abc examples/partial2.sx.abc

all : test test-dump run run-gosh partial test-regression test-swf

# unit test
test :
	./abcsx.ss -test
	./abcsx-gauche.scm -test

# dump test
test-dump : examples/textField.abc
	$(DUMP) examples/textField.abc

# swf test
test-swf :
	$(ASM) examples/textField.sx
	./swf_abc.erl 100 100 Hello examples/textField.sx.abc
	open Hello.swf

# regression test
test-regression :
	$(ASM) $(REGRESSION).sx
	$(DUMP) $(REGRESSION).sx.abc > $(REGRESSION)2.sx
	$(ASM) $(REGRESSION)2.sx
	$(DUMP) $(REGRESSION)2.sx.abc > $(REGRESSION)3.sx
	diff $(REGRESSION)2.sx $(REGRESSION)3.sx

clean :
	rm -f *.abc examples/*.abc launcher/*.abc Hello.swf
	rm -f $(REGRESSION)2.sx $(REGRESSION)3.sx

%.abc : %.as
#	asc $<
	asc -import ~/src/tamarin-central/core/builtin.abc -import examples/flashglobal.as $<

%.sx.abc : %.sx
	$(ASM) $<

