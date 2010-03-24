#DUMP=./dump.ss
#DUMP=./abcsx-gauche.scm -dump
#ASM=./abcsx-gauche.scm -asm
DUMP=./abcsx.ss -dump
ASM=./abcsx.ss -asm

REGRESSION = examples/textField

# asm test
run : 
	./runasm.sh examples/hello.sx
	./runasm.sh examples/parseInt.sx
	./runasm.sh examples/arithmetic.sx
	./runasm.sh examples/ifte.sx
	./runasm.sh examples/callLocal.sx
	./runasm.sh examples/call.sx
	./runasm.sh examples/send.sx
	./runasm.sh examples/closure.sx
	./runasm.sh examples/class.sx
	./runasm.sh examples/namespace.sx
	./runasm.sh examples/array.sx
	./runasm.sh examples/with.sx
#	./runasm.sh examples/activation.sx -- works only before tamarin 711

all : test test-dump run test-regression test-swf

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

%.sx : %.abc
	$(DUMP) $< > $@ || rm $@
