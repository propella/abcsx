#DUMP=./dump.ss
#DUMP=./abcsx-gauche.scm -dump
#ASM=./abcsx-gauche.scm -asm
DUMP=./abcsx.ss -dump
ASM=./abcsx.ss -asm

all : test test1 test2 test3

test :
	./test-plt.ss
	./abcsx-gauche.scm -test

test1 : examples/textField.abc
	$(DUMP) examples/textField.abc

test2 : 
	./runasm.sh examples/hello.sx
	./runasm.sh examples/parseInt.sx
	./runasm.sh examples/arithmetic.sx
	./runasm.sh examples/ifte.sx
	./runasm.sh examples/callLocal.sx
	./runasm.sh examples/call.sx
	./runasm.sh examples/send.sx

test3 :
	./asm.ss examples/textField.sx
	./swf_abc.erl 100 100 Hello examples/textField.sx.abc
	open Hello.swf

clean :
	rm -f *.abc examples/*.abc launcher/*.abc Hello.swf

%.abc : %.as
#	asc $<
	asc -import ~/src/tamarin-central/core/builtin.abc -import examples/flashglobal.as $<

%.sx : %.abc
	./dump.ss $< > $@ || rm $@
