test: minic.exe
	./minic.exe --debug test.c

minic.exe:
	dune build minic.exe

typing-tests:
	dune build minic.exe
	cd  ./tests ; \
	./test -2 ../minic.exe

clean:
	dune clean

.PHONY: clean minic.exe
