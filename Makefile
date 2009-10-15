PROG=samehask

all: $(PROG)

$(PROG): Main.hs
	ghc --make Main.hs -o $(PROG)

clean:
	\rm -f $(PROG) Main.hi Main.o

run: $(PROG)
	./samehask