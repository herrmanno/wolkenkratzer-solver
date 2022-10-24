all: solve

solve:
	ghc -O3 -o solve main.hs

clean:
	rm -f *.o
	rm -f *.hi
	rm -f solve

.PHONY: clean
