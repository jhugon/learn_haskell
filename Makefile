
makeCharts: makeCharts.hs Chebyshev.o
	ghc $<

Chebyshev.o: Chebyshev.hs
	ghc $^

.PHONY: clean
clean:
	rm -f *.o
	rm -f *.hi
	rm -f makeCharts

