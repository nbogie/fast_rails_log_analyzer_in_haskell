PROG  = Main
SOURCES = Types.hs Stats.hs Parser.hs Main.hs Analyzer.hs Testing.hs DemoHelp.hs

$(PROG) : $(SOURCES)
	$(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f *.hi *.o *.hp *.aux *.prof $(SOURCES:.hs=.prof) $(PROG)
tidy:
	rm -f *.hi *.o *.hp *.aux *.prof $(SOURCES:.hs=.prof) 

HC=ghc
