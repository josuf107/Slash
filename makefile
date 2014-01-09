bin/Java : Java.hs Slash.hs Java/AST.hs Slash/Handler.hs
	ghc -package-conf=cabal-dev/packages-7.4.1.conf/ -O2 -Wall --make Java -main-is Java -o bin/Java

.PHONY : clean

clean:
	rm -f *.hi */*.hi *.o */*.o bin/*
