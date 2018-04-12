all: fig.exe
fig.exe: Main.hs Shapes.hs Geom.hs Math.hs
	mkdir -p build
	ghc --make Main.hs -o fig.exe -outputdir build

clean:
	rm -f build fig.exe