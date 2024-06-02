all:
	pack build tinyidris.ipkg

run:
	pack run tinyidris.ipkg

clean:
	rm -rf build
