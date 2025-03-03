all: build

build: 
	pack build tinyidris.ipkg

run: 
	pack run tinyidris.ipkg Test/Nat.idr

watch:
	find src -iname "*.idr" | entr -sndc 'pack build tinyidris.ipkg && echo "\n" && pack run tinyidris.ipkg Test/Nat.idr'

clean:
	pack clean
