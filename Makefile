all: conf build

conf:
	./Setup.hs configure

build:
	./Setup.hs build

clean:
	./Setup.hs clean
