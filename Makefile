.PHONY: all
all:
	runhaskell -W TrackCAD.hs

.PHONY: clean
clean:
	-rm *.svg

