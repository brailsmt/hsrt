SRC_DIR=./src

all:
	ghc -threaded -i${SRC_DIR} --make HSRT Main -threaded -outputdir target -o hsrt
