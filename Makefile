.PHONY: all
all: dist/build/LatteCompiler/LatteCompiler

dist/build/LatteCompiler/LatteCompiler: src/Main.hs src/SemanticAnalysis.hs src/CodeGeneration.hs src/BNFC/AbsLatte.hs src/BNFC/ErrM.hs src/BNFC/LexLatte.hs src/BNFC/ParLatte.hs src/BNFC/PrintLatte.hs src/BNFC/SkelLatte.hs
	cabal build

src/BNFC/AbsLatte.hs src/BNFC/ErrM.hs src/BNFC/LexLatte.hs src/BNFC/ParLatte.hs src/BNFC/PrintLatte.hs src/BNFC/SkelLatte.hs: src/Latte.cf
	cd src && bnfc -p BNFC Latte.cf
	cd src && happy -gca BNFC/ParLatte.y
	cd src && alex -g BNFC/LexLatte.x
	touch src/BNFC/*
