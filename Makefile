.PHONY: all
all: latc

dist/build/LatteCompiler/LatteCompiler: src/Main.hs src/SemanticAnalysis.hs src/CodeGeneration.hs src/BNFC/AbsLatte.hs src/BNFC/ErrM.hs src/BNFC/LexLatte.hs src/BNFC/ParLatte.hs src/BNFC/PrintLatte.hs src/BNFC/SkelLatte.hs lib/runtime.bc
	cabal build

latc: dist/build/LatteCompiler/LatteCompiler
	cp dist/build/LatteCompiler/LatteCompiler latc

src/BNFC/AbsLatte.hs src/BNFC/ErrM.hs src/BNFC/LexLatte.hs src/BNFC/ParLatte.hs src/BNFC/PrintLatte.hs src/BNFC/SkelLatte.hs: src/Latte.cf
	cd src && bnfc -p BNFC Latte.cf
	cd src && happy -gca BNFC/ParLatte.y
	cd src && alex -g BNFC/LexLatte.x
	touch src/BNFC/*

lib/runtime.bc: lib/runtime.ll
	llvm-as -o lib/runtime.bc lib/runtime.ll

lib/runtime.ll: lib/runtime.c
	clang -S -emit-llvm lib/runtime.c -o lib/runtime.ll