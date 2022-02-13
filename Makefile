all: play solveHard solve evaluateHard evaluate

play: play.hs Common.hs
	ghc -O2 -fllvm play.hs

solveHard: solveHard.hs Common.hs Solver/Common.hs Solver/HardOptimal.hs Solver/Probability.hs
	ghc -O2 -fllvm solveHard.hs

solve: solve.hs Common.hs Solver/Common.hs Solver/Optimal.hs
	ghc -O2 -fllvm solve.hs

evaluateHard: evaluateHard.hs Common.hs Solver/Common.hs Solver/HardOptimal.hs Solver/Probability.hs
	ghc -O2 -fllvm evaluateHard.hs

evaluate: evaluate.hs Common.hs Solver/Common.hs Solver/Optimal.hs
	ghc -O2 -fllvm evaluate.hs


.PHONY: clean
clean:
	rm -f ./*.hi
	rm -f ./*.o
	rm -f ./Solver/*.hi
	rm -f ./Solver/*.o
	rm -f play
	rm -f solve
	rm -f evaluate
	rm -f optimalFirstGuess
