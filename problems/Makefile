# 
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable files
#   make clean   to remove all intermediate and temporary files
#   

# Files that need to be generated from other files
DEPEND += ToyTokens.hs ToyGrammar.hs ToyEval.hs

# When "make" is invoked with no arguments, we build an executable 
#  after building everything that it depends on
all: $(DEPEND) Toyi Toy

# Build an executable for Toy interpreter
Toy: $(DEPEND) Toy.hs
	ghc Toy.hs

# Build an executable for interactive mode
Toyi: $(DEPEND) Toyi.hs
	ghc Toyi.hs

# Generate ML files from a parser definition file
ToyGrammar.hs : ToyGrammar.y
	@rm -f ToyGrammar.hs
	happy ToyGrammar.y
	@chmod -w ToyGrammar.hs

# Generate ML files from a lexer definition file
ToyTokens.hs : ToyTokens.x
	@rm -f ToyTokens.hs
	alex ToyTokens.x
	@chmod -w ToyTokens.hs

# Clean up the directory
clean::
	rm -rf ToyTokens.hs ToyGrammar.hs *.hi *.o *.info

