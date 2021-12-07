CXX=g++
CXXTRGT=04cpp

all: idris cpp

run: runidr runcpp

runidr: idris
	idris2 -p contrib AoC04.idr --exec main < input

runcpp: cpp
	./$(CXXTRGT) < input

idris: AoC04.ipkg AoC04.idr 
	idris2 --build $<

cpp: 04.o BingoBoard.o
	$(CXX) -o $(CXXTRGT) $^

%.o: %.cpp
	$(CXX) -g -c $^

clean:
	$(RM) *.o *.idr~ $(CXXTRGT) 
	$(RM) -r build/

