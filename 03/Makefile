CXX=g++
CXXTRGT=03cpp

all: idris cpp

run: runidr runcpp

runidr: idris
	idris2 -p contrib AoC03.idr --exec main < input

runcpp: cpp
	./$(CXXTRGT) < input

idris: AoC03.ipkg AoC03.idr 
	idris2 --build $<

cpp: 03.o
	$(CXX) -o $(CXXTRGT) $^

%.o: %.cpp
	$(CXX) -c $^

clean:
	$(RM) *.o *.idr~ $(CXXTRGT) 
	$(RM) -r build/

