CXX=g++
CXXTRGT=09cpp

all: idris cpp

run: runidr runcpp

runidr: idris
	idris2 -p contrib AoC09.idr --exec main < input

runcpp: cpp
	./$(CXXTRGT) < input

idris: AoC09.ipkg AoC09.idr 
	idris2 --build $<

cpp: 09.o Heightmap.o
	$(CXX) -o $(CXXTRGT) $^

%.o: %.cpp
	$(CXX) -g -c $^

clean:
	$(RM) *.o *.idr~ $(CXXTRGT) 
	$(RM) -r build/

