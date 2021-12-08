CXX=g++
CXXTRGT=06cpp

all: idris cpp

run: runidr runcpp

runidr: idris
	idris2 -p contrib AoC06.idr --exec main < input

runcpp: cpp
	./$(CXXTRGT) < input

idris: AoC06.ipkg AoC06.idr 
	idris2 --build $<

cpp: 06.o
	$(CXX) -o $(CXXTRGT) $^

%.o: %.cpp
	$(CXX) -g -c $^

clean:
	$(RM) *.o *.idr~ $(CXXTRGT) 
	$(RM) -r build/

