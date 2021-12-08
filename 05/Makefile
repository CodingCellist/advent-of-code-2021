CXX=g++
CXXTRGT=05cpp

all: idris cpp

run: runidr runcpp

runidr: idris
	idris2 -p contrib AoC05.idr --exec main < input

runcpp: cpp
	./$(CXXTRGT) < input

idris: AoC05.ipkg AoC05.idr 
	idris2 --build $<

cpp: 05.o
	$(CXX) -o $(CXXTRGT) $^

%.o: %.cpp
	$(CXX) -g -c $^

clean:
	$(RM) *.o *.idr~ $(CXXTRGT) 
	$(RM) -r build/

