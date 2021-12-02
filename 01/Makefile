CXX=g++
CXXTRGT=01cpp

all: idris cpp

run: idris cpp
	idris2 AoC01.idr --exec main < input
	./$(CXXTRGT) < input

idris: AoC01.ipkg AoC01.idr 
	idris2 --build $<

cpp: 01.o
	$(CXX) -o $(CXXTRGT) $^

%.o: %.cpp
	$(CXX) -c $^

clean:
	$(RM) *.o *.idr~ $(CXXTRGT) 
	$(RM) -r build/

