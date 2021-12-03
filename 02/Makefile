CXX=g++
CXXTRGT=02cpp

all: idris cpp

run: idris cpp
	idris2 AoC02.idr --exec main < input
	./$(CXXTRGT) < input

idris: AoC02.ipkg AoC02.idr 
	idris2 --build $<

cpp: 02.o
	$(CXX) -o $(CXXTRGT) $^

%.o: %.cpp
	$(CXX) -c $^

clean:
	$(RM) *.o *.idr~ $(CXXTRGT) 
	$(RM) -r build/

