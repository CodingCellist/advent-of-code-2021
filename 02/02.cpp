#include <iostream> 
#include <string>

using namespace std;

int main() {
  string cmd;
  int x;

  int depth = 0, hPos = 0, aim = 0, depth2 = 0, hPos2 = 0;

  while (cin >> cmd >> x) {
    if (cmd == "up") {
      // part 1
      depth -= x;
      // part 2
      aim -= x;
    }
    else if (cmd == "down") {
      // part 1
      depth += x;
      // part 2
      aim += x;
    }
    else if (cmd == "forward") {
      // part 1
      hPos += x;
      // part 2
      hPos2 += x;
      depth2 += aim * x;
    }
    else {
      cout << "Something's wrong, I can feel it!\n";
      return 1;
    }
  }
  cout << "Part 1: " << (depth  * hPos ) << "\n";
  cout << "Part 2: " << (depth2 * hPos2) << "\n";
}

