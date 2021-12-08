#include <iostream> 
#include <string>
#include <sstream>
#include <numeric>  // std::accumulate
#include <chrono>

using namespace std;

const int MAX_TIMER = 8;
const int PART1_NSTEPS = 80;
const int PART2_NSTEPS = 256;

// simulate the specified number of steps using the given initial setup of
// lanternfish timers
void simulate(unsigned long (&init)[MAX_TIMER + 1], int nSteps) {
  for (int s = 0; s < nSteps; s++) {
    unsigned long newFish = init[0];
    // the fish which reset are the 0 timers and the previous new generation
    unsigned long oldFish = init[MAX_TIMER - 1] + init[0];
    for (int i = 0; i < MAX_TIMER; i++) {
      init[i] = init[i + 1];
    }
    // laternfish mature after 2 days
    init[MAX_TIMER - 2] = oldFish;
    init[MAX_TIMER] = newFish;
  }
}

// parse the digits in the string into the given array
void parseInit(string &str, unsigned long (&init)[MAX_TIMER + 1]) {
  auto strStream = stringstream(str);
  string s;
  while (getline(strStream, s, ',')) {
    switch (stoul(s)) {
      case 0:
        init[0]++;
        break;
      case 1:
        init[1]++;
        break;
      case 2:
        init[2]++;
        break;
      case 3:
        init[3]++;
        break;
      case 4:
        init[4]++;
        break;
      case 5:
        init[5]++;
        break;
      case 6:
        init[6]++;
        break;
      case 7:
        init[7]++;
        break;
      case 8:
        init[8]++;
        break;
      default:
        cerr << "Whelp...\n";
        exit(1);
    }
  }
}

int main() {
  string str;
  getline(cin, str);

  // starting arrays for parts 1 and 2
  unsigned long init1[MAX_TIMER + 1] = { 0 };
  unsigned long init2[MAX_TIMER + 1] = { 0 };

  // parse the values into the arrays for parts 1 and 2
  parseInit(str, init1);
  parseInit(str, init2);

  using chrono::time_point;
  using chrono::nanoseconds;
  using chrono::duration_cast;
  using chrono::high_resolution_clock;
  // solve and time part 1 (just for fun and comparison with Idris)
  unsigned long part1 = 0;
  time_point t1 = high_resolution_clock::now();
  simulate(init1, PART1_NSTEPS);
  part1 = accumulate(init1, init1 + (MAX_TIMER + 1), part1);
  time_point t2 = high_resolution_clock::now();
  nanoseconds time = duration_cast<nanoseconds>(t2 - t1);

  // solve part 2
  unsigned long part2 = 0;
  simulate(init2, PART2_NSTEPS);
  part2 = accumulate(init2, init2 + (MAX_TIMER + 1), part2);

  cout << "Part 1: " << part1 << " (took: " << time.count() << "ns)" << "\n";
  cout << "Part 2: " << part2 << "\n";

  return 0;
}

