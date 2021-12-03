#include <iostream> 
#include <string>
#include <cmath>

using namespace std;

const unsigned int NBITS = 12;

// iterate through the entry, accumulating the counts of 1s and 0s in the given
// arrays
void accCount(int (&is)[NBITS], int (&os)[NBITS], string entry) {
  for (int i = 0; i < NBITS; i++) {
    if (entry[i] == '0') {
      os[i] += 1;
    }
    else {
      is[i] += 1;
    }
  }
}

// calculate the gamma value as per the puzzle: if there were more 1s, add that
// corresponding power of 2, if there were more 0s, add nothing
unsigned int calcGamma(int (&is)[NBITS], int (&os)[NBITS]) {
  unsigned int gamma = 0;
  for (int i = 0; i < NBITS; i++) {
    gamma += is[i] > os[i] ? (unsigned int) pow(2, NBITS - (i + 1)) : 0;
  }
  return gamma;
}

int main() {
  string entry;
  // each element corresponds to the #bits at that position
  int ones[NBITS]  = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  int zeroes[NBITS] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

  while (cin >> entry) {
    accCount(ones, zeroes, entry);
  }

  unsigned int gamma = calcGamma(ones, zeroes);
  unsigned int epsilon = (gamma ^ ((1u << NBITS) - 1));   // I love C/C++  ^^

  cout << "Part 1: " << gamma * epsilon << "\n";
}

