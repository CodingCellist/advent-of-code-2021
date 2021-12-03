#include <iostream> 
#include <string>
#include <cmath>
#include <list>
#include <algorithm>    // std::copy_if

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

string findOxygenRating(list<string> &entries, int (&is)[NBITS], int (&os)[NBITS]) {
  list<string> filtered(entries);    // list to store the matching values in

  for (int pos = 0; pos < NBITS; pos++) {
    char mostCommon = is[pos] >= os[pos] ? '1' : '0';
    // remove entries that don't have the most common digit at that position
    filtered.remove_if(
        [pos, mostCommon](string entry) { return entry[pos] != mostCommon; }
        );
    // if we have only one entry, we're done
    if (filtered.size() == 1) return filtered.front();
  }
  return "";
}

string findCO2Rating(list<string> &entries, int (&is)[NBITS], int (&os)[NBITS]) {
  list<string> filtered(entries);    // list to store the matching values in

  for (int pos = 0; pos < NBITS; pos++) {
    char leastCommon = is[pos] < os[pos] ? '1' : '0';
    // remove entries that don't have the most common digit at that position
    filtered.remove_if(
        [pos, leastCommon](string entry) { return entry[pos] != leastCommon; }
        );
    // if we have only one entry, we're done
    if (filtered.size() == 1) return filtered.front();
  }
  return "";
}

int main() {
  string entry;
  // each element corresponds to the #bits at that position
  int ones[NBITS]  = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  int zeroes[NBITS] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

  list<string> entries;

  while (cin >> entry) {
    accCount(ones, zeroes, entry);
    entries.push_back(entry);
  }

  // PART 1
  unsigned int gamma = calcGamma(ones, zeroes);
  unsigned int epsilon = (gamma ^ ((1u << NBITS) - 1));   // I love C/C++  ^^

  cout << "Part 1: " << gamma * epsilon << "\n";

  // PART 2
  string ogr = findOxygenRating(entries, ones, zeroes);
  string csr = findCO2Rating(entries, ones, zeroes);
  if (ogr == "" || csr == "") {
    cout << "ERROR: Couldn't find at least one rating.\n";
    return 1;
  }
  unsigned long lifeSupportRating = stoul(ogr, nullptr, 2) * stoul(csr, nullptr, 2);
  cout << "Part 2: " << lifeSupportRating << "\n";
}

