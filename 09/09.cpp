#include <numeric>
#include "Heightmap.hpp"

using namespace std;

int main() {
  Heightmap hm = Heightmap::parseHeightmap(cin);
  vector<int> lowPoints = hm.getLowPoints();
  // Part 1
  // risk level is low point + 1, or sum(low points) + (1 per low point)
  int lpRisk = lowPoints.size();
  lpRisk = accumulate(lowPoints.begin(), lowPoints.end(), lpRisk);
  cout << "Part 1: " << lpRisk << "\n";
  return 0;
}

