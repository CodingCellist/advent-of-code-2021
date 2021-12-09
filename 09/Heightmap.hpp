#include <iostream>
#include <vector>

using namespace std;

class Heightmap {
  private:
    vector<int> rows;
    vector<int> lowPoints;
    int nRows;
    int nCols;

    // helpers for `getLowPoints`
    int getPos(int row, int col);
    vector<int> checkCorners();
    vector<int> checkTopRow();
    vector<int> checkBotRow();
    vector<int> checkLeftCol();
    vector<int> checkRightCol();
    vector<int> checkCenter();

  public:
    Heightmap(vector<int> &rows, int nRows, int nCols);
    static Heightmap parseHeightmap(istream &is);

    vector<int> getLowPoints();
};

