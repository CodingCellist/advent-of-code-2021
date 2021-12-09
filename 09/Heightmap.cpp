#include "Heightmap.hpp"

Heightmap::Heightmap(vector<int> &rows, int nRows, int nCols) {
  this->rows = rows;
  this->nRows = nRows;
  this->nCols = nCols;
}

// Given an input stream to read from, attempt to parse its contents as a
// `Heightmap`.
Heightmap Heightmap::parseHeightmap(istream &is) {
  vector<int> rows;
  // a parsed row
  vector<int> row;
  char c;
  int nCols = -1;
  int nRows = 0;
  while(is.get(c)) {
    // if we're at the end of the line, add the row to the rows
    if (c == '\n') {
      // check that the theDimensions match
      if (rows.size() > 0 && row.size() != nCols) {
        cerr << "parseHeightmap: two rows had mismatched sizes!\n";
        exit(1);
      }
      else {
        // the first row determines the the dimension for the rest of the parsing
        nCols = row.size();
      }

      rows.insert(rows.end(), row.begin(), row.end());
      nRows++;
      row.clear();
    }
    else {
      int i = c - '0';    // ascii single-digit number conversion
      if (i < 0 || 9 < i) {
        cerr << "parseHeightmap: i was not in [0, 9]\n";
        exit(1);
      }
      row.push_back(i);
    }
  }
  return Heightmap(rows, nRows, nCols);
}

// helper for 2D indexing of the rows
int Heightmap::getPos(int row, int col) {
  return rows.at(row * nCols + col);
}

// check if each of the corners are low points
vector<int> Heightmap::checkCorners() {
  vector<int> cornerLowPoints;
  // top-left
  int tl = getPos(0, 0);
  if (tl < getPos(1, 0) && tl < getPos(0, 1)) {
    cornerLowPoints.push_back(tl);
  }
  // top-right
  int tr = getPos(0, nCols - 1);
  if (tr < getPos(0, nCols - 2) && tr < getPos(1, nCols - 1)) {
    cornerLowPoints.push_back(tr);
  }
  // bottom-left
  int bl = getPos(nRows - 1, 0);
  if (bl < getPos(nRows - 2, 0) && bl < getPos(nRows - 1, 1)) {
    cornerLowPoints.push_back(bl);
  }
  // bottom-right
  int br = getPos(nRows - 1, nCols - 1);
  if (br < getPos(nRows - 1, nCols - 2) && br < getPos(nRows - 2, nCols - 1)) {
    cornerLowPoints.push_back(br);
  }
  return cornerLowPoints;
}

// check if the top row contains low points
vector<int> Heightmap::checkTopRow() {
  const int topRow = 0;
  vector<int> trLowPoints;
  // avoid corners
  for (int c = 1; c < nCols - 1; c++) {
    int cand = getPos(topRow, c);
    if (cand < getPos(topRow, c - 1) &&   // left
        cand < getPos(topRow + 1, c) &&   // below
        cand < getPos(topRow, c + 1)) {   // right
      trLowPoints.push_back(cand);
    }
  }
  return trLowPoints;
}

// check if the bottom row contains low points
vector<int> Heightmap::checkBotRow() {
  const int botRow = nRows - 1;
  vector<int> brLowPoints;
  // avoid corners
  for (int c = 1; c < nCols - 1; c++) {
    int cand = getPos(botRow, c);
    if (cand < getPos(botRow    , c - 1) &&   // left
        cand < getPos(botRow - 1, c    ) &&   // above
        cand < getPos(botRow    , c + 1)) {   // right
      brLowPoints.push_back(cand);
    }
  }
  return brLowPoints;
}

// check if the leftmost column contains low points
vector<int> Heightmap::checkLeftCol() {
  const int leftCol = 0;
  vector<int> lcLowPoints;
  // avoid corners
  for (int r = 1; r < nRows - 1; r++) {
    int cand = getPos(r, leftCol);
    if (cand < getPos(r - 1, leftCol    ) &&   // above
        cand < getPos(r    , leftCol + 1) &&   // right
        cand < getPos(r + 1, leftCol    )) {   // below
      lcLowPoints.push_back(cand);
    }
  }
  return lcLowPoints;
}

// check if the rightmost column contains low points
vector<int> Heightmap::checkRightCol() {
  const int rightCol = nCols - 1;
  vector<int> rcLowPoints;
  // avoid corners
  for (int r = 1; r < nRows - 1; r++) {
    int cand = getPos(r, rightCol);
    if (cand < getPos(r - 1, rightCol    ) &&   // above
        cand < getPos(r    , rightCol - 1) &&   // left
        cand < getPos(r + 1, rightCol    )) {   // below
      rcLowPoints.push_back(cand);
    }
  }
  return rcLowPoints;
}

// check if the center of the `Heightmap` contains low points
vector<int> Heightmap::checkCenter() {
  vector<int> centerLowPoints;
  // avoiding edges
  for (int r = 1; r < nRows - 1; r++) {
    for (int c = 1; c < nCols - 1; c++) {
      int cand = getPos(r, c);
      if (cand < getPos(r - 1, c    ) &&    // left
          cand < getPos(r + 1, c    ) &&    // right
          cand < getPos(r    , c - 1) &&    // above
          cand < getPos(r    , c + 1)) {    // below
        centerLowPoints.push_back(cand);
      }
    }
  }
  return centerLowPoints;
}

// Retrieve the low points of the `Heightmap`
vector<int> Heightmap::getLowPoints() {
  // if we haven't already calculated the low points, do that now
  if (lowPoints.size() == 0 && rows.size() != 0) {
    vector<int> corners  = checkCorners();
    vector<int> topRow   = checkTopRow();
    vector<int> botRow   = checkBotRow();
    vector<int> leftCol  = checkLeftCol();
    vector<int> rightCol = checkRightCol();
    vector<int> center   = checkCenter();
    // add all the low points to the `Heightmap`'s vector of low points
    lowPoints.insert(lowPoints.end(), corners.begin(), corners.end());
    lowPoints.insert(lowPoints.end(), topRow.begin(), topRow.end());
    lowPoints.insert(lowPoints.end(), botRow.begin(), botRow.end());
    lowPoints.insert(lowPoints.end(), leftCol.begin(), leftCol.end());
    lowPoints.insert(lowPoints.end(), rightCol.begin(), rightCol.end());
    lowPoints.insert(lowPoints.end(), center.begin(), center.end());
  }
  return lowPoints;
}

