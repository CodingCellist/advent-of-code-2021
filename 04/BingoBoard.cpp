#include "BingoBoard.hpp"

using namespace bingoboard;

BingoBoard::BingoBoard(std::array<std::pair<int, bool>, BingoBoard::ARR_SIZE>* values) {
  this->values = *values;
}

bool BingoBoard::isThisSet(int row, int col) {
  return values[row * BingoBoard::BOARD_SIZE + col].second;
}

// check if all values in the given row have been set
bool BingoBoard::checkRow(int row) {
  for (int col = 0; col < BingoBoard::BOARD_SIZE; col++) {
    // if any of the values are false, stop
    if (!isThisSet(row, col)) return false;
  }
  // otherwise, all good
  return true;
}

// check if all values in the given column have been set
bool BingoBoard::checkCol(int col) {
  for (int row = 0; row < BingoBoard::BOARD_SIZE; row++) {
    // if any of the values are false, stop
    if (!isThisSet(row, col)) return false;
  }
  // otherwise, all good
  return true;
}

// call the number, setting its pair to true if the value was found
void BingoBoard::call(int num) {
  std::pair<int, bool> thePair(num, false);   // the pair to search for
  auto findRes = std::find(values.begin(), values.end(), thePair);
  // if we found the value, calculate its postition in the array and set the
  // pair to true
  if (findRes != values.end()) {
    int pos = std::distance(values.begin(), std::find(values.begin(), values.end(), thePair));
    values[pos].second = true;
  }
  // otherwise, we do nothing
}

// check whether the board has won
bool BingoBoard::hasWon() {
  for (int i = 0; i < BingoBoard::BOARD_SIZE; i++) {
    // if either a row or a column is completed, stop: the board has won
    if (checkRow(i) || checkCol(i)) return true;
  }
  return false;
}

// calculate the score of the board
int BingoBoard::calcScore(int winningNum) {
  int sum = 0;
  // calculate the sum of the unmarked values
  for (std::pair<int, bool> &val : values) {
    if (!val.second) {
      sum += val.first;
    }
  }
  return winningNum * sum;
}

