#include <utility>
#include <array>
#include <algorithm>  // std::find

namespace bingoboard {
  class BingoBoard {
    public:
      // number of rows+cols on the board
      static const int BOARD_SIZE = 5;
      // the size of the "2D" array used to store the board
      static const int ARR_SIZE = BOARD_SIZE * BOARD_SIZE;

    private:
      std::array<std::pair<int, bool>, ARR_SIZE> values;
      bool isThisSet(int row, int col);
      bool checkRow(int row);
      bool checkCol(int col);

    public:
      BingoBoard(std::array<std::pair<int, bool>, ARR_SIZE>* values);

      void call(int num);
      bool hasWon();
      int calcScore(int winningNum);
  };
}

