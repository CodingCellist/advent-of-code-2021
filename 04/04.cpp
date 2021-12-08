#include <iostream> 
#include <string>
#include <sstream>
#include <list>

#include "BingoBoard.hpp"

using namespace bingoboard;

BingoBoard buildBoard() {
  std::array<std::pair<int, bool>, BingoBoard::ARR_SIZE> values;
  int v;
  for (int r = 0; r < BingoBoard::BOARD_SIZE; r++) {
    for (int c = 0; c < BingoBoard::BOARD_SIZE; c++) {
      std::cin >> v;
      values[r * BingoBoard::BOARD_SIZE + c] = std::make_pair(v, false);
    }
    // ignore the rest of the line (>> doesn't consume the newline)
    std::cin.ignore();
  }
  return BingoBoard(&values);
}

// call the given number on every board in the given list of boards
void callOnAll(std::list<BingoBoard> &boards, int number) {
  for_each(boards.begin(), boards.end(),
      [number](BingoBoard &board) { board.call(number); }
      );
}

// find the first board which will win given the numbers drawn and calculate
// its score
int solvePart1(std::list<int> &drawnNums, std::list<BingoBoard> &boards) {
  // iterator for all the numbers drawn
  auto numIter = drawnNums.begin();
  // the value that was just called
  int justCalled = -1;

  // we need at least BOARD_SIZE numbers called for a potential winner
  for (int i = 0; i < BingoBoard::BOARD_SIZE; i++) {
    callOnAll(boards, *numIter);
    justCalled = *numIter;
    // move the iterator to the next drawn number
    numIter++;
  }

  // if we have a winning board, return it, otherwise call another value
  for (/*already have numIter*/; numIter != drawnNums.end(); numIter++) {
    auto winner =
      std::find_if(boards.begin(), boards.end(), [](BingoBoard &board) { return board.hasWon(); });

    if (winner != boards.end()) {
      int score = winner->calcScore(justCalled);
      return score;
    }

    callOnAll(boards, *numIter);
    justCalled = *numIter;
  }
  return -1;
}

// find the last board which would win and calculate its score
int solvePart2(std::list<int> &drawnNums, std::list<BingoBoard> &boards) {
  // iterator for all the numbers drawn
  auto numIter = drawnNums.begin();
  // the value that was just called
  int justCalled = -1;

  // we need at least BOARD_SIZE numbers called for a potential winner
  for (int i = 0; i < BingoBoard::BOARD_SIZE; i++) {
    callOnAll(boards, *numIter);
    justCalled = *numIter;
    // move the iterator to the next drawn number
    numIter++;
  }

  // call a number, remove the boards which have won, check if we only have 1
  // left and if it has won; keep calling otherwise
  for (/*already have numIter*/; numIter != drawnNums.end(); numIter++) {

    // if there is more than 1 board left, filter the boards which have won
    if (boards.size() > 1) {
      boards.remove_if([](BingoBoard &board) { return board.hasWon(); });
    }

    // if we have one board left and it has won, calculate its score
    if (boards.size() == 1 && boards.begin()->hasWon()) {
      return boards.begin()->calcScore(justCalled);
    }

    // otherwise, call a number and keep going
    callOnAll(boards, *numIter);
    justCalled = *numIter;
  }

  std::cout << "SIZE : " << boards.size() << "\n";
  std::cout << "WON : " << boards.begin()->hasWon() << "\n";

  return -1;
}

int main() {
  std::list<int> drawnNums;

  // parse the called numbers
  std::string rawNums;
  std::getline(std::cin, rawNums);
  std::string entry;
  std::stringstream rawNumStream(rawNums);
  while (std::getline(rawNumStream, entry, ',')) {
    drawnNums.push_back(stoi(entry));
  }

  // create the boards
  std::list<BingoBoard> boards;
  while(std::getline(std::cin, entry)) {
    // if we encounter a blank line, then the next bit must be a board,
    // so parse it
    if (entry == "") {
      BingoBoard newBoard = buildBoard();
      boards.push_back(newBoard);
    }
  }

  // make a copy for part2
  std::list<BingoBoard> boards2(boards);

  // SOLVE PART 1!
  int answer = solvePart1(drawnNums, boards);
  if (answer == -1) {
    // oh no
    std::cerr << "Argh!\n";
    return 1;
  }
  else {
    std::cout << "Part 1: " << answer << "\n";
  }

  // SOLVE PART 2!
  answer = solvePart2(drawnNums, boards2);
  if (answer == -1) {
    std::cerr << "Oh no!\n";
    return 1;
  }
  else {
    std::cout << "Part 2: " << answer << "\n";
  }

  return 0;
}

