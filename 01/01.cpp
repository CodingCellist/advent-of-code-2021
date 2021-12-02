#include <iostream>

using namespace std;

int main() {
  int n1, n2;
  int count = 0;

  // read the first value
  cin >> n1;

  while (cin >> n2) {
    // add 1 to count if there was an increase
    count += n2 > n1 ? 1 : 0;
    // move to the next number
    n1 = n2;
  }

  cout << "#increases: " << count << "\n";
}

