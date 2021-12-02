#include <iostream>

using namespace std;

int main() {
  int n1, n2, n3, n4;
  int w1, w2;
  int count = 0;

  // read the first values
  cin >> n1 >> n2 >> n3;

  while (cin >> n4) {
    w1 = n1 + n2 + n3;
    w2 = n2 + n3 + n4;
    // add 1 to count if there was an increase
    count += w2 > w1 ? 1 : 0;
    // move to the next window
    n1 = n2;
    n2 = n3;
    n3 = n4;
  }

  cout << "#increases: " << count << "\n";
}

