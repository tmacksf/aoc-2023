#include <vector>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_set>

using namespace std;

int assembleNumber(vector<string> v, int i, int j, 
    unordered_set<int> &visited) {

  int vsize = v[0].size();
  if (i < 0 || j < 0 || i >= v.size() || j >= vsize)
    return 0;

  if (!isdigit(v.at(i).at(j)))
    return 0;

  int setindex = vsize * i + j;
  if (visited.contains(setindex))
    return 0;
  visited.insert(setindex);
  string num = "";
  num += v.at(i).at(j);

  // look left 
  int j1 = j - 1;
  while (j1 >= 0 && isdigit(v.at(i).at(j1))) {
    num.insert(0, 1, v.at(i).at(j1));
    setindex = vsize * i + j1;
    visited.insert(setindex);
    j1--;
  }

  // look right 
  j1 = j + 1;
  while (j1 < vsize && isdigit(v.at(i).at(j1))) {
    num += v.at(i).at(j1);
    setindex = vsize * i + j1;
    visited.insert(setindex);
    j1++;
  }

  return stoi(num);
}

int solve1(vector<string> v) {
  unordered_set<int> visited;
  pair<int, int> directions[] = {
    {-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, 
    {0, 1}, {1, -1}, {1, 0}, {1, 1}
  };
  int total = 0;
  for (int i = 0; i < v.size(); i++) {
    for (int j = 0; j < v[i].size(); j++) {
      char c = v.at(i).at(j);
      if (c == '.' || isdigit(c))
        continue;
      for (auto p : directions) {
        total += assembleNumber(v, i + p.first, j + p.second, visited);
      }
    }
  }
  return total;
}

int solve2(vector<string> v) {
  unordered_set<int> visited;
  pair<int, int> directions[] = {
    {-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, 
    {0, 1}, {1, -1}, {1, 0}, {1, 1}
  };
  int total = 0;
  for (int i = 0; i < v.size(); i++) {
    for (int j = 0; j < v[i].size(); j++) {
      char c = v.at(i).at(j);
      if (c != '*')
        continue;

      int index = 0;
      int vals[8] = {0};
      for (auto p : directions) {
        int temp = assembleNumber(v, i + p.first, j + p.second, visited);
        vals[index] = temp;
        index++;
      }

      int temp = 1;
      int count = 0;
      for (int k = 0; k < 8; k++) { 
        if (!vals[k])
          continue;
        temp *= vals[k];
        count++;
      }
      if (count > 1) {
        total += temp;
      }
    }
  }
  return total;
}

int main () {
  ifstream file("input.txt");
 
  vector<string> v;
  string str;
 
  while (file >> str) {
    v.push_back(str);
  }

  copy(v.begin(), v.end(), ostream_iterator<string>(cout, "\n"));
  cout << solve2(v) << endl;
 
  return 0;
}
