#include "vectorfunctions.h"
#include <algorithm>
#include <numeric>

using namespace std;

void backwards(vector<int>& vec){
    reverse(vec.begin(), vec.end());
}

vector<int> everyOther(const vector<int>& vec){
    vector<int> ret;
    for (int i {0}; i < vec.size(); i+=2) {
        ret.push_back(vec[i]);
    }
    return ret;
}

int smallest(const vector<int>& vec){
    return *min_element(vec.begin(), vec.end());
}

int sum(const vector<int>& vec){
    return accumulate(vec.begin(), vec.end(), 0);
}

int veryOdd(const vector<int>& suchVector){
    int odds {0};
    for (int i {1}; i < suchVector.size(); i += 2) {
        odds += suchVector[i] & 1; // branchless, yoooooo
    }
    return odds;
}
