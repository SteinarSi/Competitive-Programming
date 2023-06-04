#include <string>
#include <vector>
#include <map>

using namespace std;
using i64 = uint64_t;

class ID {
    map<string,i64> name2id;
    vector<string> id2name;

public:
    void add(const string &s){
        get_id(s);
    }

    i64 get_id(const string &s){
        auto [it, placed] = name2id.emplace(s, name2id.size());
        if (placed) id2name.push_back(s);
        return it->second;
    }
};
