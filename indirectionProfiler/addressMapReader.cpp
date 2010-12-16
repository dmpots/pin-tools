#include <iostream>
#include <fstream>
#include <map>

using namespace std;

class Interval {
private:
  string name;
  void *low;
  void *high;
public:
  Interval(string name, void *low, void *high) : name(name), low(low), high(high) {}
  void *getLow() { return low; }
  void *getHigh() { return high; }
  string getName() { return name; }

  void dump(ostream& out) {
    out << name
        << "\t" << low
        << "\t" << high
        << "\n";
  }
};

ostream& operator << (ostream& out, Interval& interval) {
  interval.dump(out);
  return out;
}

static map<void *, Interval*> images;
static map<void *, Interval*> routines;

string lookupRoutine(void* addr) {
  map<void*, Interval*>::iterator it = routines.lower_bound(addr);
  if (it == routines.end()) return "<NONE>";
  Interval * i = (*it).second;
  if (addr >= i->getLow() && addr < i->getHigh())
    return i->getName();
  return "<NONE>";
}

string lookupImage(void* addr) {
  map<void*, Interval*>::iterator it = images.lower_bound(addr);
  if (it == images.end()) return "<NONE>";
  Interval * i = (*it).second;
  if (addr >= i->getLow() && addr < i->getHigh())
    return i->getName();
  return "<NONE>";
}



void AddressMapReader(istream &in) {
  
  while (!in.eof()) {
    string label, name;
    void *low, *high;
    in >> label;
    if (!in.eof()) {
      in >> name >> low >> high;
      Interval *interval = new Interval(name, low, high);
      if (label == "Image")
        images[(void*)(((char*)high) - 1)] = interval;
      else if (label == "Routine")
        routines[(void*)(((char*)high) - 1)] = interval;
    }
  }
}

void SummaryReader(istream &in) {
  
  while (!in.eof()) {
    string label, type;
    void *src, *dest;
    int count;

    in >> label;
    if (!in.eof()) {
      in >> type >> src >> dest >> count;
      cout << label
           << "\t" << type
           << "\t" << src
           << "\t" << "'" << lookupImage(src) << "->" << lookupRoutine(src) << "'"
           << "\t" << dest
           << "\t" << "'" << lookupImage(dest) << "->" << lookupRoutine(dest) << "'"
           << "\t" << count
           << "\n";
    }
  }
}

int main(int argc, char *argv[]) {
  if (argc != 3) {
    cout << "usage:  " << argv[0] << " <AddressMap> <CallProfile>" << endl;
    return -1;
  }
  ifstream mapFile(argv[1]);
  ifstream summaryFile(argv[2]);
  AddressMapReader(mapFile);
  SummaryReader(summaryFile);
  return 0;
}
