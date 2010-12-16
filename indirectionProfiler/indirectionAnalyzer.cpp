#include <iostream>
#include <string>
#include <map>

using namespace std;

typedef map<void *, map<void *, unsigned int> > IndirectBranchMap;

void LoadSummary(istream &in) {
  IndirectBranchMap targetCounts;
  map<void*, unsigned int> sourceCounts;
  map<void*, string> sourceCategory;
  while (in) {
    string label;
    in >> label;
    if (label == "IndirectBranch") {
      string category;
      void *src, *dest;
      unsigned int count;
      in >> category >> src >> dest >> count;
      targetCounts[src][dest] += count;
      sourceCounts[src] += count;
      sourceCategory[src] = category;
    }
  }
  
  {
    IndirectBranchMap::iterator it;
    for ( it=targetCounts.begin() ; it != targetCounts.end(); it++ ) {
      void *source = (*it).first;
      map<void *, unsigned int> &targets = (*it).second;
      double countSum = 0.0;
      double weightSum = 0.0;
      {
	map<void *, unsigned int>::iterator it;
	for (it = targets.begin() ; it != targets.end() ; it++) {
	  void *target = (*it).first;
	  unsigned int count = (*it).second;
	  countSum += (double)count*(double)count;
	  weightSum += (double)count;
	}
      }
      cout << ""
	   << sourceCategory[source]
	   << "\t" << source
	   << "\t" << sourceCounts[source]
	   << "\t" << targets.size()
	   << "\t" << 100.0 * (double)sourceCounts[source] / (double)targets.size()
	   << "\t" << 100.0 * (double)countSum / (double)weightSum
	   << "\t" << 100.0 * (double)countSum / (double)weightSum / (double)targets.size()
	   << endl;
      if (0) {
	map<void *, unsigned int>::iterator it;
	for (it = targets.begin() ; it != targets.end() ; it++) {
	  void *target = (*it).first;
	  unsigned int count = (*it).second;
	  cout << source
	       << "\t" << target
	       << "\t" << count
	       << "\t" << sourceCounts[source]
	       << "\t" << 100.0 * (double)count / (double)sourceCounts[source]
	       << endl;
	}
      }
    }
  }
}

int main() {
  LoadSummary(cin);

}
