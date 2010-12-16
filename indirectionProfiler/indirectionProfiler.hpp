#ifndef INDIRECTIONPROFILER_H
#define INDIRECTIONPROFILER_H

#include <vector>
#include <string>
#include <iostream>
#include <sstream>
#include <iomanip>

static string SQLAddressFormat(void *addr) {
  stringstream ss;
  ss << setfill('0') << setw(sizeof(addr)*2) << hex << (uint64_t)addr;
  return ss.str();
}

class Branch {
private:
  xed_category_enum_t category;
  void * source;
public:
  Branch(xed_category_enum_t category, void *source) : category(category), source(source) {}
  virtual ~Branch() {}
  void * getSource() {return source;}
  xed_category_enum_t getCategory() { return category; }
  virtual void dump(ostream &out) = 0;
  virtual bool empty() = 0;

  static bool compare(Branch *a, Branch *b) {
    return (a->source < b->source);
  }
};

class DirectBranch : public Branch {
private:
  void * target;
  unsigned int count;
public:
  DirectBranch(xed_category_enum_t category, void *source, void *t):
    Branch(category, source), target(t), count(0) {}
  void * getTarget() {return target;}
  unsigned int getCount() {return count;}
  void incrCount() {count++;}
  void merge(DirectBranch &b) {
    assert(b.getSource() == getSource() &&
	   b.getTarget() == getTarget());
    count += b.getCount();
  }
  virtual void dump(ostream &out) {
    out << "DirectBranch" 
	<< "\t" << xed_category_enum_t2str(getCategory())
	<< "\t" << getSource()
	<< "\t" << getTarget()
	<< "\t" << getCount() 
	<< endl;
  }
  virtual void dumpSQL(ostream &out) {
    out << "INSERT INTO CallSites (type, category, addr) VALUES(" 
	<< "'" << "DirectBranch" << "'"
	<< ", '" << xed_category_enum_t2str(getCategory()) << "'"
	<< ", " << "'" << SQLAddressFormat(getSource()) << "'"
	<< ");" << endl;
    out << "INSERT INTO CallTargets (callsite, target, count) SELECT rowid, "
	<< "'" << SQLAddressFormat(getTarget()) << "'"
	<< ", "
	<< getCount()
	<< " FROM CallSites WHERE CallSites.addr="
	<< "'" << SQLAddressFormat(getSource()) << "'"
	  << ";"
	<< endl;
  }
  virtual bool empty() { return count == 0; }
};

class IndirectBranch : public Branch {
private:
  map<void*, unsigned int> targets;
public:
  IndirectBranch (xed_category_enum_t category, void *source) :
    Branch(category, source) {}
  void incrTarget(void *target) { targets[target]++; }
  void merge(IndirectBranch &b) {
    assert(b.getSource() == getSource());
    for (map<void*, unsigned int>::iterator it=b.targets.begin() ;
	 it != b.targets.end();
	 it++) {
      void *target = (*it).first;
      unsigned int count = (*it).second;
      targets[target] += count;
    }
  }

  virtual void dump(ostream &out) {
    for (map<void*, unsigned int>::iterator it=targets.begin() ;
	 it != targets.end();
	 it++) {
      void *target = (*it).first;
      unsigned int count = (*it).second;
      out << "IndirectBranch"
	  << "\t" << xed_category_enum_t2str(getCategory())
	  << "\t" << getSource()
	  << "\t" << target
	  << "\t" << count
	  << endl;
    }
  }
  virtual void dumpSQL(ostream &out) {
    out << "INSERT INTO CallSites (type, category, addr) VALUES(" 
	<< "'" << "IndirectBranch" << "'"
	<< ", '" << xed_category_enum_t2str(getCategory()) << "'"
	<< ", " << "'" << SQLAddressFormat(getSource()) << "'"
	<< ");" << endl;
    for (map<void*, unsigned int>::iterator it=targets.begin() ;
	 it != targets.end();
	 it++) {
      void *target = (*it).first;
      unsigned int count = (*it).second;
      out << "INSERT INTO CallTargets (callsite, target, count) SELECT rowid, "
	  << "'" << SQLAddressFormat(target) << "'"
	  << ", "
	  << count
	  << " FROM CallSites WHERE CallSites.addr="
	  << "'" << SQLAddressFormat(getSource()) << "'"
	  << ";"
	  << endl;
    }
  }
  virtual bool empty() { return targets.size() == 0; }
};


#endif /* INDIRECTIONPROFILER_H */
