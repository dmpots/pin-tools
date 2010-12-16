int baz() { return 0; }
int bar() { return baz(); }
int foo() { return bar(); }


int main() {
  return foo();
}
