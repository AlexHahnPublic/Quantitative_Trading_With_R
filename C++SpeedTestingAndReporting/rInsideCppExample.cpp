#include <RInside.h>

int main(int argc, char *argv[]) {
  // create an embedded R instance
  Rinside R(argc, argv);

  // assign a char* (string) to "txt"
  R["txt"] = "Hello, world!\n";

  // eval the init string, ignoring any returns
  R.parseEvalQ("cat(txt)");

  exit(0);
}
