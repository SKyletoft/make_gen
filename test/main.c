#include <stdio.h>

int foo(void);
int bar(void);

int main() {
	int a = foo();
	int b = bar();
	printf("%d %d\n", a, b);
}
